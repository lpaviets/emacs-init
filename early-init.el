;; -*- lexical-binding: t -*-

(set-frame-parameter nil 'fullscreen 'maximized)
(setq gc-cons-threshold 100000000) ; 1e8 = 100 MB (default: 800kB)

(defun lps/display-startup-time ()
  (message "Emacs started in %s seconds"
           (format "%.2f"
                   (float-time
                    (time-subtract after-init-time before-init-time)))))

(defun lps/display-garbage-collection ()
  (message "Emacs performed %d garbage collection"
           gcs-done))

(defun lps/restore-gc-cons ()
  ;; After startup, we restore gc-cons-threshold to a more reasonable value
  (setq gc-cons-threshold 10000000)) ; 1e7 = 10 MB

(setq read-process-output-max (* 8 1024 1024)) ;; 2mb

(add-hook 'emacs-startup-hook #'lps/display-startup-time)
(add-hook 'emacs-startup-hook #'lps/display-garbage-collection)
(add-hook 'emacs-startup-hook #'lps/restore-gc-cons)

;; This avoids a lot of regexp matches between filenames and
;; this large list of handlers.
;; TODO: make sure this does not break with TRAMP and desktop-read ?
(defvar file-name-handler-alist-old file-name-handler-alist)

(setq file-name-handler-alist nil)

(add-hook 'after-init-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

(defvar lps/only-built-in-p nil
  "Variable indicating if we can load, or even download, packages
that are not built-in.

Should be non-nil to only allow built-in packages.")

(when (member "--only-built-in" command-line-args)
  (message "Using only built-in packages")
  (setq command-line-args (delete "--only-built-in" command-line-args))
  (setq lps/only-built-in-p t))

(require 'use-package)
(use-package use-package
  :custom
  (use-package-keywords (add-to-list 'use-package-keywords :only-built-in))
  (use-package-defaults (add-to-list 'use-package-defaults
                                     '(:only-built-in ''absent t)
                                     nil 'equal))
  ;; Comment this line if you don't want to automatically install
  ;; all the packages that you are missing
  (use-package-always-ensure nil)
  ;; Uncomment the folllowing line to have a detailed startup log
  ;; (use-package-verbose t)
  ;; (use-package-compute-statistics t)
  ;; (use-package-always-defer t)
  :config
  (defun use-package-normalize/:only-built-in (_name-symbol keyword args)
    (use-package-only-one (symbol-name keyword) args
      #'use-package-normalize-value))

  ;; We could use the function `package-built-in-p'
  ;;
  ;; However, we might not want to load /all/ the built-in packages in
  ;; a "debug"/fresh install session, and reciprocally, we might have
  ;; some local features/packages configured with `use-package' that
  ;; are not built-in, although their code is already
  ;; present/tested/whatever.
  (defun use-package-handler/:only-built-in (name _keyword arg rest state)
    (let ((body (use-package-process-keywords name rest state)))
      `((when (or (not lps/only-built-in-p)
                  (eq ,arg t)
                  (and (eq ',name 'emacs)
                       (eq ,arg 'absent)))
          ,@body)))))

(defmacro system-case (&rest cases)
  "Light wrapper around `cl-case' on `system-type'"
  `(cl-case system-type
     ,@cases))

(defun lps/versionify (version)
  (cl-etypecase version
    (string version)
    (number (number-to-string version))
    (list (string-join (mapcar #'number-to-string version) "."))
    (symbol (if (eq version t)
                "0"
                (error "Can't understand this version number: %s " version)))))

(defmacro ensure-version (package version &rest body)
  "Execute BODY when the version of PACKAGE is larger than VERSION"
  (declare (indent 2))
  (let ((package-version (intern (concat (symbol-name package) "-version"))))
    `(when (version<= ,(lps/versionify version) ,package-version)
       ,@body)))

(defmacro ensure-emacs-version (version &rest body)
  (declare (indent 1))
  `(ensure-version emacs ,version
     ,@body))

(defmacro version-case (package &rest cases)
  "CASES is a list of (VERSION BODY) where version is a version
number or a string. The macro expands to the code associated the
latest possible version.
As a special case, the version T is considered to be smaller than
all the other versions"
  (declare (indent 1))
  (let ((versions (sort cases (lambda (v1 v2)
                                (version<= (lps/versionify (car v1))
                                           (lps/versionify (car v2))))))
        (gver (make-symbol "version"))
        (package-version (intern (concat (symbol-name package) "-version")))
        version-conds)
    (dolist (ver versions)
      (let ((v-num (car ver))
            (v-body (cdr ver)))
        (push (cons `(version<= ,(lps/versionify v-num) ,gver) v-body)
              version-conds)))
    `(let ((,gver ,package-version))
       (cond
        ,@version-conds))))

(defmacro ensure-defun (name args-or-version &rest body)
  "Define the function NAME if it not already defined.
If ARGS-OR-VERSION is a list, it is considered to be the lambda-list of
the function NAME, and BODY is its body.
If it is a string or an integer, it is the version number before which
the function NAME will unconditionnally be defined, even it is already
fboundp."
  (declare (indent defun))
  (let (args version)
    (if (or (stringp args-or-version)
            (integerp args-or-version))
        (progn
          (setq args (car body))
          (setq version (lps/versionify args-or-version))
          (setq body (cdr body)))
      (setq args args-or-version))
    `(when (or (and ,version (version<= emacs-version ,version))
               (not (fboundp ',name)))
       (defun ,name ,args
         ,@body))))

;; Macro used to advice a function so that it is always called with
;; some lexical bindings
(defmacro advice-ensure-bindings (fun bindings)
  (let ((wrap-fun (gensym "fun"))
        (wrap-args (gensym "args")))
    `(advice-add ',fun
                 :around (lambda (,wrap-fun &rest ,wrap-args)
                           (let ,bindings
                             (apply ,wrap-fun ,wrap-args))))))

(defvar lps/defun-overriden nil
  "List of overriden functions by `defun-override'. Used to keep track of
the internal changes made by this config.")

(defmacro defun-override (prefixed-old-name lambda-list &rest body)
  (declare (doc-string 3) (indent 2))
  (let* ((old-name (intern (string-trim-left (symbol-name prefixed-old-name)
                                             "lps/"))))
    `(progn
       (defun ,prefixed-old-name ,lambda-list
         ,@body)

       (advice-add ',old-name :override ',prefixed-old-name)

       (add-to-list 'lps/defun-overriden ',old-name))))

(defun add-hook-once (hook function &optional depth local)
  (let (fun-add fun-remove)
    (setf fun-add (lambda ()
                    (unwind-protect
                        (funcall function)
                      (funcall fun-remove))))
    (setf fun-remove (lambda ()
                       (remove-hook hook fun-add)))
    (add-hook hook fun-add depth local)))

;; Poor man's benchmark macro
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  (let ((cur-time (gensym "TIME"))
        (res (gensym "RES")))
    `(let ((,cur-time (current-time)))
       (let ((,res (progn ,@body)))
         (message "%.06f" (float-time (time-since ,cur-time)))
         ,res))))

(use-package emacs
  :init
  ;; Use the right font according to what is installed on the system
  (defvar lps/default-font
    (system-case
     (gnu/linux "DejaVu Sans Mono")))

  (defvar lps/fixed-font
    (system-case
     (gnu/linux "DejaVu Sans Mono")))

  (defvar lps/variable-font
    (system-case
     (gnu/linux "Cantarell")))

  (defun lps/set-default-fonts ()
    ;; Variable pitch
    (let ((all-fonts (font-family-list))
          (font-assoc `((variable-pitch ,lps/variable-font)
                        (fixed-pitch ,lps/fixed-font)
                        (default ,lps/default-font))))
      (dolist (new-font font-assoc)
        (let ((font-name (car new-font))
              (font-val (cadr new-font)))
          (when (member font-val all-fonts)
            (set-face-attribute font-name nil :family font-val))))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (lps/set-default-fonts))))
    (lps/set-default-fonts)))

(use-package nerd-icons
  ;; Why is it needed ? Shouldn't it be called automatically ?
  ;; Anyway, I need that for Emacs to automatically use the right font
  ;; for this Unicode codepoints ...
  :init
  ;; TODO: clarify which one is needed ?
  (defun lps/setup-nerd-fonts ()
    (when (display-graphic-p)
      (nerd-icons-set-font)))

  (if (daemonp)
      (add-hook 'server-after-make-frame-hook 'lps/setup-nerd-fonts)
    (add-hook-once 'post-command-hook 'lps/setup-nerd-fonts)))

(setq inhibit-startup-message t
      ;; Hack to speedup startup, but have easy mode change !
      initial-scratch-message ";; (lisp-interaction-mode)\n;; (org-mode)\n\n"
      initial-major-mode 'fundamental-mode)
;; Emacs frame startup
;; Maximize the Emacs frame at startup
(dolist (prop '((fullscreen . maximized)
                ;; (visibility . nil)
                ))
  (add-to-list 'default-frame-alist prop nil 'equal))

(when (version<= "29" emacs-version)
  (add-to-list 'default-frame-alist '(alpha-background . 95))
  (set-frame-parameter nil 'alpha-background 95))

;; (add-hook 'after-make-frame-functions 'make-frame-visible)

;; Bell
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

;; Menus
;; Doom inspired: don't call the functions directly
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq scroll-bar-mode nil
      tool-bar-mode nil
      menu-bar-mode nil)

(tooltip-mode -1)
(set-fringe-mode 10)

;; Themes
(use-package kaolin-themes
  :custom
  (kaolin-themes-comments-style 'alt)
  (kaolin-themes-distinct-parentheses t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-hl-line-colored t)
  (kaolin-themes-org-scale-headings nil))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-project-detection 'project)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-mu4e t)
  (mode-line-compact 'long)
  :config
  ;; Fix a bug where symbol-with-pos are inserted instead of "bare symbols"
  (ensure-emacs-version 29
    (let ((remove-pos-from-seg (lambda (it)
                                 (cons (remove-pos-from-symbol (car it)) (cdr it)))))
      (setq doom-modeline-fn-alist (mapcar remove-pos-from-seg doom-modeline-fn-alist)
            doom-modeline-var-alist (mapcar remove-pos-from-seg doom-modeline-var-alist))))

  ;; Fix an Eldoc bug ? Doc seems to pop in a random buffer, and so
  ;; force-mode-line-update doesn't refresh the "current" mode-line. Still
  ;; somewhat bad: the doc might appear in a dimmed buffer, and so it is hard to
  ;; "find" at first glance, and moreover is dimmed itself so hard to read. Still,
  ;; better than nothing.

  ;; TODO: make a *proper* fix, so that Eldoc puts documentation in the correct
  ;; buffer, and so that force-mode-line-update is enough to update it (without
  ;; changing other buffers' modelines)
  (defun-override lps/doom-modeline-eldoc-minibuffer-message (format-string &rest args)
    "Display message specified by FORMAT-STRING and ARGS on the mode-line as needed.
  This function displays the message produced by formatting ARGS
  with FORMAT-STRING on the mode line when the current buffer is a minibuffer.
  Otherwise, it displays the message like `message' would."
    (if (minibufferp)
        (progn
          (add-hook 'minibuffer-exit-hook
                    (lambda () (setq eldoc-mode-line-string nil
                                     ;; https://debbugs.gnu.org/16920
                                     eldoc-last-message nil))
                    nil t)
          (with-current-buffer
              (window-buffer
               (or (window-in-direction 'above (minibuffer-window))
                   (minibuffer-selected-window)
                   (get-largest-window)))
            (setq eldoc-mode-line-string
                  (when (stringp format-string)
                    (apply #'format-message format-string args)))
            (force-mode-line-update 'all))) ; only diff is in the non-nil arg here
      (apply #'message format-string args)))
  ;; Hide encoding in modeline when UTF-8(-unix)
  (defun lps/hide-utf-8-encoding ()
    (setq-local doom-modeline-buffer-encoding
                (not (or (eq buffer-file-coding-system 'utf-8-unix)
                         (eq buffer-file-coding-system 'utf-8)))))

  (add-hook 'after-change-major-mode-hook #'lps/hide-utf-8-encoding)

  ;; Add recursive-depth info to the mode line
  ;; Useful for e.g. Isearch sessions
  (let ((rec-depth-indicator '(:eval
                               (let ((rec-depth (recursion-depth)))
                                 (unless (zerop rec-depth)
                                   (propertize (format "[%d] " rec-depth)
                                               'face
                                               '(:foreground "orange red")))))))
    (unless (and (listp global-mode-string)
                 (member rec-depth-indicator global-mode-string))
      (push rec-depth-indicator global-mode-string)))

  ;; Hack, as we disable minor modes in mode-line
  ;; Put this in global-mode-string, where it definitely does not belong ...
  (cl-pushnew '(:eval
                (when (bound-and-true-p company-search-mode)
                  company-search-lighter))
              global-mode-string
              :test 'equal)

  (doom-modeline-def-modeline 'lps/pdf
    '(bar window-number modals matches buffer-info pdf-pages)
    '(compilation misc-info mu4e major-mode process vcs time))

  (add-to-list 'doom-modeline-mode-alist
               '(pdf-view-mode . lps/pdf)
               nil
               'equal))
