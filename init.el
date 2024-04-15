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

(defvar lps/only-built-in-p nil
  "Variable indicating if we can load, or even download, packages
that are not built-in.

Should be non-nil to only allow built-in packages.")

(when (member "--only-built-in" command-line-args)
  (message "Using only built-in packages")
  (setq command-line-args (delete "--only-built-in" command-line-args))
  (setq lps/only-built-in-p t))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents t))       ; Async

(setq package-native-compile t)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Symlink (or directly place) your personal packages in this
;; directory. Simple way to add personal packages Need to use
;; `update-file-autoloads' or `update-directory-autoloads' in this dir
;; and regularly and place the autoloads in the
;; personal-<private-shared>autoloads.el file
;; TODO: fix this with new autoloads as of version 29 !

;; (let* ((extra-package-dir (expand-file-name "extra-packages"
;;                                             user-emacs-directory))
;;        (extra-package-dir-shared (expand-file-name "shared" extra-package-dir))
;;        (extra-package-dir-private (expand-file-name "private" extra-package-dir))
;;        (extra-autoloads (list (expand-file-name "personal-private-autoloads.el"
;;                                                 extra-package-dir-private)
;;                               (expand-file-name "personal-shared-autoloads.el"
;;                                                 extra-package-dir-shared))))
;;   (add-to-list 'load-path extra-package-dir-shared)
;;   (add-to-list 'load-path extra-package-dir-private)
;;   (dolist (file extra-autoloads)
;;     (when (file-exists-p file)
;;       (load file))))


(defun lps/reinstall-package (pkg)
  (interactive (list (intern (completing-read "Reinstall package: "
                                              (mapcar #'car package-alist)))))
  (ignore-errors
    (unload-feature pkg t))
  (package-reinstall pkg)
  (require pkg))

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
  (use-package-verbose t)
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

(use-package benchmark-init
  :disabled t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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

(defmacro defun-override (name-or-old-new-name lambda-list &rest body)
  (declare (doc-string 3) (indent 2))
  (let* ((name
          (if (listp name-or-old-new-name)
              (cadr name-or-old-new-name)
            name-or-old-new-name))
         (old-name
          (if (listp name-or-old-new-name)
              (car name-or-old-new-name)
            (intern (string-trim-left (symbol-name name-or-old-new-name) "lps/")))))
    `(progn
       (defun ,name ,lambda-list
         ,@body)

       (advice-add ',old-name :override ',name))))

(use-package emacs
  :ensure nil
  :init
  (defvar lps/quick-edit-map (make-sparse-keymap))
  (defvar lps/system-tools-map (make-sparse-keymap))
  (defvar lps/all-hydras-map (make-sparse-keymap))
  (defvar lps/manipulate-lines-map (make-sparse-keymap)))

(use-package package
  :ensure nil
  :bind-keymap
  ("C-c s" . lps/system-tools-map)
  :bind
  (:map lps/system-tools-map
        ("P i" . package-install)
        ("P l" . package-list-packages)
        ("P d" . package-delete)
        ("P u" . package-update)))

(system-case
 (gnu/linux
  (use-package password-cache
    :custom
    (password-cache t)
    (password-cache-expiry 300))

  (use-package pinentry
    :custom
    (epg-pinentry-mode 'loopback)
    :config
    (pinentry-start))

  (use-package auth-source
    :custom
    (auth-sources (remove "~/.authinfo" auth-sources))
    (auth-source-cache-expiry 86400) ;; All day

    :config
    (defvar lps/auth-cache-expiry-setup-p t) ; change it to ask for duration on startup

    (defun lps/auth-source-define-cache-expiry ()
      (interactive)
      (unless lps/auth-cache-expiry-setup-p
        (setq lps/auth-cache-expiry-setup-p t)
        (when (y-or-n-p (concat "Change default auth-cache-expiry value "
                                "(default "
                                (number-to-string auth-source-cache-expiry)
                                ") ?"))
          (setq auth-source-cache-expiry
                (read-number "New cache expiry value in seconds: " auth-source-cache-expiry)))))

    (defun lps/force-forget-all-passwords ()
      (interactive)
      (auth-source-forget-all-cached)
      (shell-command "gpgconf --kill gpg-agent")
      ;; (shell-command "gpgconf -- reload gpg-agent")
      (setq lps/auth-cache-expiry-setup-p nil)))))

(ensure-emacs-version 29
  (use-package emacs
    :bind
    (:map lps/system-tools-map
          ("r" . restart-emacs))))

(use-package desktop
  :init
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (let ((desktop-load-locked-desktop t))
                (desktop-save-mode 1)
                (desktop-read (car desktop-path)))))
  (unless (daemonp)
    (desktop-save-mode 1))
  :custom
  (desktop-restore-frames nil) ;; Otherwise buggy with daemon-mode
  (desktop-path (list (locate-user-emacs-file "desktop-saves/")))
  (desktop-restore-eager 10)
  (desktop-lazy-verbose nil)
  (desktop-modes-not-to-save '(tags-table-mode     ; default
                               eglot--managed-mode ; seems buggy ?
                               ))
  (desktop-restore-forces-onscreen nil))

(use-package saveplace
  :ensure nil
  :custom
  (save-place-file (locate-user-emacs-file ".saveplaces"))
  (save-place-limit 1000)               ; better be safe
  :init
  (save-place-mode 1))

(use-package server
  :custom
  (server-client-instructions nil))

(use-package emacs
  :custom
  (custom-file (locate-user-emacs-file "custom-file.el"))
  :config
  (load custom-file 'noerror))

(ensure-emacs-version 28
 (use-package emacs
   :custom
   (native-comp-async-report-warnings-errors 'silent)))

(use-package emacs
  :custom
  (locale-coding-system 'utf-8)
  (display-raw-bytes-as-hex t)
  (default-transient-input-method "rfc1345") ; see https://www.rfc-editor.org/rfc/rfc1345
  :init
  (prefer-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-clipboard-coding-system 'utf-8)
  (set-file-name-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(use-package emacs
  :init
  (define-key key-translation-map (kbd "<C-dead-circumflex>") (kbd "C-^"))
  (define-key key-translation-map (kbd "<M-dead-circumflex>") (kbd "M-^"))
  :bind
  (:map lps/quick-edit-map
        ("C-z" . iso-transl-ctl-x-8-map)))

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
            (set-face-font font-name font-val))))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (lps/set-default-fonts))))
    (lps/set-default-fonts)))

(use-package emacs
  :custom
  (inhibit-startup-message t)
  (initial-scratch-message nil)
  :init
  ;; Emacs frame startup
  ;; Maximize the Emacs frame at startup
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (ensure-emacs-version 29
   (add-to-list 'default-frame-alist '(alpha-background . 95))
   (set-frame-parameter nil 'alpha-background 95))

  ;; Bell
  (setq ring-bell-function 'ignore)
  (setq visible-bell nil)

  ;; Menus
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)
  (menu-bar-mode -1))

(use-package emacs
  :init
  (column-number-mode t)

  (defun lps/activate-truncate-lines ()
    (let ((inhibit-message t))
      (toggle-truncate-lines 1)))

  (defvar lps/truncate-lines-modes-hook '(dired-mode-hook
                                          outline-mode-hook
                                          tabulated-list-mode-hook
                                          occur-mode-hook)
    "Modes in which `truncate-lines' will be set to `t' automatically")

  (dolist (hook lps/truncate-lines-modes-hook)
    (add-hook hook 'lps/activate-truncate-lines))

  (defun lps/enable-auto-hscroll-scroll ()
    (setq-local auto-hscroll-mode t))
  :custom
  (hscroll-margin 10)
  (hscroll-step 10)
  (auto-hscroll-mode 'current-line)
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t)
  (fill-column 80)                      ; default 70 is a bit low
  :hook
  ((prog-mode LaTeX-mode) . display-line-numbers-mode)
  ((text-mode org-mode LaTeX-mode comint-mode) . visual-line-mode)
  (LaTeX-mode . auto-fill-mode))

;; Themes
(use-package kaolin-themes
  :custom
  (kaolin-themes-comments-style 'alt)
  (kaolin-themes-distinct-parentheses t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-hl-line-colored t)
  (kaolin-themes-org-scale-headings nil)
  :config
  ;; TODO: fix stuff happening in a recent Emacs version
  ;; The :box attribute of faces used to support ":style none"
  ;; This is no longer the case: must be NIL (I guess), or some other
  ;; correct value. See e.g. a fix in doom-themes here
  ;; https://github.com/doomemacs/themes/issues/809
  ;; In kaolin themes, I find no other version of :style none,
  ;; but there might be some remaining.
  ;; Moreover, even after doing this, it seems that the actual attribute
  ;; is not changed.
  ;; Even this fix is not enough: the theme /loading/ makes Emacs sad :(
  ;; FIXME: currently, themes files are modified within the elpa/<kaolin...>
  ;; folder, which is bad.
  (when (version<= "30" emacs-version)
    (set-face-attribute 'custom-button nil :box
                        '(:line-width 1 :color "cerulean4"))
    (set-face-attribute 'custom-button-mouse nil :box
                        '(:line-width 1 :color "amber3"))
    (set-face-attribute 'custom-button-pressed nil :box '(:line-width 1 "gray3"))))

(when lps/only-built-in-p
  (load-theme 'deeper-blue))

(use-package emacs
  :after kaolin-themes
  :init
  (defvar lps/default-theme 'kaolin-ocean)
  (defvar lps/default-light-theme 'modus-operandi)
  (defvar lps/live-presentation-p nil)

  (load-theme lps/default-theme t)

  :bind
  (:map lps/quick-edit-map
        ("c" . lps/resize-and-color-region))

  :config
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     lps/default-theme
     '(hl-line ((t (:background "#39424D"))) t)))

  (defun lps/toggle-live-code-presentation-settings ()
    "Various useful settings for live coding sessions
  Still very buggy, but this should not matter in a live presentation
  setting.
  Avoid toggling several times, just use it once if possible"
    (interactive)
    (if lps/live-presentation-p
        (progn
          (unless (equal custom-enabled-themes (list lps/default-theme))
            (disable-theme (car custom-enabled-themes))
            (load-theme lps/default-theme t))
          (global-hl-line-mode -1)
          (text-scale-set 0)
          (setq-default cursor-type 'box))

      (progn
        (unless (y-or-n-p "Keep current theme ?")
          (disable-theme custom-enabled-themes)
          (load-theme lps/default-light-theme t)
          (custom-theme-set-faces
           lps/default-light-theme
           '(hl-line ((t (:background "#DFD8EE"))) t)))
        (global-display-line-numbers-mode 1)
        (global-hl-line-mode 1)
        (text-scale-increase 2)
        (setq-default cursor-type 'bar)))

    (setq lps/live-presentation-p (not lps/live-presentation-p)))

;;; Inspired from https://www.reddit.com/r/emacs/comments/vb05co/resizerecolour_text_onthefly/
  (defun lps/resize-and-color-region (beg end &optional reset)
    "Resize/recolour selected region;defaulting to blue at size 300,for titles.
  If RESET is non-NIL, revert to the previous colour/size."
    (interactive "r\nP")
    (let ((contents (buffer-substring-no-properties beg end))
          (revert-props (get-text-property beg 'old-props))
          (old-props (text-properties-at beg))) ; assume same everywhere
      (when contents
        (delete-region beg end)
        (if reset
            (insert (apply 'propertize contents revert-props))
          (let ((color (read-color "Colour: "))
                (size (read-number "Size: ")))
            (insert (propertize contents
                                'font-lock-face
                                `(:foreground ,color :height ,size)
                                'old-props
                                old-props))))))))

(use-package emacs
  :commands lps/slideshow-mode
  :defer t
  :init
  (defvar-local lps/slideshow-mode-line--old-format nil
    "Storage for the old `mode-line-format', to be restored upon
quitting the minor mode")
  (defvar-local lps/slideshow--old-window-configuration nil
    "Storage for the old window configuration, to be restored upon
quitting the minor mode")
  (define-minor-mode lps/slideshow-mode
    "Minor mode to use for nice slideshows.

All windows except the current one are deleted.
The mode-line is also hidden, and the frame becomes full-screen.

Upon quitting `lps/slideshow-mode', the previous window configuration,
the mode-line and the usual non-full-screen Emacs are restored."
    :init-value nil
    :global nil
    (if lps/slideshow-mode
        (progn
          (unless lps/slideshow-mode-line--old-format
            (setq lps/slideshow-mode-line--old-format mode-line-format))
          (unless lps/slideshow--old-window-configuration
            (setq lps/slideshow--old-window-configuration
                  (current-window-configuration)))
          (setq mode-line-format nil)
          (toggle-frame-fullscreen)
          (delete-other-windows)
          (pdf-view-goto-page 1))
      (setq mode-line-format lps/slideshow-mode-line--old-format
            lps/slideshow-mode-line--old-format nil)
      (toggle-frame-fullscreen)
      (set-window-configuration lps/slideshow--old-window-configuration)
      (setq lps/slideshow--old-window-configuration nil))
    (redraw-display)))

;; First time used: run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :config
  ;; Avoid unnecessary warnings
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-fileicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)

  ;;define an icon function with all-the-icons-faicon
  ;;to use filecon, etc, define same function with icon set
  (defun with-faicon (icon str &rest height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  ;; filecon
  (defun with-fileicon (icon str &rest height v-adjust)
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str)))

(use-package doom-modeline
  :after all-the-icons
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

  (doom-modeline-mode 1)

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
              :test 'equal))

(use-package battery
  :ensure nil
  :only-built-in t
  :config
  (when (and battery-status-function
              (let ((status (battery-format "%B" (funcall battery-status-function))))
                (not (or (string-match-p "N/A" status)
                         (string-match-p "unknown" status)))))
      (display-battery-mode 1)))

(use-package time
  :ensure nil
  :only-built-in t
  :custom
  (display-time-24hr-format t)
  (display-time-format "[%H:%M]")
  :init
  (display-time-mode 1))

(use-package beacon
  :init
  (beacon-mode 1)
  :custom
  (beacon-blink-when-point-moves-vertically 30)
  (beacon-size 20))

(use-package rainbow-mode
  :defer t)

(use-package highlight-numbers
  :hook ((prog-mode LaTeX-mode) . highlight-numbers-mode))

(use-package hl-line
  :only-built-in t
  :hook ((tabulated-list-mode
          ibuffer-mode
          dired-mode
          proced-mode)
         . hl-line-mode))

(use-package hl-todo
  :init
  (global-hl-todo-mode 1)
  :bind
  (:map hl-todo-mode-map
        ("C-c t p" . hl-todo-previous)
        ("C-c t n" . hl-todo-next)
        ("C-c t o" . hl-todo-occur)
        ("C-c t i" . hl-todo-insert))
  :custom
  (hl-todo-include-modes '(prog-mode text-mode))
  (hl-todo-color-background t)
  (hl-todo-wrap-movement t)
  (hl-todo-highlight-punctuation ":!.?")
  (hl-todo-keyword-faces `(("TODO" . "#cc9393")
                           ("FAIL" . "#8c5353")
                           ("DONE" . "#afd8af")
                           ("HACK" . "#d0bf8f")
                           ("FIXME" . "#cc9393")))
  :config
  (defvar hl-todo-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'hl-todo-next)
      (define-key map (kbd "p") #'hl-todo-previous)
      (define-key map (kbd "i") #'hl-todo-insert)
      map))
  (dolist (cmd '(hl-todo-next hl-todo-previous hl-todo-insert))
    (put cmd 'repeat-map 'hl-todo-repeat-map)))

(use-package emacs
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :init
  ;; Tab behaviour and whitespaces
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  :custom
  (cycle-spacing-actions '(just-one-space
                           (delete-all-space inverted-arg)
                           delete-all-space
                           restore))
  :bind
  (:map lps/quick-edit-map
        ("DEL" . cycle-spacing)
        ("<C-backspace>" . join-line)
        ("<C-S-backspace>" . join-next-line))
  :config
  (defun join-next-line (&optional beg end)
    (interactive
     (progn (barf-if-buffer-read-only)
            (and (use-region-p)
                 (list (region-beginning) (region-end)))))
    (join-line t beg end)))

(use-package page-break-lines
  :hook (emacs-news-mode . page-break-lines-mode))

(use-package hydra
  :defer t
  :bind-keymap ("C-c h" . lps/all-hydras-map))

;; Easier hydra definition
(use-package pretty-hydra
  :after hydra)

(use-package emacs
  :ensure nil
  :only-built-in nil
  :after pretty-hydra
  :bind (:map lps/all-hydras-map
              ("a" . hydra-appearance/body))
  :config
  ;; define a title function
  (defvar appearance-title (with-faicon "desktop" "Appearance"))

  ;; generate hydra

  (pretty-hydra-define hydra-appearance (:title appearance-title
                                                :quit-key "q")
    ("Theme"
     (
      ;;     ("o" olivetti-mode "Olivetti" :toggle t)
      ;;     ("t" toggle-window-transparency "Transparency" :toggle t )
      ("c" lps/rotate-through-themes "Cycle Themes" )
      ("t" lps/restore-initial-themes "Restore Theme")
      ("+" text-scale-increase "Zoom In")
      ("-" text-scale-decrease "Zoom Out")
      ("x" toggle-frame-maximized "Maximize Frame" :toggle t )
      ("X" toggle-frame-fullscreen "Fullscreen Frame" :toggle t))
     "Highlighting"
     (("d" rainbow-delimiters-mode "Rainbow Delimiters" :toggle t )
      ("r" rainbow-mode "Show Hex Colours" :toggle t )
      ("n" highlight-numbers-mode "Highlight Code Numbers" :toggle t )
      ("l" display-line-numbers-mode "Show Line Numbers" :toggle t )
      ("_" global-hl-line-mode "Highlight Current Line" :toggle t )
      ;;    ("I" rainbow-identifiers-mode "Rainbow Identifiers" :toggle t )
      ("b" beacon-mode "Show Cursor Trailer" :toggle t )
      ("w" whitespace-mode "Show Whitespaces" :toggle t))
     "Miscellaneous"
     (("j" visual-line-mode "Wrap Line Window"  :toggle t)
      ("m" visual-fill-column-mode "Wrap Line Column"  :toggle t)
      ;;    ("a" adaptive-wrap-prefix-mode "Indent Wrapped Lines" :toggle t )
      ;;   ("i" highlight-indent-guides-mode  "Show Indent Guides" :toggle t )
      ("g" fci-mode "Show Fill Column" :toggle t )
      ("<SPC>" nil "Quit" :color blue )))))

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (completions-group t)
  :config
  (minibuffer-depth-indicate-mode 1)

  (defun lps/disable-minibuffer-completion-help (fun &rest args)
    (cl-letf (((symbol-function #'minibuffer-completion-help)
               #'ignore))
      (apply fun args)))

  (defun lps/completing-read-in-region (start end collection &optional predicate)
    "Prompt for completion of region in the minibuffer if non-unique.
   Use as a value for `completion-in-region-function'.

It might be buggy with some backend, so use at your own risk"
    (let* ((initial (buffer-substring-no-properties start end))
           (all (completion-all-completions initial collection predicate
                                            (length initial)))
           (completion (cond
                        ((atom all) nil)
                        ((and (consp all) (atom (cdr all))) (car all))
                        (t (completing-read
                            "Completion: " collection predicate nil initial)))))
      (cond (completion (completion--replace start end completion) t)
            (t (message "No completion") nil))))

  (defmacro lps/with-completing-read-in-region (&rest body)
    (declare (indent 1))
    `(let ((completion-in-region-function 'lps/completing-read-in-region))
       ,@body)))

(use-package icomplete
  :ensure nil
  :when lps/only-built-in-p
  :only-built-in t
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-compute-delay 0.05)
  :config
  (icomplete-vertical-mode 1))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("<C-backspace>" . lps/minibuffer-go-up-directory))
  :config
  (defun lps/minibuffer-go-up-directory (arg)
    (interactive "p")
    (let* ((filename (minibuffer-contents))
           (directory-maybe (file-name-directory filename))
           (directory (if (and (string-suffix-p "/" filename)
                               (equal filename directory-maybe))
                          (file-name-directory (substring filename 0 -1))
                        directory-maybe)))
      (if directory
          (progn
            (delete-minibuffer-contents)
            (insert directory))
        (backward-kill-word arg)))))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package emacs
  :ensure nil
  :bind
  ([remap kill-buffer] . lps/kill-buffer)
  :init
  ;; Automatically reload a file if it has been modified
  (global-auto-revert-mode t)

  :custom
  (display-buffer-base-action
   '((display-buffer-reuse-window)
     (display-buffer-reuse-mode-window)
     (display-buffer-in-previous-window)
     (display-buffer-same-window)))
  (uniquify-buffer-name-style 'forward)
  (uniquify-after-kill-buffer-p t)
  (global-auto-revert-ignore-modes '(pdf-view-mode))

  :config
  (defun lps/kill-buffer (&optional arg)
    "Kill the current buffer if no ARG. Otherwise, prompt for a
buffer to kill. If ARG is nil and the function is called from the
minibuffer, exit recursive edit with `abort-recursive-edit'"
    (interactive "P")
    (if arg
        (call-interactively 'kill-buffer)
      (if (minibufferp)
          (abort-recursive-edit)
        (kill-buffer (current-buffer)))))

  ;; Display all the "help" buffers in the same window
  (defvar lps/help-modes '(helpful-mode
                           help-mode
                           Man-mode
                           apropos-mode
                           Info-mode))

  ;; Help buffers with special name
  (defvar lps/help-buffers nil)

  (defun lps/buffer-help-p (buffer &optional action)
    "Return t if BUFFER is an help buffer, nil otherwise"
    (when buffer
      (or (member (buffer-local-value 'major-mode (get-buffer buffer))
                  lps/help-modes)
          (member (if (stringp buffer)
                      buffer
                    (buffer-name buffer))
                  lps/help-buffers))))

  (add-to-list 'display-buffer-alist
               `(lps/buffer-help-p
                 (display-buffer--maybe-same-window
                  display-buffer-reuse-window
                  display-buffer-reuse-mode-window)
                 (mode . ,lps/help-modes)
                 (inhibit-same-window . nil)
                 (quit-restore ('window 'window nil nil)))))

(use-package all-the-icons-ibuffer
  :defer t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package ibuffer
  :defer t
  :only-built-in t
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-saved-filter-groups
   '(("default"
      ("Dired" (derived-mode . dired-mode))
      ("Emacs" (or
                (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")))
      ("Help" (predicate lps/buffer-help-p (current-buffer)))
      ("Special" (and
                  (not (process))
                  (or
                   (starred-name)
                   (mode . special-mode))))
      ("Org Files" (mode . org-mode))
      ("Process" (process))
      ("Git" (name . "^magit"))
      ("Images/PDF" (or
                     (file-extension . "pdf")
                     (mode . image-mode)))
      ("Programming" (and
                      (derived-mode . prog-mode)
                      (not (mode . fundamental-mode))))
      ("Mail" (or
               (name . "^\\*mm\\*.*$") ; heuristic for attachments
               (derived-mode . gnus-article-mode)
               (mode . mu4e-headers-mode)
               (mode . mu4e-main-mode))))))
  :config
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)

  (defun lps/ibuffer-switch-to-default-filter ()
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook #'lps/ibuffer-switch-to-default-filter))

(use-package winner
  :only-built-in t
  :custom
  (winner-boring-buffers '("*Completions*"
                           "*Compile-Log*"
                           "*Fuzzy Completions*"
                           "*Apropos*"
                           "*Help*"
                           "*Buffer List*"
                           "*Ibuffer*"))
  :init
  (winner-mode 1))

(use-package windmove
  ;; Make windmove work in Org mode:
  :only-built-in t
  :hook
  (org-shiftup-final . windmove-up)
  (org-shiftleft-final . windmove-left)
  (org-shiftdown-final . windmove-down)
  (org-shiftright-final . windmove-right)

  :init
  (windmove-default-keybindings 'shift)
  (windmove-swap-states-default-keybindings '(ctrl shift))

  (defun lps/windmove-mode-local-off ()
    ;; Hack to disable windmove locally
    (setq-local windmove-mode nil))

  (defun lps/windmove-mode-local-off-around (fun &rest args)
    (unwind-protect
        (progn
          (add-hook 'minibuffer-setup-hook 'lps/windmove-mode-local-off)
          (apply fun args))
      (remove-hook 'minibuffer-setup-hook 'lps/windmove-mode-local-off))))

;; Taken from https://emacs.stackexchange.com/questions/2189/how-can-i-prevent-a-command-from-using-specific-windows
(defun lps/toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))

(require 'xdg)

(use-package ffap
  :ensure nil
  :only-built-in t
  :bind ("C-c C-f" . ffap-menu)
  :init
  (ffap-bindings)
  :custom
  (ffap-pass-wildcards-to-dired t)
  :config
  (defun lps/find-file-as-root (filename)
    "Switch to a buffer visiting the file FILENAME as root, creating
one if none exists."
    (interactive "P")
    (find-file (concat "/sudo:root@localhost:" filename)))

  (advice-add #'ffap-menu-ask :around 'lps/disable-minibuffer-completion-help))

(use-package recentf
  :ensure nil
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 50)
  :config
  (dolist (excl (list (expand-file-name (locate-user-emacs-file "eshell/"))
                      (expand-file-name (locate-user-emacs-file "\\.elfeed/"))
                      "\\.synctex\\.gz" "\\.out$" "\\.toc" "\\.log"
                      (expand-file-name recentf-save-file)
                      "/usr/local/share/emacs/"
                      "bookmarks$"
                      (expand-file-name "~/Mail/")))
    (add-to-list 'recentf-exclude excl)))

(use-package emacs
  :custom
  (require-final-newline t)
  (view-read-only t))

(use-package emacs
  :ensure nil
  :only-built-in t
  :custom
  (delete-by-moving-to-trash t)
  :init
  ;; From Magnars, from emacsrocks.com
  (defun lps/rename-current-buffer-file ()
    "Renames current buffer and file it is visiting."
    (interactive)
    (let* ((name (buffer-name))
           (filename (buffer-file-name))
           (basename (file-name-nondirectory filename)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" name)
        (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
          (if (get-buffer new-name)
              (error "A buffer named '%s' already exists!" new-name)
            (rename-file filename new-name 1)
            (rename-buffer new-name)
            (set-visited-file-name new-name)
            (set-buffer-modified-p nil)
            (message "File '%s' successfully renamed to '%s'"
                     name (file-name-nondirectory new-name)))))))

  (defun lps/delete-current-buffer-file (&optional arg)
    "Delete the file visited by the current buffer
Always delete by moving to trash, regardless of `delete-by-moving-to-trash'
If called with a prefix argument, also kills the current buffer"
    (interactive "P")
    (let ((filename (buffer-file-name)))
      (if (not (and filename (file-exists-p filename)))
          (error "Buffer '%s' is not visiting a file!" (buffer-name))
        (delete-file filename t)
        (when arg
          (kill-buffer)))))

  :bind
  (:map ctl-x-x-map
        ("R" . lps/rename-current-buffer-file)
        ("D" . lps/delete-current-buffer-file)))

(use-package emacs
  :ensure nil
  :only-built-in t
  :init
  (defvar lps/backup-directory (locate-user-emacs-file".backups/"))
  (unless (file-exists-p lps/backup-directory)
    (make-directory lps/backup-directory))

  (setq backup-directory-alist `(("." . ,lps/backup-directory)))

  (defun lps/disable-auto-save-mode ()
    (auto-save-mode -1)))

(use-package emacs
  :bind
  (:map ctl-x-x-map
        ("s" . lps/save-as-temp-file))
  :init
  (defun lps/save-as-temp-file ()
    "Save the current buffer to a temporary file.

If the buffer name is NAME.EXT (where EXT is optional), the
resulting temporary file will use NAME as a prefix and EXT as an
extension.

The code is not very robust, and more or less assumes that the
buffer name already resembles a file name"
    (interactive)
    (let* ((name (buffer-name))
           (ext (file-name-extension name))
           (name-sans (file-name-base name)))
      (write-file (make-temp-file (or name-sans "")
                                  nil
                                  (and ext (concat "." ext)))))))

(use-package outline
  :ensure nil
  :defer t
  :hook (prog-mode . outline-minor-mode)
  :custom
  (outline-minor-mode-prefix (kbd "M-o"))
  :config
  ;; Problems with TAB -> completely override cycle keymap
  (setq outline-mode-cycle-map (make-sparse-keymap)))

(use-package emacs
  :ensure nil
  :custom
  (scroll-preserve-screen-position t)
  (scroll-error-top-bottom t)
  (mouse-wheel-tilt-scroll t))

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :bind
  (:map help-map
        (";" . helpful-at-point))
  (:map helpful-mode-map
        ("i" . lps/helpful-manual)
        ("s" . lps/helpful-source))
  :init
  (require 'helpful) ;; somewhat hacky, would like to autoload ...
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-symbol 'helpful-symbol)
  (defalias 'describe-key 'helpful-key)
  :config
  ;; Taken from `helpful--manual'
  (defun lps/helpful-manual ()
    (interactive)
    (info-lookup 'symbol helpful--sym #'emacs-lisp-mode))

  ;; From the main `helpful-update' and `helpful--navigate'
  (defun lps/helpful-source ()
    (interactive)
    (-let* ((primitive-p (helpful--primitive-p helpful--sym helpful--callable-p))
            (look-for-src (or (not primitive-p)
                              find-function-C-source-directory))
            ((buf pos opened)
             (if look-for-src
                 (helpful--definition helpful--sym helpful--callable-p)
               '(nil nil nil)))
            (source-path (when buf
                           (buffer-file-name buf))))
      (find-file source-path)
      (when pos
        (when (or (< pos (point-min))
                  (> pos (point-max)))
          (widen))
        (goto-char pos)))))

(use-package emacs
  :ensure nil
  :custom
  (apropos-documentation-sort-by-scores t)
  (describe-char-unidata-list t)
  :bind
  (:map help-map
        ("u" . describe-face)
        ("U" . describe-font)
        ("C-k" . describe-keymap)))

(use-package man
  :only-built-in t
  :bind
  (:map help-map
        ("M" . man))
  :custom
  (Man-notify-method 'aggressive)
  :config
  ;; Minor improvements to visual appearance:
  ;; sections and some keywords are easier to see
  (set-face-attribute 'Man-overstrike nil
                      :inherit font-lock-builtin-face
                      :bold t)
  (set-face-attribute 'Man-underline nil
                      :inherit font-lock-variable-name-face
                      :underline t))

(use-package info
  :bind
  (:map Info-mode-map
        ("S" . consult-info)))

;;; Add some colours to Info nodes
(use-package info-colors
  :hook (Info-selection . info-colors-fontify-node)
  :config
  ;; Just to see better lisp code block
  (set-face-attribute 'info-colors-lisp-code-block nil :weight 'bold))

(use-package info-rename-buffer
  :init
  (info-rename-buffer-mode 1))

;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init
  (which-key-mode 1)
  (which-key-setup-side-window-bottom) ;; default
  :diminish
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05))

(use-package help-at-pt
  :only-built-in t
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.5))

(use-package emacs
  :init
  (ensure-emacs-version "28.1"
    (context-menu-mode 1)))

;; Don't disable any command
;; BE CAREFUL
;; If you are a new user, you might to comment out this line
(setq disabled-command-function nil)

(global-unset-key (kbd "C-z"))

;; Generic Prescient configuration
(use-package prescient
  :custom
  (prescient-history-length 50)
  (prescient-sort-length-enable nil)
  :config
  (prescient-persist-mode 1))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package savehist
  :ensure nil
  :only-built-in t
  :init
  (savehist-mode))

(use-package keycast
  :defer t
  :custom
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-insert-after "%e")
  (keycast-mode-line-format "%10s%k%c%r%10s"))

;; Type "y" instead of "yes RET" for confirmation
(version-case emacs
  (28 (setq use-short-answers t))
  (t (defalias 'yes-or-no-p 'y-or-n-p)))

(use-package consult
  :defer t
  :bind
  ("C-S-s" . lps/consult-line-strict-match)
  ("C-c i" . lps/consult-imenu-or-org-heading)
  ("C-c r r" . consult-register-load)
  ("C-c r s" . consult-register-store)
  ([remap switch-to-buffer] . consult-buffer)
  ([remap repeat-complex-command] . consult-complex-command)
  ([remap bookmark-jump] . consult-bookmark)
  ([remap project-switch-to-buffer] . consult-project-buffer)
  ("M-g e" . consult-compile-error)
  ("M-g f" . consult-flymake)
  ("M-g o" . consult-outline)
  (:map isearch-mode-map
        ([remap isearch-edit-string] . consult-isearch-history))
  (:map lps/system-tools-map
        ("C-f" . consult-file-externally))
  :custom
  (consult-narrow-key "<")
  (xref-show-definitions-function 'consult-xref)
  (xref-show-xrefs-function 'consult-xref)
  :config
  (defun lps/consult-imenu-or-org-heading ()
    (interactive)
    (if (equal major-mode 'org-mode)
        (consult-org-heading)
      (consult-imenu)))

  (defun lps/consult-line-strict-match (&optional initial start)
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((orderless-matching-styles '(orderless-literal)))
      (consult-line initial start)))

  ;; Fix a bug in earlier version of Emacs
  (ensure-defun ensure-list "28.1" (x)
    (if (listp x) x (list x))))

(use-package embark
  :defer t
  :bind
  ("C-," . embark-act)
  ([remap describe-bindings] . embark-bindings)
  (:map embark-file-map
        ("s" . lps/find-file-as-root))
  :custom
  (embark-action-indicator #'lps/embark-indicator-which-key)
  (embark-become-indicator embark-action-indicator)
  :config
  (defun lps/embark-indicator-which-key (map &rest _ignore)
    (which-key--show-keymap "Embark" map nil nil 'no-paging)
    #'which-key--hide-popup-ignore-command))

(use-package embark-consult
  :after (consult embark))

(ensure-emacs-version 28.0
  (use-package repeat
    :only-built-in t
    :bind
    (:map lps/quick-edit-map
          ("z" . repeat))
    :init
    (repeat-mode 1)

    ;; Add visual indicator when repeat-mode is "activated"
    (defvar lps/default-cursor-background (face-background 'cursor))

    (defun lps/repeat-in-progress-change-cursor ()
      (set-cursor-color
       (if repeat-in-progress
           (face-foreground 'error)
         lps/default-cursor-background)))

    (add-hook 'post-command-hook
              'lps/repeat-in-progress-change-cursor
              90)))

(use-package emacs
  :ensure nil
  :bind
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  ([remap count-words-region] . count-words)
  ([remap count-words-region] . count-words))

(use-package emacs
  :ensure nil
  :custom
  (read-char-by-name-sort 'code))

(use-package multiple-cursors
  :defer t
  :init
  (defvar lps/multiple-cursors-map (make-sparse-keymap))
  (defvar lps/multiple-cursors-repeat-map (make-sparse-keymap))
  :bind
  ("<M-S-mouse-1>" . mc/add-cursor-on-click)
  (:map mc/keymap
        ("<return>" . nil))
  (:map lps/all-hydras-map
        ("M" . hydra-multiple-cursors/body))
  (:map lps/multiple-cursors-map
        ("<down>" . mc/mark-next-like-this)
        ("<up>" . mc/mark-previous-like-this)
        ("<right>" . mc/unmark-next-like-this)
        ("<left>" . mc/unmark-previous-like-this)
        ("a" . mc/mark-all-like-this)
        ("A" . mc/mark-all-dwim)
        ("d" . mc/mark-all-symbols-like-this-in-defun)
        ("D" . mc/mark-all-like-this-in-defun))
  (:map lps/multiple-cursors-repeat-map
        ("<down>" . mc/mark-next-like-this)
        ("<up>" . mc/mark-previous-like-this)
        ("<right>" . mc/unmark-next-like-this)
        ("<left>" . mc/unmark-previous-like-this))
  :bind-keymap
  ("C-ù" . lps/multiple-cursors-map)
  :config
  (pretty-hydra-define hydra-multiple-cursors (:title "Multiple cursors"
                                                      :quit-key "q")
    ("Add to region"
     (("l" mc/edit-lines "Edit lines in region" :exit t)
      ("b" mc/edit-beginnings-of-lines "Edit beginnings of lines in region" :exit t)
      ("e" mc/edit-ends-of-lines "Edit ends of lines in region" :exit t))
     "Mark same word (all)"
     (("a" mc/mark-all-like-this "Mark all like this" :exit t)
      ("A" mc/mark-all-dwim "Mark all dwim")
      ("s" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
      ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
      ("r" mc/mark-all-in-region "Mark all in region" :exit t)
      ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
      ("d" mc/mark-all-symbols-like-this-in-defun "Mark symbols in defun" :exit t))
     "Mark same word (one)"
     (("n" mc/mark-next-like-this "Mark next like this")
      ("N" mc/skip-to-next-like-this "Skip to next like this")
      ("p" mc/mark-previous-like-this "Mark previous like this")
      ("P" mc/skip-to-previous-like-this "Skip to previous like this"))
     "Unmark"
     (("M-n" mc/unmark-next-like-this "Unmark next like this")
      ("M-p" mc/unmark-previous-like-this "Unmark previous like this"))
     "More"
     (("M" mc/mark-more-like-this-extended "Mark like this interactively")
      ("C-n" mc/mark-next-lines "Mark next lines")
      ("C-p" mc/mark-previous-lines "Mark previous lines"))))

  (dolist (command '(mc/mark-next-like-this
                     mc/mark-previous-like-this
                     mc/unmark-next-like-this
                     mc/unmark-previous-like-this
                     mc/mark-all-dwim
                     mc/mark-all-like-this))
    (put command 'repeat-map 'lps/multiple-cursors-repeat-map)))

(use-package emacs
  :ensure nil
  :when lps/only-built-in-p
  :custom
  (completion-styles '(basic partial-completion substring)))

(use-package orderless
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-auto-help t)
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal
                               orderless-regexp))
  (orderless-style-dispatchers '(lps/orderless-initialism-if-semicolon
                                 lps/orderless-substring-if-equal
                                 lps/orderless-flex-if-twiddle
                                 lps/orderless-without-if-bang))

  :config
  ;; From the Orderless package documentation
  (defun lps/orderless-flex-if-twiddle (pattern _index _total)
    "Use `orderless-flex' if the input starts with a ~"
    (if (string-prefix-p "~" pattern)
        `(orderless-flex . ,(substring pattern 1))

      (when (string-suffix-p "~" pattern)
        `(orderless-flex . ,(substring pattern 0 -1)))))

  (defun lps/orderless-substring-if-equal (pattern _index _total)
    "Use `orderless-literal' if the input starts with a ="
    (if (string-prefix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 1))

      (when (string-suffix-p "=" pattern)
        `(orderless-literal . ,(substring pattern 0 -1)))))

  (defun lps/orderless-first-initialism (pattern index _total)
    "Use `orderless-initialism' for the first component"
    (if (= index 0) 'orderless-initialism))

  (defun lps/orderless-initialism-if-semicolon (pattern _index _total)
    "Use `orderless-initialism' if the input starts with a ;"
    (if (string-prefix-p ";" pattern)
        `(orderless-initialism . ,(substring pattern 1))

      (when (string-suffix-p ";" pattern)
        `(orderless-initialism . ,(substring pattern 0 -1)))))

  (defun lps/orderless-without-if-bang (pattern _index _total)
    (cond
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))))

  ;; Fix some bugs with remote filenames
  ;; Taken from Vertico documentation
  (when (featurep 'vertico)
    (defun basic-remote-try-completion (string table pred point)
      (and (vertico--remote-p string)
           (completion-basic-try-completion string table pred point)))

    (defun basic-remote-all-completions (string table pred point)
      (and (vertico--remote-p string)
           (completion-basic-all-completions string table pred point)))

    (add-to-list
     'completion-styles-alist
     '(basic-remote basic-remote-try-completion
                    basic-remote-all-completions
                    nil))

    (setq completion-category-overrides '((file
                                           (styles
                                            basic-remote
                                            partial-completion))))))

;; Company. Auto-completion package
(use-package company
  :diminish
  :init
  (global-company-mode t)
  :hook
  (prog-mode . lps/company-default-backends-prog)
  (text-mode . lps/company-default-backends-text)
  :bind
  ("TAB" . company-indent-or-complete-common)
  (:map company-active-map
        ("<tab>" . company-complete)
        ("TAB" . company-complete)
        ("C-n" . nil)
        ("C-p" . nil)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("M-s" . company-search-candidates))
  (:map company-search-map
        ("C-n" . nil)
        ("C-p" . nil)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous))
  (:map lps/quick-edit-map
        ("SPC" . company-manual-begin))

  :custom
  ;; Generic company settings
  (company-minimum-prefix-length 4)
  (company-idle-delay nil)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-flip-when-above t)
  (company-tooltip-limit 20)
  (company-require-match nil)
  (company-search-regexp-function 'company-search-words-regexp)

  :config
  ;; Don't use orderless for company
  (advice-ensure-bindings company--perform ((completion-styles '(basic
                                                                 partial-completion
                                                                 emacs22))))

  ;; Use our personal default backends
  (defun lps/company-default-backends-prog ()
    (setq-local company-backends '((company-capf company-files company-dabbrev)
                                   (company-dabbrev-code
                                    company-gtags company-etags
                                    company-keywords
                                    company-clang)
                                   company-oddmuse)))

  (defun lps/company-default-backends-text ()
    (setq-local company-backends '((company-capf company-files)
                                   company-oddmuse)))

  ;; AZERTY-friendly company number selection
  ;; Might lead to company-box being a bit broken ? Long function names are cut-off
  (dolist (map (list company-active-map company-search-map))
    (dolist (key-char '((10 . ?à)
                        (1 . ?&)
                        (2 . ?é)
                        (3 . ?\")
                        (4 . ?')
                        (5 . ?\()
                        (6 . ?-)
                        (7 . ?è)
                        (8 . ?_)
                        (9 . ?ç)))
      (define-key map (kbd (format "M-%c" (cdr key-char)))
                  `(lambda () (interactive) (company-complete-number ,(car key-char)))))))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-show-single-candidate 'never)
  :config
  (setq company-box-backends-colors
        '((company-yasnippet :all "dark turquoise"
                             :selected (:background "slate blue"
                                                    :foreground "white")))))

(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :custom
  (company-quickhelp-delay 0.2)
  :config
  ;; Temporary (??) hack: we used HELPFUL to override the built-in help,
  ;; so company quickhelp got confused ...
  (defun-override lps/elisp--company-doc-buffer (str)
    (let ((symbol (intern-soft str)))
      ;; FIXME: we really don't want to "display-buffer and then undo it".
      (save-window-excursion
        ;; Make sure we don't display it in another frame, otherwise
        ;; save-window-excursion won't be able to undo it.
        (let ((display-buffer-overriding-action
               '(nil . ((inhibit-switch-frame . t)))))
          (ignore-errors
            (cond
             ((fboundp symbol) (describe-function symbol))
             ((boundp symbol) (describe-variable symbol))
             ((featurep symbol) (describe-package symbol))
             ((facep symbol) (describe-face symbol))
             (t (signal 'user-error nil)))
            (if (or (derived-mode-p 'help-mode)
                    (derived-mode-p 'helpful-mode))
                (buffer-name))))))))

(use-package company-yasnippet
  :ensure nil
  :after company yasnippet
  :bind
  (:map yas-minor-mode-map
        ("<C-tab>" . lps/company-yasnippet-show-or-complete))
  :config
  (defun lps/company-yasnippet-show-or-complete ()
    (interactive)
    (let ((company-backends '(company-yasnippet)))
      (call-interactively 'company-complete))))

(use-package company-dabbrev
  :ensure nil
  :after company
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-dabbrev-downcase nil)
  (company-dabbrev-minimum-length 4))

(use-package emacs
  :only-built-in nil
  :bind (:map lps/all-hydras-map
              ("m" . hydra-move/body))
  :config
  (defhydra hydra-move ()
    "Movement"                        ; m as in movement
    ("n" next-line)
    ("p" previous-line)
    ("f" forward-char)
    ("b" backward-char)
    ("a" beginning-of-line)
    ("e" move-end-of-line)
    ("v" scroll-up-command)
    ;; Converting M-v to V here by analogy.
    ("V" scroll-down-command)
    ("l" recenter-top-bottom)))

(use-package emacs
  :ensure nil
  :bind
  ("M-n" . forward-paragraph)
  ("M-p" . backward-paragraph)
  (:map visual-line-mode-map
        ("C-S-a" . beginning-of-line)
        ("C-S-e" . end-of-line))
  :custom
  (sentence-end-double-space nil))

(use-package isearch
  :ensure nil
  :only-built-in t
  :bind
  (:map isearch-mode-map
        ("M-." . isearch-forward-thing-at-point))
  (:map search-map
        ("s" . isearch-forward)
        ("M-s" . isearch-forward) ;; avoids early/late release of Meta
        ("r" . isearch-backward)
        ("x" . isearch-forward-regexp))
  :custom
  ;; Interpret whitespaces as "anything but a newline"
  (search-whitespace-regexp "[-\\/_ \\t.]+")
  (isearch-regexp-lax-whitespace t)
  (isearch-yank-on-move 'shift)
  (isearch-allow-scroll t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  :config
  ;; Change this face to distinguish between current match and other ones
  (set-face-foreground 'isearch "yellow")
  (set-face-foreground 'lazy-highlight "yellow3"))

(use-package replace
  :ensure nil
  :only-built-in t
  :bind
  ("C-%" . query-replace-number)
  (:map query-replace-map
        ("RET" . act)
        ("<return>" . act))
  (:map lps/quick-edit-map
        ("%" . replace-string)
        ("C-%" . replace-regexp))
  :config
  (defun query-replace-number (num to-expr &optional delimited start end
                                   backward region-noncontiguous-p)
    (declare (interactive-args
              (start (use-region-beginning))
              (end (use-region-end))
              (region-noncontiguous-p (use-region-noncontiguous-p))))
    (interactive
     (let* ((query-replace-lazy-highlight nil)
            (common
             (query-replace-read-args
              (concat "Query replace"
                      (if (eq current-prefix-arg '-) " backward" "")
                      (if (use-region-p) " in region" "")
                      " numbers")
              t)))
       (list (nth 0 common) ;; num variable
             (query-replace-compile-replacement
              (concat "\\,(let ((" (nth 0 common) " \\#&))"
                      (nth 1 common)
                      ")")
              t)
             (nth 2 common)
             ;; These are done separately here
             ;; so that command-history will record these expressions
             ;; rather than the values they had this time.
             (use-region-beginning) (use-region-end)
             (nth 3 common)
             (use-region-noncontiguous-p))))
    (perform-replace "[+-]?\\([[:digit:]]*\\.\\)?[[:digit:]]+"
                     to-expr t t delimited
                     nil nil start end backward region-noncontiguous-p)))

(use-package avy
  :defer t
  :bind
  ("M-é" . avy-goto-char-2)
  (:map isearch-mode-map
        ("M-é" . avy-isearch))
  :custom
  ;; Using an AZERTY keyboard home row
  (avy-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l ?m))
  (avy-all-windows nil)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.5)
  (avy-translate-char-function '(lambda (c) (if (= c 32) ?q c))))

(use-package emacs
  :only-built-in nil
  :bind
  (:map lps/all-hydras-map
        ("r" . hydra-rectangle/body))
  :config
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                       :color pink
                                       :hint nil
                                       :post (deactivate-mark))
    "
      ^_p_^       _w_ copy      _o_pen       _N_umber-lines                   |\\     -,,,--,,_
    _b_   _f_     _y_ank        _t_ype       _e_xchange-point                 /,`.-'`'   ..  \-;;,_
      ^_n_^       _d_ kill      _c_lear      _r_eset-region-mark             |,4-  ) )_   .;.(  `'-'
    ^^^^          _u_ndo        _q_ quit     _I_nsert-string-rectangle      '---''(./..)-'(_\_)
    "
    ("p" rectangle-previous-line)
    ("n" rectangle-next-line)
    ("b" rectangle-backward-char)
    ("f" rectangle-forward-char)
    ("d" kill-rectangle)                    ;; C-x r k
    ("y" yank-rectangle)                    ;; C-x r y
    ("w" copy-rectangle-as-kill)            ;; C-x r M-w
    ("o" open-rectangle)                    ;; C-x r o
    ("t" string-rectangle)                  ;; C-x r t
    ("c" clear-rectangle)                   ;; C-x r c
    ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
    ("N" rectangle-number-lines)            ;; C-x r N
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1))) ;; C-x SPC
    ("I" string-insert-rectangle)
    ("u" undo nil)
    ("q" nil)))

(use-package emacs
  :after rect
  :bind
  ("C-x r I" . string-insert-rectangle)
  (:map rectangle-mark-mode-map
        ("RET" . rectangle-exchange-point-and-mark)
        ("<C-return>" . cua-rectangle-mark-mode)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region)
  :custom
  (shift-select-mode nil))

(use-package emacs
  :ensure nil
  :init
  (defvar lps/yank-indent-modes '(prog-mode latex-mode))
  :bind
  ("M-k" . lps/copy-line-at-point)
  ("M-à" . lps/mark-line)
  ("<C-backspace>" . delete-region)
  ([remap yank] . lps/yank-indent)
  ([remap kill-region] . lps/kill-region-or-word-dwim)
  :custom
  (kill-read-only-ok t)
  (kill-ring-max 100)
  (kill-do-not-save-duplicates t)
  :config
  (defun lps/copy-line-at-point (arg)
    "Copy lines in the kill ring, starting from the line at point.

If ARG is not specified or equal to 1, do not copy the indentation.

If ARG > 1, copy subsequent lines and indentation."
    (interactive "p")
    (let ((beg (if (equal 1 arg)
                   (save-excursion
                     (back-to-indentation)
                     (point))
                 (line-beginning-position)))
          (end (line-end-position arg)))
      (copy-region-as-kill beg end)))

  (defun lps/mark-line ()
    "Select the current line. If the region is already active, extend
the current selection by line."
    (interactive)
    (if (region-active-p)
        (progn
          (forward-line 1)
          (end-of-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position)))))

  (defun lps/yank-indent (arg)
    (interactive "*P")
    (let ((point (point)))
      (yank arg)
      (when (-some 'derived-mode-p lps/yank-indent-modes)
        (indent-region point (point)))))

  ;; Make `C-w' consistent with readline
  ;;
  ;; Should not change things as you *usually* (personnally, at least)
  ;; don't want to kill an inactive region, the bounds of which are
  ;; unclear.
  (defun lps/kill-region-or-word-dwim ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (backward-kill-word 1))))

(use-package emacs
  :ensure nil
  :bind
  ([remap exchange-point-and-mark] . lps/exchange-point-and-mark)
  :custom
  (set-mark-command-repeat-pop t)
  :init
  ;;Taken from https://spwhitton.name/blog/entry/transient-mark-mode/
  (defun lps/exchange-point-and-mark (arg)
    "Exchange point and mark, but reactivate mark a bit less often.

  Specifically, invert the meaning of ARG in the case where
  Transient Mark mode is on but the region is inactive."
    (interactive "P")
    (exchange-point-and-mark
     (if (and transient-mark-mode (not mark-active))
         (not arg)
       arg))))

(use-package drag-stuff
  :config
  (setq drag-stuff-except-modes '(org-mode
                                  tabulated-list-mode))
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys))

(use-package emacs
  :ensure nil
  :bind
  ("<C-down>" . lps/duplicate-line-or-region-down)
  ("<C-up>" . lps/collapse-line-up)
  :config
  (defun lps/duplicate-line-or-region-down (arg)
    "Duplicate current line or region if active.
Move point in the last duplicated string (line or region)."
    (interactive "*p")
    (if (region-active-p)
        (progn
          (save-excursion
            (let* ((bor (region-beginning))
                   (eor (region-end))
                   (content (buffer-substring bor eor)))
              (goto-char eor)
              (end-of-line) ; necessary if region is inside longer line
              (dotimes (i arg)
                (newline)
                (insert content))))
          (next-line (* arg (count-lines (region-beginning) (region-end)))))

      (save-excursion
        ;; local variables for start and end of line
        (let* ((bol (progn (beginning-of-line) (point)))
               (eol (progn (end-of-line) (point)))
               (line (buffer-substring bol eol)))
          (dotimes (i arg)
            (newline)
            (insert line))))
      (next-logical-line arg)))

  (defun lps/collapse-line-up (arg)
    "Delete the current line and move point on the previous line"
    (interactive "*p")
    (let (final)
      (save-excursion
        (previous-logical-line arg)
        (setq final (point)))
      (kill-whole-line (- arg))
      (goto-char final))))

(use-package undo-tree
  :diminish
  :disabled t
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

(use-package vundo
  :defer t
  :hook (vundo-mode . lps/enable-auto-hscroll-scroll)
  :bind
  ("C-_" . undo)
  ("M-_" . undo-redo)
  ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  (vundo-compact-display nil)
  (vundo-window-max-height 5))

(defun lps/find-delete-forward-all-regexp (re &optional beg)
  "Searches for all the matches of the regexp RE after the point, or after the optional position BEG.
  Returns a list of strings containing the matches in order, or nil if none was found.
  Deletes (rather than kill) those matches from the buffer"
  (save-excursion
    (let (matches)
      (goto-char (or beg (point)))
      (while (re-search-forward re nil t)
        (push (match-string 0) matches)
        (delete-region (match-beginning 0) (match-end 0)))
      matches)))

(defun lps/move-all-regexp-pos-buffer (re &optional beg move split)
  "Moves all the string matching the regexp RE after the point (or after BEG) to the end of the buffer
(or to the position MOVE if provided)
  If SPLIT is provided, it will be inserted before each match, including the first one.
  The initial strings are destroyed, and the kill-ring is not modified"
  (save-excursion
    (let ((matches (nreverse (lps/find-delete-forward-all-regexp re beg))))
      (goto-char (or move (point-max)))
      (while matches
        (insert (or split ""))
        (insert (pop matches))))))

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(use-package align
  :ensure nil
  :bind
  (:map lps/quick-edit-map
        ("C-a a" . align)
        ("C-a e" . align-entire)
        ("C-a x" . align-regexp)
        ("C-a c" . align-current)))

(use-package emacs
  :ensure nil
  :bind-keymap ("C-o" . lps/manipulate-lines-map)
  :bind
  (:map lps/manipulate-lines-map
        ("o" . open-line)
        ("p" . lps/insert-line-above)
        ("n" . lps/insert-line-below)
        ("l" . list-matching-lines)
        ("s" . sort-lines)
        ("r b" . delete-blank-lines)
        ("r d" . delete-matching-lines)
        ("r k" . keep-lines))
  :custom
  (list-matching-lines-default-context-lines 1)
  (list-matching-lines-jump-to-current-line t)
  :config
  (defun lps/insert-line-above (N)
    (interactive "P")
    (save-excursion
      (beginning-of-line)
      (newline-and-indent N)))

  (defun lps/insert-line-below (N)
    (interactive "P")
    (save-excursion
      (end-of-line)
      (newline-and-indent N))))

(use-package emacs
  :ensure nil
  :bind-keymap
  ("C-z" . lps/quick-edit-map)
  :bind
  (:map lps/quick-edit-map
        ("C-u" . lps/underline-or-frame-dwim)
        ("k" . zap-up-to-char)
        ("C-t" . lps/make-filename-from-sentence)
        ("i" . lps/insert-file-name))
  (:map lisp-data-mode-map
        ("M-*" . lps/earmuffify))
  :config
  (defun lps/fill-width-repeat-string (width str)
    "Insert STR as many times as necessary to fill WIDTH,
potentially using only a prefix of STR for the final iteration"
    (let* ((len (length str))
           (k (/ width len))
           (rem (% width len)))
      (dotimes (i k)
        (insert str))
      (insert (substring str 0 rem))))

  (defun lps/underline-or-frame-dwim (str &optional arg)
    "Underlines the current line with the string STR or with `comment-start'
if none is provided. If `comment-start' is NIL, use \"-\" instead.
If called interactively, prompt for STR.
With a prefix argument, frame the line using STR instead of underlining it.
In this case, also insert a blank space before and after the region if none
are present.
Breaks if region or line spans multiple visual lines"
    (interactive (list (let ((default (or comment-start "-")))
                         (read-string (concat "Use string (default " default " ): ")
                                      nil nil default))
                       current-prefix-arg))
    (save-excursion
      (let* ((len (length str))
             (from (if (region-active-p)
                       (region-beginning)
                     (line-beginning-position)))
             (to (if (region-active-p)
                     (region-end)
                   (line-end-position)))
             (col (- from (line-beginning-position)))
             (width (if arg
                        (+ (* 2 len) (- to from))
                      (- to from))))
        (if arg
            (progn
              (goto-char from)
              (insert str)
              (unless (looking-at " ")
                (insert " ")
                (setq width (1+ width))
                (setq to (1+ to)))
              (goto-char (+ len to))
              (unless (looking-at " ")
                (insert " ")
                (setq width (1+ width)))
              (insert str)
              (beginning-of-line)
              (insert "\n")
              (forward-line -1)
              (indent-to col)
              (lps/fill-width-repeat-string width str)
              (forward-line 1)
              (end-of-line)
              (insert "\n")
              (indent-to col)
              (lps/fill-width-repeat-string width str))
          (progn
            (end-of-line)
            (insert "\n")
            (indent-to col)
            (lps/fill-width-repeat-string width str))))))

  (defvar lps/do-not-capitalize-list '("the" "a" "an" "of" "in" "on" "by"
                                       "no" "or" "and" "if" "for" "to" "is"
                                       "as"
                                       "le" "la" "les" "et" "ou"
                                       "si" "un" "une" "de" "des"
                                       "du" "d" "l" "ni"))

  (defun lps/make-filename-from-sentence (&optional replace-spaces)
    "Create a title from the current line or region and add it to the
 kill-ring.
If REPLACE-SPACE is a character, replace spaces with this char.
If it is non-nil, replace it by an underscore _"
    (interactive "P")
    (let* ((bounds (if (region-active-p)
                       (car (region-bounds))
                     (cons (line-beginning-position)
                           (line-end-position))))
           (start (car bounds))
           (end (set-marker (make-marker) (cdr bounds))))
      (goto-char start)
      (capitalize-word 1)
      (while (< (point) (marker-position end))
        (let ((num-spaces (skip-chars-forward "[:punct:][:space:][\n]")))
          (if (> num-spaces 0)
              (progn
                (delete-backward-char num-spaces)
                (cond
                 ((not replace-spaces) (insert " "))
                 ((characterp replace-spaces) (insert replace-spaces))
                 (t (insert "_"))))
            (forward-char 1)))
        (let ((word-at-pt (word-at-point)))
          (if (or (not word-at-pt)
                  (member (downcase word-at-pt)
                          lps/do-not-capitalize-list))
              (downcase-word 1)
            (capitalize-word 1))))
      (kill-ring-save start (point))))

  (defun lps/earmuffify ()
    (interactive)
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (if (not bounds)
          (message "No symbol at point")
        (goto-char (car bounds))
        (unless (= (char-after) ?*)
          (insert ?*))
        (forward-symbol 1)
        (unless (= (char-before) ?*)
          (insert ?*)))))

  (defun lps/insert-file-name (&optional absolute)
    "Prompt for a file, and inserts its name in the current buffer,
relative to the current directory.

If ABSOLUTE is non-nil, inserts the absolute file name instead."
    (interactive "P")
    (let ((file (read-file-name "Insert file: ")))
      (insert (if absolute
                  (expand-file-name file)
                (file-relative-name file))))))

(use-package project
  :custom
  ;; Difficulties with symlinks, which I use a lot
  (project-vc-merge-submodules nil))

(use-package projection
  :hook (after-init . global-projection-hook-mode)
  :bind-keymap
  ("C-x P" . projection-map)
  :bind
  ([remap project-list-buffers] . ibuffer-projection-current-project)
  :config
  ;; Override, I believe it's buggy
  ;; Replaced PROJECTION-FILES with PROJECTION-ROOT
  (defun-override lps/projection-ibuffer--current-project (project)
    "Open an IBuffer window showing only buffers in PROJECT."
    (ibuffer nil (format "*%s Buffers*" (project-name project))
             (list (cons 'projection-root project)))))

(use-package projection-multi)
(use-package projection-multi-embark
  :after (embark projection-multi)
  :config
  (projection-multi-embark-setup-command-map))

(use-package magit
  :defer t
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer
  :bind
  ("C-x g" . magit-status)
  (:map magit-section-mode-map
        ("M-^" . magit-section-up))
  :custom
  (magit-view-git-manual-method 'man) ; can't understand what Gitman is
  (magit-module-sections-nested nil) ; disable if many modules in a given repo
  (magit-clone-always-transient t)
  :config
  (dolist (action '(stage-all-changes unstage-all-changes))
    (add-to-list 'magit-no-confirm action))

  ;; From https://emacs.stackexchange.com/a/43975/31651
  (transient-define-suffix magit-submodule-update-all ()
    "Update all submodules"
    :description "Update All     git submodule update --init --recursive"
    (interactive)
    (magit-with-toplevel
      (magit-run-git-async "submodule" "update" "--init" "--recursive")))

  (transient-append-suffix 'magit-submodule "u"
    '("U" magit-submodule-update-all))

  ;; Insert modules /after/ the other sections
  (add-hook 'magit-status-sections-hook 'magit-insert-modules 1)
  (add-hook 'magit-status-sections-hook 'magit-insert-tracked-files 1))

(use-package transient
  :custom
  (transient-highlight-higher-levels t)
  (transient-default-level 7)
  :config
  ;; Replace the underline with a slanted text, less disturbing
  (set-face-attribute 'transient-higher-level nil
                      :underline nil
                      :italic t))

(use-package git-timemachine
  :defer t)

(use-package forge
  :after magit
  :custom
  (forge-bug-reference-hooks nil))

(use-package smerge-mode
  :defer t
  :hook
  (find-file . lps/smerge-maybe-start)
  :bind-keymap
  ("C-c m" . smerge-basic-map)
  :init
  (defvar smerge-repeat-map
    (let ((map (make-sparse-keymap)))
      (dolist (key-cmd '(("n" smerge-next)
                         ("p" smerge-prev)
                         ("a" smerge-keep-all)
                         ("l" smerge-keep-lower)
                         ("u" smerge-keep-upper)
                         ("b" smerge-keep-base)
                         ("r" smerge-resolve)))
        (let ((key (car key-cmd))
              (cmd (cadr key-cmd)))
          (define-key map (kbd key) cmd)
          (put cmd 'repeat-map 'smerge-repeat-map)))
      map))
  :config
  (defun lps/smerge-maybe-start ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward smerge-begin-re nil t)
          (message "Smerge-mode automatically enabled: there seem to be conflicts !")
          (smerge-mode 1))))))

;; Always highlight matching parenthesis
(use-package paren
  :ensure nil
  :only-built-in t
  :init
  (show-paren-mode t)
  :custom
  ;; (show-paren-style 'mixed) ; Too invasive
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen t))

;; rainbow-delimiters. Hightlights with the same colour matching parenthesis
(use-package rainbow-delimiters
  :hook ((prog-mode comint-mode) . rainbow-delimiters-mode))

(use-package paredit
  :init
  (defun lps/paredit-enable-electric-pair-disable ()
    (paredit-mode 1)
    (electric-pair-local-mode -1))

  :hook ((sly-mrepl-mode
          eshell-mode
          ielm-mode
          eval-expression-minibuffer-setup
          lisp-data-mode
          cider-mode
          cider-repl-mode)
         . lps/paredit-enable-electric-pair-disable)

  :bind
   ;; Not restricted to paredit but useful sexp manipulation
  ("C-M-<backspace>" . backward-kill-sexp) ; why is NOT already there ?!
  ("C-S-t" . transpose-sexps)
  (:map paredit-mode-map
        ("M-?" . nil)
        ("C-S-w" . paredit-copy-as-kill)
        ("M-s" . nil) ;; To get isearch-mode-map
        ("M-s M-s" . paredit-splice-sexp)
        ("C-M-;" . paredit-convolute-sexp)
        ([remap newline] . paredit-newline)
        ("C-j" . nil)
        ("<C-backspace>" . paredit-delete-region)
        ("M-S-<left>" . lps/transpose-sexp-backward)
        ("M-S-<right>" . lps/transpose-sexp-forward))
  :config
  ;; Version 29 or 30 broke something ?!
  ;; Remove paredit broken RET key
  (define-key paredit-mode-map (kbd "RET") nil t)

  (defun lps/transpose-sexp-backward ()
    (interactive)
    (transpose-sexps 1)
    (backward-sexp 2))

  (defun lps/transpose-sexp-forward ()
    (interactive)
    (forward-sexp 1)
    (transpose-sexps 1)
    (backward-sexp 1))

  (defun lps/paredit-no-space-insert-after-sharp-dispatch (endp delimiter)
    "Always return T, unless we are right after a #<form> where form is only made of
characters of WORD syntax
This ensures that no space is inserted after e.g. #2A or #C"
    (not (and (/= (point) (line-beginning-position))
              (= delimiter ?\()
              (not endp)
              (or (looking-back "#\\w+")
                  (looking-back ",@")))))

  (add-to-list 'paredit-space-for-delimiter-predicates
  'lps/paredit-no-space-insert-after-sharp-dispatch))

(use-package elec-pair
  :only-built-in t
  :hook ((prog-mode
          org-mode
          inferior-python-mode)
         . electric-pair-local-mode)) ;; needed for org-babel

(use-package adjust-parens
  :after paredit
  :hook (paredit-mode . adjust-parens-mode)
  :bind
  (:map adjust-parens-mode-map
        ("TAB" . nil)
        ("<backtab>" . nil)
        ("M-<left>" . lps/lisp-dedent-adjust-parens)
        ("M-<right>" . lps/lisp-indent-adjust-parens))
  :config
  (defun lps/lisp-dedent-adjust-parens ()
    (interactive)
    (save-excursion
      (back-to-indentation)
      (call-interactively 'lisp-dedent-adjust-parens)))

  (defun lps/lisp-indent-adjust-parens ()
    (interactive)
    (save-excursion
      (back-to-indentation)
      (call-interactively 'lisp-indent-adjust-parens))))

(use-package emacs
  :ensure nil
  :init
  (defun lps/insert-parentheses (&optional arg)
    "Same as `insert-parentheses' but if no ARG is provided, it wraps
the next s-expression in parentheses rather than inserting () at point
Does not insert a space before the inserted opening parenthesis"
    (interactive "P")
    (let ((parens-require-spaces nil))
      (if arg
          (insert-parentheses arg)
        (insert-parentheses 1))))

  (defun lps/insert-quotes (&optional arg)
    "Same as `lps/insert-parentheses' with quotes \" characters "
    (interactive "P")
    (let ((parens-require-spaces nil))
      (if arg
          (insert-pair arg ?\" ?\")
        (insert-pair 1 ?\" ?\"))))

  :bind
  ([remap insert-parentheses] . lps/insert-parentheses)
  ("M-\"" . lps/insert-quotes))

(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand))

;;YASnippet
(use-package yasnippet
  :diminish
  :init
  (defvar lps/snippets-dir-root (expand-file-name "snippets" user-emacs-directory))
  :custom
  (yas-verbosity 1)
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil))
  :config
  (defun lps/snippets-initialize ()
    "Initialize personnal snippets, so Yasnippet can see them."
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs lps/snippets-dir-root t))
    (yas-load-directory lps/snippets-dir-root))

  (lps/snippets-initialize))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package emacs
  :ensure nil
  :only-built-in nil
  :hook ((python-mode
          c-mode
          c++-mode
          haskell-mode)
         . lps/lsp-by-default-in-session)
  :init
  ;; Abstract away the client used: lsp-mode or eglot
  (defvar lps/language-server-client 'eglot)

  (defun lps/start-language-server ()
    (interactive)
    (call-interactively lps/language-server-client))

  ;; Sometimes, we don't want to start a full server just to check a
  ;; file or make a few edits to it. In my use, this mostly depends
  ;; on the session: In a quick session, I might not want to start a
  ;; server for one or two files, however, once I start using LSP,
  ;; there is no reason not to assume that I also want to use it by
  ;; default for other files in the same session
  (defvar lps/default-lsp-mode -1) ; change to ask for LSP on startup

  (defun lps/lsp-by-default-in-session ()
    (if (> lps/default-lsp-mode 0)
        (lps/start-language-server)
      (if (and (= lps/default-lsp-mode 0)
               (y-or-n-p "Automatically use lsp-mode in the current session ?"))
          (progn
            (setq lps/default-lsp-mode 1)
            (lps/start-language-server)))
      (setq lps/default-lsp-mode -1)))

  (defun lps/toggle-lsp-by-default-in-session ()
    (interactive)
    (setq lps/default-lsp-mode (not lps/default-lsp-mode)))

  ;; Fix documentation: don't want to start a server to view some
  ;; C code in helpful buffers !
  (advice-ensure-bindings helpful-update ((lps/default-lsp-mode -1))))

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :disabled t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostics-provider :flymake)  ; :none if none wanted
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  (lsp-ui-sideline-show-code-actions nil))

;; Might not work, recommended to use package-install instead
;; Dependencies might not be the correct ones

(use-package eglot
  ;;:hook ((python-mode c-mode c++-mode) . eglot-ensure)
  :hook (eglot-managed-mode . lps/eldoc-set-documentation-strategy)
  :ensure nil
  :only-built-in t
  :init
  (defvar lps/eglot-prefix-map (make-sparse-keymap))
  :bind
  (:map lps/eglot-prefix-map
        ("r"   . eglot-rename)
        ("g g" . xref-find-definitions)
        ("g r" . xref-find-references)
        ("g a" . xref-find-apropos)
        ("g d" . eglot-find-declaration)
        ("g i" . eglot-find-implementation)
        ("g t" . eglot-find-typeDefinition)
        ("a" . eglot-code-actions)
        ("h" . eldoc))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider))
  :config
  (define-key eglot-mode-map (kbd "C-c l") lps/eglot-prefix-map))

;; Flycheck
(use-package flymake
  :only-built-in t
  ;; :bind-keymap
  ;; ("C-c !" . flymake-mode-map)
  :bind
  (:map flymake-mode-map
        ("C-c ! c" . flymake-start)
        ("C-c ! l" . flymake-show-buffer-diagnostics)
        ("C-c ! L" . flymake-show-project-diagnostics)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! p" . flymake-goto-prev-error))
  :config
  (defvar flymake-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'flymake-goto-next-error)
      (define-key map (kbd "p") #'flymake-goto-prev-error)
      (define-key map (kbd "l") #'flymake-show-buffer-diagnostics)
      map))
  (dolist (cmd '(flymake-goto-next-error
                 flymake-goto-prev-error
                 flymake-show-buffer-diagnostics))
    (put cmd 'repeat-map 'flymake-repeat-map)))

(use-package emacs
  :ensure nil
  :bind
  (:map prog-mode-map
        ("<f5>" . lps/auto-compile))
  :config
  (defvar lps/auto-compile-command-alist nil
    "Alist containing commands to run to automatically compile the
current file. Elements are of the form (MODE . COMMAND) where
COMMAND is a function or a symbol")

  (defun lps/auto-compile ()
    "If the current major mode is in `lps/auto-compile-command-alist',
call the associated function interactively. Otherwise, call the
`compile' command"
    (interactive)
    (let ((command (or (cdr (assoc major-mode
                                   lps/auto-compile-command-alist))
                       'compile)))
      (call-interactively command))))

(use-package eldoc
  :custom
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (with-eval-after-load 'paredit
    (eldoc-add-command-completions "paredit-"))
  ;; Put that in a function: needed for Eglot ...
  (defun lps/eldoc-set-documentation-strategy ()
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)))

(use-package emacs
  :ensure nil
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-prompt-read-only t)
  (comint-completion-addsuffix nil))

(use-package python
  :defer t
  ;;    :hook (python-mode . lps/run-python)
  :custom
  (python-shell-interpreter "python3")
  :config
  (require 'lsp-pyright)
  (defun lps/run-python ()
    (save-excursion
      (call-interactively 'run-python)))

  (add-to-list 'lps/auto-compile-command-alist
               (cons 'python-mode 'python-shell-send-buffer))

  (push 'company-indent-or-complete-common python-indent-trigger-commands))

(use-package lsp-pyright
  :defer t)

(use-package python-mls
  :disabled t
  :after python
  :config
  (python-mls-setup)

  (defun lps/python-mls-prompt-fix (fun &rest args)
    (when python-mls-mode
      (apply fun args)))

  (advice-add 'python-mls-check-prompt :around 'lps/python-mls-prompt-fix))

;; Tuareg (for OCaml and ML like languages)
(use-package tuareg
  :defer t
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t))

;; C/C++
;; See https://github.com/MaskRay/ccls/wiki/lsp-mode
(use-package ccls
  :defer t
  :config
  (setq ccls-executable (executable-find "ccls")))

(use-package emacs
  :ensure nil
  :hook
  ((c-mode c++-mode) . lps/c-c++-mode-basic-compile-command)
  :config
  (defun lps/c-c++-mode-basic-compile-command ()
    (let* ((buf (file-name-nondirectory (or (buffer-file-name) (buffer-name))))
           (buf-no-ext (file-name-sans-extension buf))
           (c-mode-p (eq major-mode 'c-mode))
           (compiler (if c-mode-p "gcc " "g++ ")))
      (setq-local compile-command (concat compiler
                                          buf
                                          " -o "
                                          buf-no-ext)))))

(use-package disaster
  :defer t
  :bind
  (:map c-mode-base-map
        ("C-c C-d" . disaster)))

(use-package rustic
  :ensure nil
  :defer t
  :custom
  (rustic-popup-commands
   '((?b "build"      build)
     (?f "fmt"        fmt)
     (?r "run"        run)
     (?i "comint-run" comint-run)
     (?c "clippy"     clippy)
     (?o "outdated"   outdated)
     (?e "clean"      clean)
     (?k "check"      check)
     (?t "test"       test)
     (?d "doc"        doc)))

  (rustic-lsp-client 'eglot))

(use-package emacs
  :after hexl
  :bind
  (:map hexl-mode-map
        ("C-c C-r" . lps/readelf))
  :config
  ;; Heavily inspired by https://github.com/abo-abo/elf-mode/blob/master/elf-mode.el
  ;; Same thing, I simply do not want to add extra packages for no reason
  (defvar lps/readelf-command "readelf -a -W %s")
  (defun lps/readelf ()
    (interactive)
    (let ((cur-file (buffer-file-name))
          (elf-buffer (get-buffer-create "*Elf*")))
      (with-current-buffer elf-buffer
        (let ((inhibit-read-only t))
          (insert (shell-command-to-string
                   (format lps/readelf-command cur-file)))
          (set-buffer-modified-p nil)
          (special-mode)))
      (pop-to-buffer elf-buffer))))

(use-package macrostep
  :bind
  (:map lps/quick-edit-map
        ("e" . macrostep-expand)))

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package elmacro
  :defer t)

(use-package emacs
  :ensure nil
  :bind
  ("C-c C-e" . lps/eval-and-replace-last-sexp)
  (:map lps/quick-edit-map
        ("x" . emacs-lisp-macroexpand)
        ("C-r" . lps/print-eval-region)
        ("C-e" . lps/eval-and-replace-last-sexp))

  :config
  (defun lps/eval-and-replace-last-sexp ()
    "Evaluate the last s-expression, and replace it with the result"
    (interactive)
    (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

  (defun lps/print-eval-region (start end)
    (interactive "r")
    (eval-region start end t)))

;; Make sure that sbcl is available on PATH
(use-package sly
  :hook
  (lisp-mode . sly-editing-mode)
  :bind
  (:map sly-mode-map
        ("M-_" . nil)
        ("<f6>" . sly-documentation-lookup))
  (:map sly-doc-map
        ("C-g" . nil)
        ("C-h" . nil)
        ("g" . common-lisp-hyperspec-glossary-term)
        ("h" . sly-documentation-lookup))
  (:map sly-prefix-map
        ("C-p" . nil)
        ("M-p" . sly-pprint-eval-last-expression)
        ("M-i" . sly-inspect-no-eval)
        ("C-i" . consult-imenu)
        ("M-u" . sly-unintern-symbol))
  :custom
  ;; Clisp makes SLY crash ?!
  (inferior-lisp-program "sbcl --dynamic-space-size 4GB --lose-on-corruption")
  (sly-net-coding-system 'utf-8-unix)
  (sly-complete-symbol-function 'sly-flex-completions)
  :config
  (add-to-list 'lps/auto-compile-command-alist
               (cons 'lisp-mode 'sly-compile-and-load-file))

  (setq common-lisp-hyperspec-root
        (concat "file://"
                (expand-file-name "Other/HyperSpec/" (xdg-user-dir "DOCUMENTS"))))

  (define-key sly-prefix-map (kbd "C-v") sly-selector-map)

  (defun lps/sly-company-setup ()
    (setq-local company-prescient-sort-length-enable nil)
    (setq-local company-backends '(company-capf)))

  (defun lps/sly-start-repl ()
    (unless (sly-connected-p)
      (sly)))

  ;; ;; Buggy still ...
  ;; (defun lps/desktop-restore-lisp-file-no-repl (buffer-file-name
  ;;                                               _buffer-name
  ;;                                               _buffer-misc)
  ;;   (let ((sly-editing-mode-hook (remove 'lps/sly-start-repl
  ;;                                        sly-editing-mode-hook)))
  ;;     (desktop-restore-file-buffer buffer-file-name _buffer-name _buffer-misc)))

  ;; (add-to-list 'desktop-buffer-mode-handlers
  ;;              '(lisp-mode . lps/desktop-restore-lisp-file-no-repl)
  ;;              nil 'equal)

  (add-hook 'sly-mode-hook 'lps/sly-company-setup)
  (add-hook 'sly-minibuffer-setup-hook 'paredit-mode)

  ;; Don't use Ido, just use our default
  (defalias 'sly-completing-read completing-read-function)

  ;; View HyperSpec within Emacs using EWW
  (add-to-list 'browse-url-handlers
               '("hyperspec" . eww-browse-url))

  ;; Find package: naming  convention
  (defun lps/lisp-search-buffer-package ()
    (let ((package (sly-search-buffer-package)))
      (when package
        (string-trim-left package "#?:"))))

  (setq sly-find-buffer-package-function 'lps/lisp-search-buffer-package)

  ;; Fast inspection. Might be buggy.
  (defun sly-inspect-no-eval (symbol &optional inspector-name)
    (interactive (list (sly-read-symbol-name " symbol's function: ")
                       (sly-maybe-read-inspector-name)))
    (when (not symbol)
      (error "No symbol given"))
    (sly-eval-for-inspector `(slynk:init-inspector ,(concat "'" symbol))
                            :inspector-name inspector-name))

  ;; Pop debugger *below* current window.
  ;; Intended to have a setup like this:
  ;; +------+------+
  ;; |      |      |
  ;; |      | repl |
  ;; |      |      |
  ;; | file +------+
  ;; |      |      |
  ;; |      |  db  |
  ;; |      |      |
  ;; +------+------+
  ;;
  ;; When REPL triggers an error, pop create debugger below it
  ;; Otherwise, pop to the window

  ;; Bugged for now
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*sly-db" . ((display-buffer-reuse-mode-window
  ;;                              display-buffer-below-selected)
  ;;                             . ((inhibit-same-window . nil)
  ;;                                (mode . sly-db-mode)))))
  )

(use-package sly-mrepl
  :ensure nil
  :after sly
  :hook
  (sly-mrepl . lps/sly-setup)
  :bind
  (:map sly-mrepl-mode-map
        ("C-c C-n" . sly-mrepl-next-prompt)
        ("C-c C-p" . sly-mrepl-previous-prompt)
        ("C-c C-q" . sly-quit-lisp)
        ("RET" . lps/sly-mrepl-ret)
        ("C-RET" . sly-mrepl-return)
        ("C-<return>" . sly-mrepl-return))
  (:map sly-selector-map
        ("C-v" . lps/sly-mrepl-other-window))
  (:map sly-mode-map
        ([remap sly-mrepl] . lps/sly-mrepl-other-window))
  :config
  (defun lps/sly-mrepl-other-window ()
    (interactive)
    (require 'sly-mrepl)
    (lps/sly-start-repl)
    (sly-mrepl #'pop-to-buffer))

  ;; Redefinition: do not pop-up or show the MREPL buffer
  ;; Also, a few hacks to center the window properly
  (defun sly-mrepl-on-connection ()
    (let* ((inferior-buffer
            (and (sly-process) (process-buffer (sly-process))))
           (inferior-window
            (and inferior-buffer (get-buffer-window inferior-buffer t))))
      (let ((sly-mrepl-pop-sylvester
             (or (eq sly-mrepl-pop-sylvester 'on-connection)
                 sly-mrepl-pop-sylvester)))
        (sly-mrepl (lambda (buffer)
                     (with-current-buffer buffer
                       (goto-char (point-min))
                       (recenter-top-bottom)
                       (goto-char (point-max))
                       (other-window -1)))))
      (when inferior-window
        (bury-buffer inferior-buffer)
        (delete-window inferior-window))
      (goto-char (point-max))))

  ;; Change behaviour of the <return> key in the REPL
  (defun lps/sly-mrepl-ret ()
    (interactive)
    (if (looking-at "[:space:]*\\'")
        (sly-mrepl-return)
      (paredit-newline)))

  ;; Allow paredit to scroll to bottom on input when insert a parenthesis
  (defun lps/sly-mrepl-paredit-open-scroll-to-bottom (&rest args)
    "Fix to also scroll to the bottom of the SLY REPL when inserting a parenthesis.
  This is needed, as `comint-preinput-scroll-to-bottom' does not
  recognize `paredit-open-round' as a command susceptible to
  trigger the scrolling."
    (if (and (derived-mode-p major-mode 'comint-mode)
             comint-scroll-to-bottom-on-input)
        (let* ((current (current-buffer))
               (process (get-buffer-process current))
               (scroll comint-scroll-to-bottom-on-input))
          (when (and process (< (point) (process-mark process)))
            (if (eq scroll 'this)
                (goto-char (point-max))
              (walk-windows
               (lambda (window)
                 (if (and (eq (window-buffer window) current)
                          (or (eq scroll t) (eq scroll 'all)))
                     (with-selected-window window
                       (goto-char (point-max)))))
               nil t))))))

  (advice-add 'paredit-open-round :before 'lps/sly-mrepl-paredit-open-scroll-to-bottom)

  ;; Setup various variables & pop to previous buffer:
  ;; Save-excursion does not work, as (sly) connects in
  ;; an asynchronous manner, so it returns before the REPL
  ;; is actually setup.
  (defun lps/sly-setup ()
    (lps/sly-company-setup)
    ;; Why does SLY disable it ???
    (setq-local comint-scroll-to-bottom-on-input t)
    ;; (sly-switch-to-most-recent 'lisp-mode)
    ))

(use-package sly-stickers
  :ensure nil
  :after sly
  :bind
  (:map sly-stickers-mode-map
        ("C-c C-s C-t" . sly-stickers-toggle-break-on-stickers)))

(use-package sly-quicklisp
  :after sly
  :hook (sly . sly-quicklisp-mode)
  :bind
  (:map sly-prefix-map
        ("C-d C-r" . sly-register-local-projects))
  :config
  ;; Redefine the sly-quickload function to also list local projects
  (defun sly-quickload (system)
    "Interactive command made available in lisp-editing files."
    (interactive
     (list (completing-read "QL system? "
                            (append
                             (sly-eval
                              '(ql:list-local-systems))
                             (sly-eval
                              '(slynk-quicklisp:available-system-names)))
                            nil
                            nil)))
    (sly-eval-async `(slynk-quicklisp:quickload ,system)
      (lambda (retval)
        (setq sly-quicklisp--enabled-dists retval)
        (sly-message "%s is ready to use!" system)))
    (sly-message "ql:quickloading %s..." system))

  ;; Redefine the modeline construct: takes too much space
  (defun sly-quicklisp--mode-line-construct ()
  "A little pretty indicator in the mode-line"
  `(:propertize ,(cond (sly-quicklisp--enabled-dists
                        (format "QL(%s)" (length sly-quicklisp--enabled-dists)))
                       (sly-quicklisp-mode
                        "QL")
                       (t
                        "-"))
                face sly-quicklisp-indicator-face
                mouse-face mode-line-highlight
                help-echo ,(if sly-quicklisp--enabled-dists
                               (format "Enabled dists %s"
                                       sly-quicklisp--enabled-dists)
                             "NO QL dists reported so far. Load a system using `sly-quickload'")))

  (defun sly-register-local-projects ()
    (interactive)
    (sly-eval-async '(ql:register-local-projects)
      (lambda (retval)
        (sly-message "ql:register-local-projects completed")))))

(use-package sly-macrostep
  :after sly)

(use-package sly-repl-ansi-color
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color)

  (defun lps/sly-colour-lisp-output (string)
    (with-temp-buffer
      (insert string)
      ;; Set Warning/Error in red
      (goto-char (point-min))
      (while (re-search-forward "^.*\\(WARNING\\|ERROR\\).*" nil t)
        (replace-match (format "%c[3;91m\\&%c[0m" 27 27) t nil))
      ;; Replace comments in purple
      (goto-char (point-min))
      (while (re-search-forward "^;.*" nil t)
        (replace-match (format "%c[3;95m\\&%c[0m" 27 27) t nil))
      (buffer-string)))

  (add-hook 'sly-mrepl-hook
   (lambda ()
     (add-hook 'sly-mrepl-output-filter-functions 'lps/sly-colour-lisp-output))))

(use-package sly-asdf
  :init
  (add-to-list 'sly-contribs 'sly-asdf 'append)
  :bind
  (:map sly-prefix-map
        ("M-s" . sly-asdf-isearch-system)
        ("M-%" . sly-asdf-query-replace-system))
  :config
  ;; Bug in the initial implementation, runs isearch instead of multi-isearch ?
  ;; Initial implementation also reinvents the wheel -> multi-isearch-files already exists
  (defun sly-asdf-isearch-system (sys-name)
    "Run function `multi-isearch-files' on the files of an ASDF system SYS-NAME."
    (interactive (list (sly-asdf-read-system-name nil nil)))
    (let ((files (mapcar 'sly-from-lisp-filename
                         (sly-eval `(slynk-asdf:asdf-system-files ,sys-name)))))
      (multi-isearch-files files))))

(use-package common-lisp-snippets
  :after yasnippet sly
  :config
  (let ((modifier "M-"))
    (dolist (bind '(("L" . "lambda")))
      (define-key sly-editing-mode-map (kbd (concat modifier (car bind)))
        (lambda ()
          (interactive)
          (yas-expand-snippet (yas-lookup-snippet (cdr bind))))))))

(use-package cider
  :defer t)

(use-package haskell-mode
  :defer t
  :mode
  ("\\.hs\\'" . haskell-mode)
  ("\\.lhs\\'" . haskell-mode)
  :hook
  (haskell-mode . haskell-collapse-mode)
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . hindent-mode)
  (haskell-mode . haskell-indentation-mode)
  :custom
  (haskell-process-suggest-remove-import-lines t)  ; warnings for redundant imports etc
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-show-overlays nil)) ; redundant with flycheck

(use-package haskell-snippets
  :after yasnippet haskell-mode)

(use-package lsp-haskell
  :defer t
  :custom
  (lsp-haskell-server-path "~/.ghcup/bin/haskell-language-server-wrapper"))

(use-package js2-mode
  :defer t
  :mode
  (("\\.js\\'" . js2-mode))
  :custom
  (js2-include-node-externs t))

(use-package tide
  :hook
  (js2-mode . lps/setup-tide-mode)
  (tide . lps/add-tide-hooks)
  :config
  (defun lps/setup-tide-mode ()
    "Set up Tide mode."
    (interactive)
    (tide-setup)
    (flymake-mode 1)
    (tide-hl-identifier-mode 1)
    (if (and tide-completion-setup-company-backend
             (not (or (eq 'company-tide (car company-backends))
                      (member 'company-tide (car company-backends)))))
        (setq-local company-backends (list (cons 'company-tide
                                                 (car company-backends))))))

  (defun lps/add-tide-hooks ()
    (add-hook 'before-save-hook #'tide-format-before-save nil t)))

(use-package web-mode
  :defer t
  :mode
  (("\\.html?" . web-mode)
   ("\\.css" . web-mode))
  :hook
  (web-mode . rainbow-mode))

(use-package emmmet-mode
  :defer t
  :disabled t
  :hook
  ((sgml-mode css-mode web-mode) . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t))

(use-package eglot-java
  :after eglot
  :hook (java-mode . eglot-java-mode))

(use-package gdb-mi
  :ensure nil
  :defer t
  :hook (gdb-mode . gdb-many-windows))

(use-package dape
  :defer t
  :custom
  (dape-buffer-window-arrangement 'gud)
  :config
  (add-hook 'dape-on-stopped-hooks 'dape-info)
  (add-hook 'dape-on-stopped-hooks 'dape-repl))

(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-mode)
  :custom
  (antlr-tool-command "java org.antlr.v4.Tool")
  (antlr-ask-about-save nil))

(use-package sage-shell-mode
  :hook
  ((sage-shell-mode sage-shell:sage-mode) . lps/sage-shell-hooks)
  :config
  ;; Be careful: if you use the (now somewhat obsolete) `sage-mode',
  ;; this might break things and call the wrong functions
  (sage-shell:define-alias)
  ;; Sage is its own shell, no need to open a Python shell too !
  (defun lps/sage-shell-hooks ()
    (eldoc-mode 1)
    (setq-local python-mode-hook (remove 'lps/run-python python-mode-hook))))

(use-package graphviz-dot-mode
  :mode ("\\.dot\\'" . graphviz-dot-mode)
  :init
  (with-eval-after-load 'org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")
   ;; '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")
   ;; '("◉" "○" "●" "○" "●" "○" "●")
   ))

(use-package org
  :only-built-in t
  :defer t
  :hook (org-mode . lps/org-mode-setup)
  :bind
  (:map org-mode-map
        ("<C-S-return>" . org-insert-subheading)
        ("<C-S-left>" . nil)
        ("<C-S-right>" . nil)
        ("<C-S-up>" . nil)
        ("<C-S-down>" . nil)
        ("C-," . nil)
        ("C-a" . org-beginning-of-line)
        ("C-e" . org-end-of-line)
        ([remap org-insert-structure-template] . 'lps/org-insert-structure-template))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit))
  (:map org-cdlatex-mode-map
        ("°" . cdlatex-math-symbol)
        ("C-°" . cdlatex-math-modify)
        ("'" . nil)
        ("`" . nil)
        ("$" . TeX-insert-dollar))    ; might break things ? Not here by default
  :init
  ;; Have to do this early
  (setq org-directory (expand-file-name "OrgFiles/" (xdg-user-dir "DOCUMENTS")))
  (defun lps/org-expand-file-name (name &optional as-directory)
    (let ((file-or-dir (expand-file-name name org-directory)))
      (if as-directory
          (file-name-as-directory file-or-dir)
        file-or-dir)))

  :custom
  ;; Coding in blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-use-speed-commands t)
  (org-list-allow-alphabetical t)
  (org-blank-before-new-entry '((heading . t)
                                (plain-list-item . auto)))
  (org-special-ctrl-a/e t) ;; With visual-line-mode, need to bind C-a/C-e too
  (org-return-follows-link t)
  (org-imenu-depth 4)
  (org-catch-invisible-edits 'show)
  (org-latex-packages-alist '(("" "amsfonts" t) ("" "lps-tex-style" t)))
  (org-preview-latex-default-process (if (executable-find "dvisvgm")
                                         'dvisvgm
                                       'dvipng))
  (org-format-latex-options (list
                             :foreground 'default
                             :background (if (executable-find "dvisvgm")
                                             "Transparent"
                                           'default)
                             :scale (if (executable-find "dvisvgm")
                                        1.25
                                      1.5)
                             :html-foreground "Black"
                             :html-background "Transparent"
                             :html-scale 1.0
                             :matchers '("begin" "$1"
                                         "$" "$$"
                                         "\\(" "\\[")))
  (org-priority-highest ?A)
  (org-priority-lowest ?E)
  (org-priority-default ?C)
  (org-archive-subtree-save-file-p t)
  (org-ellipsis " ▾")
  :config
  ;; Mouse support
  (require 'org-mouse)

  ;; Basic fonts and faces
  (defun lps/org-font-setup ()
    ;; Set faces for heading levels
    ;; For non-headers: org-default
    (dolist (face '((org-level-1 . 1.1)
                    (org-level-2 . 1.08)
                    (org-level-3 . 1.06)
                    (org-level-4 . 1.04)
                    (org-level-5 . 1.02)
                    (org-level-6 . 1.0)
                    (org-level-7 . 1.0)
                    (org-level-8 . 1.0)))
      (set-face-attribute (car face) nil :weight 'regular :height (cdr face) :inherit 'fixed-pitch))

    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (dolist (face '((org-block fixed-pitch :foreground unspecified :extend t)
                    (org-block-begin-line italic :foreground "dark gray" :background "#1d1d2b" :inherit fixed-pitch :height 1.0)
                    (org-code (shadow fixed-pitch))
                    (org-table (shadow fixed-pitch))
                    (org-verbatim (shadow fixed-pitch))
                    (org-special-keyword (font-lock-comment-face fixed-pitch))
                    (org-meta-line (font-lock-comment-face fixed-pitch))
                    (org-checkbox fixed-pitch)
                    (org-formula fixed-pitch)
                    (org-link (link fixed-pitch))))
      (cl-destructuring-bind (name inherit &rest args) face
        (apply 'set-face-attribute name nil :inherit inherit args))))

  (advice-add 'org-read-date :around 'lps/windmove-mode-local-off-around)

  (defun lps/org-mode-setup ()
    (lps/org-font-setup)
    (org-indent-mode 1)
    ;; (variable-pitch-mode 1)
    (visual-line-mode 1)
    (lps/windmove-mode-local-off)
    (org-cdlatex-mode 1)
    (setq-local electric-pair-inhibit-predicate
                'lps/electric-pair-inhibit-predicate-org))

  ;; Improve org edition
  (defun lps/electric-pair-inhibit-predicate-org (char)
    (or (char-equal char ?<)
        (electric-pair-default-inhibit char)))

  ;; Improve the input of emphasis marker: auto surround region
  ;; Adapted from https://github.com/alphapapa/unpackaged.el
  ;; If region is already marked, remove markers instead of doubling them
  (defmacro lps/def-org-maybe-surround (&rest keys)
    "Define and bind interactive commands for each of KEYS that surround the region or insert text.
  Commands are bound in `org-mode-map' to each of KEYS.  If the
  region is active, commands surround it with the key character,
  otherwise call `org-self-insert-command'."
    `(progn
       ,@(cl-loop for key in keys
                  for name = (intern (concat "lps/org-maybe-surround-" key))
                  for docstring =
                  (format "If region is active, surround it with \"%s\".
  Otherwise call `org-self-insert-command'." key)
                  collect `(defun ,name ()
                             ,docstring
                             (interactive)
                             (if (region-active-p)
                                 (let* ((beg (region-beginning))
                                        (end (region-end))
                                        (char-beg (char-after beg))
                                        (char-end (char-before end)))
                                   (if (and (string= ,key (string char-beg))
                                            (string= ,key (string char-end)))
                                       (let ((delete-active-region nil))
                                         (save-excursion
                                           (goto-char end)
                                           (delete-backward-char 1)
                                           (goto-char beg)
                                           (delete-forward-char 1)))
                                     (save-excursion
                                       (goto-char end)
                                       (insert ,key)
                                       (goto-char beg)
                                       (insert ,key))))
                               (call-interactively #'org-self-insert-command)))
                  collect `(define-key org-mode-map (kbd ,key) #',name))))

  (lps/def-org-maybe-surround "~" "=" "*" "/" "+")

  ;; Babel configuration
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (latex . t)
     (lisp . t)))

  ;; (setq org-confirm-babel-evaluate nil) ; Take care if executing someone
                                           ; else code

  (ensure-version org 9.2
    ;; This is needed as of Org 9.2
    (require 'org-tempo)
    (let ((bound-key-templates
           (mapcar #'car org-structure-template-alist)))
      (dolist (key-template '(;; Programming
                              ("sh" . "src shell")
                              ("el" . "src emacs-lisp")
                              ("py" . "src python")
                              ("latex" . "src latex")
                              ("cl" . "src lisp")
                              ;; Math stuff
                              ("lt" . "theorem")
                              ("ld" . "definition")
                              ("lc" . "corollary")
                              ("le" . "examples")
                              ("ll" . "lemma")
                              ("lr" . "remark")))

        (unless
            (member (car key-template) bound-key-templates)
          (push key-template org-structure-template-alist)))))

  ;; Math stuff
  (dolist (name '("definition"
                  "examples"
                  "theorem"
                  "lemma"
                  "corollary"
                  "remark"))
    (add-to-list 'org-protecting-blocks name nil 'string-equal))

  (add-hook 'org-babel-post-tangle-hook 'delete-trailing-whitespace)

  (defun lps/org-insert-structure-template ()
    (interactive)
    (call-interactively 'org-insert-structure-template)
    (call-interactively 'org-edit-special)))

(use-package ox-html
  :after org
  :ensure nil
  :custom
  (org-html-postamble nil)
  ;; A way to have the default face and background in HTML too ! By
  ;; default, the "HTML fontification" step is done by looking at the
  ;; text properties of each character in a buffer, but the default face
  ;; is "without properties", in a sense.
  (org-html-head (mapconcat 'identity
                            (list "<style>"
                                  "  pre {"
                                  (format "    background-color: %s;"
                                          (face-background 'org-block))
                                  (format "    color: %s;"
                                          (face-foreground 'default))
                                  "  }"
                                  "</style>")
                            "\n"))
  :config
  ;;; Abbreviations
  ;;; Random code to automatically relace some words with an abbreviation during
  ;;; the export process.

  ;; TODO:
  ;; Add options to toggle case sensitivity ?
  ;;
  ;; Either hard-code list of contexts to preserve (src-block, verbatim ...),
  ;; or find this list somewhere ? It probably exists in some form ...
  (defvar lps/org-export-abbr-contexts-no-replace '(keyword
                                                    src-block
                                                    verbatim)
    "List of Org elements in which abbreviations should not be replaced")

  (defvar lps/org-export-abbr-global-abbrevs nil
    "List of abbreviations to replace in every exported file.

  An abbreviation is a cons cell (SHORT . LONG-DESCRIPTION), both
  SHORT and LONG-DESCRIPTION being strings.

  Occurrences of SHORT will be replaced by a corresponding <abbr>
  HTML element.

  SHORT needs not be a single word, but will only be matched at
  word boundaries (so for example, if SHORT is \"some text\", it
  will not be matched in \"My awesome text !\")

  SHORT can also be a regular expression.

  If an abbreviation is defined in a file and is also present in
  this list, the local version supersedes the global one.

  The matches are case-sensitive. To perform case-insensitive
  replacement, set `case-fold-search' to `t'.")

  (defun lps/org-parse-abbr-definition (abbr)
    (string-match "\\[\\(.+\\)\\][ \t]*\\(.*\\)" abbr)
    (let ((short (match-string 1 abbr))
          (description (match-string 2 abbr)))
      (cons short description)))

  (defun lps/org-export-replace-abbr (backend)
    "Replace all the abbreviations with an <abbr> HTML element.

  Abbreviations are defined using the ABBR keyword, with the following syntax:

  #+ABBR: [short-text-1] long-description-1
  #+ABBR: [short-text-2] long-description-2

  with one abbreviation per line. In the buffer, occurrences of the
  abbreviated texts are matched using a simple regexp search, see
  `lps/org-export-abbr-global-abbrevs' for a full description.

  For parsing reasons, LONG-DESCRIPTION should not contain the
  character ], even if it is escaped.

  This works in a single pass over the buffer, and should not
  perform recursive expansion: for example, if an abbreviation has
  a description containing a word that is also an abbreviation, it
  should not be expanded."
    (when (equal backend 'html)
      (let* ((abbrevs-raw (cdar (org-collect-keywords '("abbr"))))
             (abbrevs-local (mapcar 'lps/org-parse-abbr-definition abbrevs-raw))
             (abbrevs (append abbrevs-local lps/org-export-abbr-global-abbrevs))
             (abbrevs-regexp (concat "\\b\\(?:" ; regexp matching any abbr
                                     (mapconcat #'car abbrevs "\\|")
                                     "\\)\\b")))
        (save-excursion
          (beginning-of-buffer)
          ;; Match any of the abbr in the buffer, single pass
          (while (re-search-forward abbrevs-regexp nil t)
            ;; Check context: do nothing if in a "bad context"
            (unless (save-match-data
                      (member (car (org-element-at-point))
                              lps/org-export-abbr-contexts-no-replace))
              ;; Get the actual matched string, and its associated description
              (let* ((abbr-matched (match-string 0))
                     (description (cl-loop for (from . to) in abbrevs
                                           when (save-match-data
                                                  (string-match from abbr-matched))
                                           return to)))
                ;; Replace the match with inline HTML. The matched string has
                ;; the same case, the description is fixed.
                (replace-match (concat "@@html:<abbr title=\""
                                       description
                                       "\">"
                                       abbr-matched
                                       "</abbr>@@")))))))))

  (add-hook 'org-export-before-parsing-hook 'lps/org-export-replace-abbr)
  ;;; Useful ID nodes
  (define-minor-mode lps/org-export-html-with-useful-ids-mode
    "Attempt to export Org as HTML with useful link IDs.
  Instead of random IDs like \"#orga1b2c3\", use heading titles,
  made unique when necessary."
    :global t
    (if lps/org-export-html-with-useful-ids-mode
        (advice-add #'org-export-get-reference
                    :override #'lps/org-export-get-reference)
      (advice-remove #'org-export-get-reference #'lps/org-export-get-reference)))

  (defun lps/org-export-get-reference (datum info)
    "Like `org-export-get-reference', except uses heading titles
  instead of random numbers."
    (let ((cache (plist-get info :internal-references)))
      (or (car (rassq datum cache))
          (let* ((crossrefs (plist-get info :crossrefs))
                 (cells (org-export-search-cells datum))
                 ;; Preserve any pre-existing association between
                 ;; a search cell and a reference, i.e., when some
                 ;; previously published document referenced a location
                 ;; within current file (see
                 ;; `org-publish-resolve-external-link').
                 ;;
                 ;; However, there is no guarantee that search cells are
                 ;; unique, e.g., there might be duplicate custom ID or
                 ;; two headings with the same title in the file.
                 ;;
                 ;; As a consequence, before re-using any reference to
                 ;; an element or object, we check that it doesn't refer
                 ;; to a previous element or object.
                 (new (or (cl-some
                           (lambda (cell)
                             (let ((stored (cdr (assoc cell crossrefs))))
                               (when stored
                                 (let ((old (org-export-format-reference stored)))
                                   (and (not (assoc old cache)) stored)))))
                           cells)
                          (when (org-element-property :raw-value datum)
                            ;; Heading with a title
                            (lps/org-export-new-title-reference datum cache))
                          ;; NOTE: This probably breaks some Org Export
                          ;; feature, but if it does what I need, fine.
                          (org-export-format-reference
                           (org-export-new-reference cache))))
                 (reference-string new))
            ;; Cache contains both data already associated to
            ;; a reference and in-use internal references, so as to make
            ;; unique references.
            (dolist (cell cells) (push (cons cell new) cache))
            ;; Retain a direct association between reference string and
            ;; DATUM since (1) not every object or element can be given
            ;; a search cell (2) it permits quick lookup.
            (push (cons reference-string datum) cache)
            (plist-put info :internal-references cache)
            reference-string))))

  (defun lps/org-export-new-title-reference (datum cache)
    "Return new reference for DATUM that is unique in CACHE."
    (cl-macrolet ((inc-suffixf (place)
                    `(progn
                       (string-match (rx bos
                                         (minimal-match (group (1+ anything)))
                                         (optional "--" (group (1+ digit)))
                                         eos)
                                     ,place)
                       ;; HACK: `s1' instead of a gensym.
                       (-let* (((s1 suffix) (list (match-string 1 ,place)
                                                  (match-string 2 ,place)))
                               (suffix (if suffix
                                           (string-to-number suffix)
                                         0)))
                         (setf ,place (format "%s--%s" s1 (cl-incf suffix)))))))
      (let* ((title (org-element-property :raw-value datum))
             (ref (url-hexify-string (substring-no-properties title)))
             (parent (org-element-property :parent datum)))
        (while (--any (equal ref (car it))
                      cache)
          ;; Title not unique: make it so.
          (if parent
              ;; Append ancestor title.
              (setf title (concat (org-element-property :raw-value parent)
                                  "--" title)
                    ref (url-hexify-string (substring-no-properties title))
                    parent (org-element-property :parent parent))
            ;; No more ancestors: add and increment a number.
            (inc-suffixf ref)))
        ref)))

  (lps/org-export-html-with-useful-ids-mode 1))

(use-package org-agenda
  :after org
  :ensure nil
  :hook (org-agenda-mode . lps/windmove-mode-local-off)
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files
   (list (lps/org-expand-file-name "agenda" t)  ; basic agenda files
         (lps/org-expand-file-name "archive" t) ; archived files
         ;; org-roam-directory, not necessarily loaded
         ;; at this point: don't want to have org-agenda
         ;; depend on org-roam
         (lps/org-expand-file-name "RoamNotes" t)
         ;; Now for articles-notes: somewhat bad to hardcode ...
         (lps/org-expand-file-name "RoamNotes/articles-notes" t)))
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-archive-location (concat (lps/org-expand-file-name "archive" t)
                                "%s_archive::")) ; not technically
                                        ; part of org-agenda
  (org-agenda-start-with-log-mode t)
  (org-agenda-show-inherited-tags nil)
  ;; Window configuration
  (org-agenda-restore-windows-after-quit t)
  (org-agenda-window-setup 'other-window)
  (org-tag-alist
   '((:startgroup)
     ;; Put mutually exclusive tags here
     (:endgroup)
     ("@home" . ?H)
     ("@work" . ?W)
     ("agenda" . ?a)
     ("plan" . ?p)
     ("note" . ?n)
     ("idea" . ?i)
     ("read" . ?r)))

  (org-todo-keywords
   '((sequence "TODO(t)"
               "NEXT(n)"
               "|"
               "STOP(s)"
               "DONE(d)")
     (sequence "READ(r)" "IDEA(i)" "|" "OVER(o)")))

  (org-todo-keyword-faces
   '(("IDEA" . (:foreground "purple1" :weight bold))
     ("READ" . (:foreground "orchid3" :weight bold))
     ("NEXT" . (:foreground "orange1" :weight bold))))

  (org-agenda-breadcrumbs-separator "/")
  (org-agenda-format-date 'lps/org-agenda-format-date)
  (org-agenda-prefix-format
   '((agenda . " %i %(lps/agenda-category 15)%?-15t%b% s")
     (todo . " %i %(lps/agenda-category 15) ")
     (tags . " %i %(lps/agenda-category 15) ")
     (search . " %i %(lps/agenda-category 15)")))

  :config
  ;; Faces
  (set-face-attribute 'org-agenda-date nil :italic t :underline nil)
  (set-face-attribute 'org-agenda-date-today nil :italic t :underline nil)
  (set-face-attribute 'org-agenda-date-weekend nil :italic t :underline nil)

  ;; Maybe don't hardcode width and refer to some kind of window/buffer width ?
  (defun lps/org-agenda-format-date (date)
    (let ((old (org-agenda-format-date-aligned date)))
      (format (concat (make-string 40 ?-) "\n%s") old)))

  ;; Icons and categories
  (dolist (tag-and-icon `(("Lectures"     . "🏫")
                          ("Conference"   . "👥")
                          ("Talk"         . "🔊")
                          ("Exam"         . "💯")
                          ("Seminar"      . "🪧")
                          ("Workshop"     . "👥") ; same icon as conference
                          ("Culture"      . "🎨")
                          ("PhDResearch"  . "🎓")
                          ("Holidays"     . "☀️")
                          ("Science"      . "👩🏾‍🔬")
                          ("Banque"       . "💰")
                          ("Informatique" . "💻")
                          ("Logement"     . "🏠️")
                          ("Santé"        . "⚕️")
                          ("Social"       . "🎉")
                          ("Books"        . "📚")
                          ("Movies"       . "🎥")
                          ("Workplace"    . "🏢")
                          ("Others"       . "❓")))
    (cl-pushnew (list (car tag-and-icon)
                      (list (substring-no-properties (cdr tag-and-icon)))
                      nil nil
                      :ascent 'center)
                org-agenda-category-icon-alist
                :test 'equal))

  ;; Taken from
  ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
  (defun lps/agenda-category (&optional len)
    "Get category of item at point for agenda.

Category is defined by one of the following items:

- CATEGORY property
- TITLE keyword
- TITLE property
- filename without directory and extension

When LEN is a number, resulting string is padded right with
spaces and then truncated with ... on the right if result is
longer than LEN.

Usage example:

  (setq org-agenda-prefix-format
        '((agenda . \" %(lps/agenda-category) %?-12t %12s\")))

Refer to `org-agenda-prefix-format' for more information."
    (let* ((file-name (when buffer-file-name
                        (file-name-sans-extension
                         (file-name-nondirectory buffer-file-name))))
           (title (org-with-point-at 1
                    (when (re-search-forward (concat "^#\\+" "title" ": \\(.*\\)")
                                             (point-max) t)
                      (buffer-substring-no-properties
                       (match-beginning 1)
                       (match-end 1)))))
           (category (org-get-category))
           (result
            (or (if (and
                     title
                     (string-equal category file-name))
                    title
                  category)
                "")))
      (if (numberp len)
          (s-truncate len (s-pad-right len " " result))
        result)))

  (dolist (cmd '(("p" . "PhD related views")
                 ("pp" "PhD (everything)"
                  ((agenda) (tags-todo "phd"))
                  ((org-agenda-tag-filter-preset '("+phd"))))
                 ("pe" "PhD excluded"
                  ((agenda) (tags-todo "-phd"))
                  ((org-agenda-tag-filter-preset '("-phd"))))))
    (cl-pushnew cmd
                org-agenda-custom-commands
                :test 'equal)))

(use-package org-capture
  :after org
  :ensure nil
  :init
  (defun lps/org-capture-make-generic-timestamp-template (abbrev name file &optional extra)
    `(,abbrev
      ,name entry
      (file+function ,(lps/org-expand-file-name file)
                     lps/org-ask-location)
      ,(concat "* %?\n%^t" (or extra "\n"))
      :empty-lines 1))

  :bind
  ("C-c o" . org-capture)
  (:map org-capture-mode-map
        ([remap save-buffer] . org-capture-finalize))
  :custom
  (org-capture-templates
   `(("t" "Tasks")
     ("tt" "Task" entry
      (file+olp ,(lps/org-expand-file-name "agenda/Tasks.org") "Inbox")
      "* TODO %?\n  %U\n  %a\n  %i"
      :empty-lines 1)

     ("ts" "Science Todo" entry
      (file+olp ,(lps/org-expand-file-name "agenda/Science.org") "Ideas")
      "** TODO %?\n"
      :empty-lines 1)

     ("a" "Agenda")
     ,(lps/org-capture-make-generic-timestamp-template "at" "Teaching"
                                                       "agenda/Teaching.org")

     ,(lps/org-capture-make-generic-timestamp-template "as" "Science Event"
                                                       "agenda/Science.org"
                                                       "%i")

     ,(lps/org-capture-make-generic-timestamp-template "aw" "Workplace events"
                                                       "agenda/Workplace.org"
                                                       "%i")

     ,(lps/org-capture-make-generic-timestamp-template "ao" "Others"
                                                       "agenda/Others.org")
     ("aa"
      "Any (pick)" entry
      (function lps/org-capture-agenda-any-location)
      "* %?\n%^t"
      :empty-lines 1)

     ("r" "Random")
     ("rr" "Random" plain
      (file+function "everything.org"
                     lps/org-ask-location))

     ("rm" "Movie" checkitem
      (file+function "movies.org" lps/org-ask-location))

     ("rb" "Book" checkitem
      (file+function "books.org" lps/org-ask-location))

     ("rR" "Restaurant" checkitem
      (file+function "restaurants.org" lps/org-ask-location))))

  (org-capture-bookmark nil)
  :config
  ;; From https://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture
  (defun lps/org-ask-location ()
    (let* ((org-refile-targets '((nil :maxlevel . 2)))
           (hd (condition-case nil
                   (car (org-refile-get-location nil nil t))
                 (error (car org-refile-history)))))
      (goto-char (point-min))
      (outline-next-heading)
      (beginning-of-line)              ; problems when hd is the first heading ?
      (if (re-search-forward
           (format org-complex-heading-regexp-format (regexp-quote hd))
           nil t)
          (goto-char (point-at-bol))
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " hd "\n")))
    (end-of-line))

  ;; Mimics `org-capture-set-target-location', in particular the file+function part
  (defun lps/org-capture-agenda-any-location ()
    (let ((path (read-file-name "Capture in file: "
                                (lps/org-expand-file-name "agenda" t))))
      (set-buffer (org-capture-target-buffer path))
      (org-capture-put-target-region-and-position)
      (widen)
      (lps/org-ask-location))))

(use-package org-roam
  :after org
  :init
  (defvar lps/org-roam-map (make-sparse-keymap))
  :custom
  (org-roam-directory (lps/org-expand-file-name "RoamNotes" t))
  (org-roam-node-display-template (concat "${title:*} "
                                          (propertize "${tags}"
                                                      'face
                                                      'org-tag)))
  (org-roam-capture-templates
   `(("d"
      "default" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t
      :hook ,(lambda () (call-interactively 'org-roam-tag-add)))))
  :bind-keymap
  ("C-c n" . lps/org-roam-map)
  :bind
  (:map lps/org-roam-map
        ("t" . org-roam-buffer-toggle)
        ("f" . org-roam-node-find)
        ("g" . org-roam-graph)
        ("i" . org-roam-node-insert)
        ("c" . org-roam-capture)
        ("a" . org-roam-tag-add))
  :config
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after consult org-roam
  :bind
  (:map lps/org-roam-map
        ("F" . consult-org-roam-file-find)
        ("b" . consult-org-roam-backlinks)
        ("s" . consult-org-roam-search))
  :custom
  (consult-org-roam-grep-func #'consult-grep)
  :config
  (consult-org-roam-mode))

(use-package org-roam-ui
  :after org-roam
  :bind
  (:map lps/org-roam-map
   ("G" . org-roam-ui-mode))
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow nil)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(system-case
 (gnu/linux
  (use-package pdf-tools
    :magic ("%PDF" . pdf-view-mode)
    :init
    ;; For some reason it doesn't work when put in the :custom section ?!
    (setq pdf-tools-enabled-modes
          '(pdf-history-minor-mode
            pdf-isearch-minor-mode
            pdf-links-minor-mode
            pdf-misc-minor-mode
            pdf-outline-minor-mode
            pdf-misc-size-indication-minor-mode
            pdf-misc-menu-bar-minor-mode
            pdf-annot-minor-mode
            pdf-sync-minor-mode
            pdf-misc-context-menu-minor-mode
            pdf-cache-prefetch-minor-mode
            ;; pdf-occur-global-minor-mode ; bugged autoload
            pdf-view-auto-slice-minor-mode ; add to defaults
            ;; pdf-virtual-global-minor-mode
            ))
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward)
                ("C-c ?" . lps/pdf-maybe-goto-index)
                ("<C-down>" . pdf-view-scroll-up-or-next-page)
                ("<C-up>" . pdf-view-scroll-down-or-previous-page)
                ("<C-left>" . image-scroll-right)
                ("<C-right>" . image-scroll-left)
                ("s a" . pdf-view-auto-slice-minor-mode)
                ("G" . pdf-view-goto-label)
                ("<f11>" . lps/slideshow-mode))
    :custom
    (pdf-links-read-link-convert-commands '("-font" "FreeMono"
                                            "-pointsize" "%P"
                                            "-undercolor" "%f"
                                            "-fill" "%b"
                                            "-draw" "text %X,%Y '%c'"))
    (pdf-links-convert-pointsize-scale 0.015) ;; Slightly bigger than default
    (pdf-view-display-size 'fit-page)
    (pdf-view-selection-style 'glyph)
    :config
    (pdf-tools-install :no-query)

    (defun lps/pdf-maybe-goto-index ()
      "Tries to guess where the index of the document is,
and ask for a keyword to find from there. If no Index is found,
move to the end of the document, and search backward instead."
      (interactive)
      (if (ignore-errors (pdf-outline (current-buffer)))
          (progn
            (goto-char (point-max))
            (let ((case-fold-search t))
              (if (search-backward "Index" nil t)
                  (pdf-outline-follow-link-and-quit)
                (pdf-outline-quit)))
            (isearch-forward))
        (pdf-view-goto-page (pdf-cache-number-of-pages))
        (isearch-backward)))

    (defun lps/pdf-maybe-search-doc-dwim (word)
      "Tries to guess where the documentation of WORD is in the current
manual.

It first jumps to the index, then searches for a match. If it is
the only match, it highlights it and asks for confirmation before
jumping to the relevant link.

Otherwise, give control back to the user with a `isearch' session
pre-filled with WORD."
      ()))))

(use-package saveplace-pdf-view
  :after pdf-view)

(use-package doc-toc
  :after pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("C-c C-e" . doc-toc-extract-pages)
        ("C-c M-e" . doc-toc-extract-pages-ocr))
  :config
  ;; Mandatory as the package is poorly-written ...
  (define-key pdf-view-mode-map (kbd "C-c e") nil))

;; AUCTeX initialization
(use-package tex-site
  :ensure auctex) ;; Don't defer, buggy ?

(use-package tex
  :ensure auctex
  :defer t
  :bind
  (:map TeX-mode-map
        ("C-c '" . TeX-error-overview)
        ("<f5>" . lps/auto-compile)
        ("<backtab>" . indent-for-tab-command)
        ([remap TeX-documentation-texdoc] . lps/TeX-documentation-texdoc))
  :custom
  ;; Parse documents to provide completion
  (TeX-parse-self t)
  ;; Automatically save style information
  (TeX-auto-save t)
  ;; Ask for the master file & don't assume anything
  (TeX-master nil)
  ;; Don't ask permission to save before compiling
  (TeX-save-query nil)
  ;; Automatically insert braces after sub- and superscripts in math mode
  (TeX-electric-sub-and-superscript t)
  ;; Don't insert magic quotes right away.
  (TeX-quote-after-quote t)
  ;; Don't insert braces by default
  (TeX-insert-braces t)
  ;; But do insert closing $ when inserting the first one
  (TeX-electric-math '("$" . "$"))
  ;; Don't ask for confirmation when cleaning
  (TeX-clean-confirm nil)
  ;; AucTeX doesn't search subdirectories for input/include ...
  (TeX-arg-input-file-search 'ask)
  ;; Ask for note when inserting citations
  (TeX-arg-cite-note-p t)
  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "PDF tools")))
  ;; Compilation
  ;; Automatically open the error buffer if errors happened
  ;; But don't collect bad-boxes by default
  ;; If needed, you can still show them with <C-c '> (TeX-error-overview)
  (TeX-debug-bad-boxes nil)
  (TeX-debug-warnings nil)
  (TeX-error-overview-open-after-TeX-run t)
  :config
  (add-to-list 'lps/auto-compile-command-alist
               (cons 'latex-mode 'lps/TeX-recompile-all))

  ;; SyncTeX forward and inverse Search
  (setq TeX-source-correlate-mode t
        ;; Produce a PDF by default
        TeX-PDF-mode t)

  (unless (assoc "PDF tools" TeX-view-program-list-builtin)
    (push '("PDF tools" TeX-pdf-tools-sync-view) TeX-view-program-list))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Redefine TeX-documentation-texdoc to open the doc in Emacs
  (defun lps/TeX-documentation-texdoc (&optional arg)
    "Run texdoc to read documentation.

Prompt for selection of the package of which to show the
documentation.

If called with a prefix argument ARG, after selecting the
package, prompt for selection of the manual of that package to
show."
    (interactive "P")
    (let ((at-point (thing-at-point 'symbol))
          (pkgs TeX-active-styles)
          (pkg nil)
          buffer list doc)
      ;; Strip off properties.  XXX: XEmacs doesn't have
      ;; `substring-no-properties'.
      (set-text-properties 0 (length pkg) nil pkg)
      (setq pkg (completing-read (concat "View documentation in manual (default "
                                         at-point
                                         "): ")
                                 pkgs
                                 nil nil nil nil at-point t))
      (unless (zerop (length pkg))
        (progn
          ;; Create the buffer, insert the result of the command, and
          ;; accumulate the list of manuals.
          (with-current-buffer (get-buffer-create
                                (setq buffer (format "*texdoc: %s*" pkg)))
            (erase-buffer)
            (insert (shell-command-to-string
                     (concat "texdoc --list --nointeract " pkg)))
            (goto-char 1)           ; No need to use `point-min' here.
            (save-excursion
              (while (re-search-forward
                      ;; XXX: XEmacs doesn't support character classes in
                      ;; regexps, like "[:alnum:]".
                      "^ *\\([0-9]+\\) +\\([-~/a-zA-Z0-9_.${}#%,:\\ ()]+\\)"
                      nil t)
                (push (cons (match-string 1) (match-string 2)) list))))
          (unwind-protect
              (cond
               ((null (executable-find "texdoc"))
                ;; Note: `shell-command-to-string' uses shell, only
                ;; `call-process' looks at `exec-path', thus only here makes
                ;; sense to use `executable-find' to test whether texdoc is
                ;; available.
                (message "texdoc not found"))
               (list
                ;; Go on if there are manuals listed: show the buffer, prompt
                ;; for the number of the manual, then run
                ;;     texdoc --just-view <doc>
                (TeX-pop-to-buffer (get-buffer buffer))
                (condition-case nil
                    (when (setq doc
                                (cdr (assoc
                                      (if arg (TeX-read-string "Please enter \
the number of the file to view, anything else to skip: ") "1") list)))
                      (find-file doc))
                  ;; Exit gently if a `quit' signal is thrown.
                  (quit nil)))
               (t (message "No documentation found for %s" pkg)))
            ;; In any case quit-and-kill the window.
            (kill-buffer buffer))))))

  ;; Redefine TeX-completing-read-multiple
  ;; Might be a bit buggy (return value is not expected to be nil,
  ;; or something along those lines ...) but I prefer having a
  ;; consistent interface, regardless of minor bugs
  (defalias 'TeX-completing-read-multiple 'completing-read-multiple)

  ;; Redefine multi-prompt-value to use completing-read-multiple
  ;; rather than read-from-minibuffer
  ;; This allows Vertico to do its job
  (with-eval-after-load 'multi-prompt
    (defun multi-prompt-key-value
        (prompt table &optional predicate require-match initial-input
                hist def inherit-input-method)
      "Read multiple strings, with completion and key=value support.
PROMPT is a string to prompt with, usually ending with a colon
and a space.  TABLE is an alist.  The car of each element should
be a string representing a key and the optional cdr should be a
list with strings to be used as values for the key.

See the documentation for `completing-read' for details on the
other arguments: PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST,
DEF, and INHERIT-INPUT-METHOD.

The return value is the string as entered in the minibuffer."
      (let* ((minibuffer-completion-table #'multi-prompt-key-value-collection-fn)
             (minibuffer-completion-predicate predicate)
             (minibuffer-completion-confirm
              (unless (eq require-match t) require-match))
             (multi-prompt-completion-table
              ;; Expand the table here because completion would otherwise
              ;; interpret symbols in the table as functions.  However, it
              ;; would be nicer if this could be done during the actual
              ;; completion in order to avoid walking through the whole
              ;; table.
              (multi-prompt-expand-completion-table table))
             (map (if require-match
                      crm-local-must-match-map
                    crm-local-completion-map))
             ;; (input (read-from-minibuffer
             ;;         prompt initial-input map
             ;;         nil hist def inherit-input-method))
             (input (or (completing-read-multiple
                         prompt minibuffer-completion-table
                         predicate require-match initial-input
                         hist def inherit-input-method)
                        "")))
        (and def (string-equal input "") (setq input def))
        input)))

  ;; Better completion functions
  (defun lps/latex-company-setup () ;; TO FIX !
    (setq-local company-backends
                '((company-math-symbols-unicode
                   company-math-symbols-latex
                   company-latex-commands
                   company-capf
                   company-dabbrev
                   company-ispell)))
    (setq-local company-minimum-prefix-length 4))

  ;; Improve isearch and query-replace (regexp or not) in math mode
  (defun lps/safe-texmathp (beg end)
    (if (derived-mode-p 'tex-mode)
        (save-excursion (save-match-data (goto-char beg) (texmathp)))
      t))

  (defvar lps/isearch-old-filter-predicate nil
    "Old predicate used in `isearch-filter-predicate'
Internal use.")

  (defvar lps/isearch-in-mathp nil)

  (isearch-define-mode-toggle latex-math "m" nil
    "This determines whether to search only in math mode in the
LaTeX document"
    (isearch--momentary-message
     (if (setq lps/isearch-in-mathp (not lps/isearch-in-mathp))
         (progn
           (setq lps/isearch-old-filter-predicate isearch-filter-predicate
                 isearch-filter-predicate 'lps/safe-texmathp)
           "Search or replace restricted to math mode")
       (setq isearch-filter-predicate lps/isearch-old-filter-predicate
             lps/isearch-old-filter-predicate nil)
       "Search or replace not restricted to math mode")))

  (defun lps/TeX-remove-macro ()
    "Remove current macro and return `t'.  If no macro at point,
return `nil'."
    (interactive)
    (when (TeX-current-macro)
      (let ((bounds (TeX-find-macro-boundaries))
            (brace  (save-excursion
                      (goto-char (1- (TeX-find-macro-end)))
                      (TeX-find-opening-brace))))
        (delete-region (1- (cdr bounds)) (cdr bounds))
        (delete-region (car bounds) (1+ brace)))
      t))

  (defun lps/TeX-recompile-all ()
    ;; Clean everything
    (TeX-clean t)
    ;; Recompile everything
    (let ((TeX-debug-bad-boxes t)
          (TeX-debug-warnings t)
          (TeX-error-overview-open-after-TeX-run t))
      (TeX-command-sequence t t)))

  ;; Beamer-related stuff
  (defvar beamer-mode-map
    (define-keymap
      "C-M-x"   'lps/LaTeX-beamer-compile-frame
      "C-c M-r" 'lps/LaTeX-beamer-change-all-pauses
      "C-x n f" 'lps/LaTeX-beamer-narrow-to-frame
      "<remap> <mark-paragraph>" 'lps/LaTeX-beamer-mark-frame))

  (define-minor-mode beamer-mode
    "A minor mode for editing LaTeX document using the beamer class.
  It defines the following commands:

  \\{beamer-mode-map}"
    :keymap beamer-mode-map
    (if beamer-mode
        (progn
          (advice-add 'TeX-fold-item :override 'lps/TeX-fold-item-beamer)
          (lps/LaTeX-beamer-frame-as-section)
          (lps/LaTeX-beamer-fold-all-frames))
      (progn
        (advice-remove 'TeX-fold-item 'lps/TeX-fold-item-beamer)
        (lps/LaTeX-beamer-remove-frame-as-section)
        (TeX-fold-clearout-buffer))))

  (add-hook 'TeX-update-style-hook
            (lambda ()
              (TeX-add-style-hook "beamer" 'beamer-mode))) ; Buggy ?! Overrides default

  ;; Folding
  (defvar lps/TeX-beamer-fold-frame-overlay-regexp
    (concat (regexp-quote TeX-esc)
            "begin[ \t]*{"
            "[A-Za-z*]+}.*?[ \t\n]*?"
            (regexp-quote TeX-esc)
            "frametitle[ \t]*{"
            "\\([[:ascii:]]+?\\)}\n"))

  (defun lps/TeX-fold-frame (type)
    "Hide the frame at point.

  Return non-nil if a frame was found and folded, nil otherwise."
    (when (and (eq type 'env)
               (eq major-mode 'latex-mode)
               (string= (LaTeX-current-environment) "frame"))
      (when-let ((item-start (condition-case nil
                                 (save-excursion
                                   (LaTeX-find-matching-begin) (point))
                               (error nil)))
                 (item-end (TeX-fold-item-end item-start type))
                 (item-name (or (save-excursion
                                  (goto-char item-start)
                                  (re-search-forward
                                   lps/TeX-beamer-fold-frame-overlay-regexp
                                   item-end t 1)
                                  (match-string-no-properties 1))
                                "frame"))
                 (display-string-spec (if (string= item-name "frame")
                                          "[frame]"
                                        (concat "[frame:"
                                                (truncate-string-to-width item-name 12 0 nil "…")
                                                "]")))
                 (ov (TeX-fold-make-overlay item-start item-end type
                                            display-string-spec)))
        (TeX-fold-hide-item ov))))

  (defun lps/TeX-fold-item-beamer (type)
    (or (lps/TeX-fold-frame type)
        ;; Copy of TeX-fold-env code:
        ;; Can't directly call it, as this is the function we advice,
        ;; as it is used as the entry point in most other functions
        ;; rather than the wrapper TeX-fold-<env/macro> ...
        (if (and (eq type 'env)
                 (eq major-mode 'plain-tex-mode))
            (message
             "Folding of environments is not supported in current mode")
          (let ((item-start (cond ((and (eq type 'env)
                                        (eq major-mode 'context-mode))
                                   (save-excursion
                                     (ConTeXt-find-matching-start) (point)))
                                  ((and (eq type 'env)
                                        (eq major-mode 'texinfo-mode))
                                   (save-excursion
                                     (Texinfo-find-env-start) (point)))
                                  ((eq type 'env)
                                   (condition-case nil
                                       (save-excursion
                                         (LaTeX-find-matching-begin) (point))
                                     (error nil)))
                                  (t
                                   (TeX-find-macro-start)))))
            (when item-start
              (let* ((item-name (save-excursion
                                  (goto-char item-start)
                                  (looking-at
                                   (cond ((and (eq type 'env)
                                               (eq major-mode 'context-mode))
                                          (concat (regexp-quote TeX-esc)
                                                  "start\\([A-Za-z]+\\)"))
                                         ((and (eq type 'env)
                                               (eq major-mode 'texinfo-mode))
                                          (concat (regexp-quote TeX-esc)
                                                  "\\([A-Za-z]+\\)"))
                                         ((eq type 'env)
                                          (concat (regexp-quote TeX-esc)
                                                  "begin[ \t]*{"
                                                  "\\([A-Za-z*]+\\)}"))
                                         (t
                                          (concat (regexp-quote TeX-esc)
                                                  "\\([A-Za-z@*]+\\)"))))
                                  (match-string-no-properties 1)))
                     (fold-list (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
                                      ((eq type 'math)
                                       TeX-fold-math-spec-list-internal)
                                      (t TeX-fold-macro-spec-list-internal)))
                     fold-item
                     (display-string-spec
                      (or (catch 'found
                            (while fold-list
                              (setq fold-item (car fold-list))
                              (setq fold-list (cdr fold-list))
                              (when (member item-name (cadr fold-item))
                                (throw 'found (car fold-item)))))
                          ;; Item is not specified.
                          (if TeX-fold-unspec-use-name
                              (concat "[" item-name "]")
                            (if (eq type 'env)
                                TeX-fold-unspec-env-display-string
                              TeX-fold-unspec-macro-display-string))))
                     (item-end (TeX-fold-item-end item-start type))
                     (ov (TeX-fold-make-overlay item-start item-end type
                                                display-string-spec)))
                (TeX-fold-hide-item ov)))))))

  ;; Mark
  (defun lps/LaTeX-beamer-mark-frame ()
    (interactive)
    (unless (member "beamer" TeX-active-styles)
      (error "Not in a beamer document"))
    (beginning-of-line)
    (while (not (looking-at-p "\\\\begin *{frame}"))
      (LaTeX-find-matching-begin))
    (forward-char)
    (LaTeX-mark-environment))

  ;; Adapted from:
  ;; https://mbork.pl/2016-07-04_Compiling_a_single_Beamer_frame_in_AUCTeX
  (defun lps/LaTeX-beamer-compile-frame ()
    "Compile the current frame"
    (interactive)
    (let* ((sections (lps/LaTeX-beamer-all-sections))
           (add-sections (lambda ()
                           (save-excursion
                             (goto-char (point-min))
                             (re-search-forward "\\\\begin{document}" nil t)
                             (insert "\n"
                                     "% SECTIONS BEFORE POINT\n"
                                     (mapconcat 'identity (car sections) "\n"))
                             (goto-char (point-max))
                             (re-search-backward "\\\\end{document}" nil t)
                             (insert "\n"
                                     "% SECTIONS AFTER POINT\n"
                                     (mapconcat 'identity (cdr sections) "\n")))))
           (TeX-region-hook (cons add-sections TeX-region-hook)))
      (save-mark-and-excursion
        (lps/LaTeX-beamer-mark-frame)
        (TeX-command-run-all-region))))

  ;; TODO: finish integrating this
  ;;
  ;; Difficulty: inserting sections that appear *before* point above the "current"
  ;; text, and those appearing *after*, after.
  ;;
  ;;Moreover, empty sections are not appearing, and all sections are empty, as we
  ;;want to compile a single frame.
  (defun lps/LaTeX-beamer-all-sections ()
    ;; Taken from `reftex-section-info'. Somewhat buggy, not an expert in RefTeX
    ;; code, so use at your own risk.
    ;; Inlines a bunch of stuff found in called functions too.
    (let (sections-before
          sections-after
          (start-point (point)))
      (save-excursion
        (goto-char (point-min))
        (save-match-data
          (while (re-search-forward reftex-section-regexp nil t)
            (let* ((macro (reftex-match-string 0))
                   (level-exp (cdr (assoc (match-string 2) reftex-section-levels-all)))
                   (text1 (buffer-substring-no-properties
                           (point)
                           (min (+ (point) 150)
                                (point-max)
                                (condition-case nil
                                    ;; Unneeded fanciness.
                                    (let ((forward-sexp-function nil))
                                      (up-list 1)
                                      (1- (point)))
                                  (error (point-max))))))
                   ;; Literal can be too short since text1 too short. No big problem.
                   (text (reftex-nicify-text text1)))
              (when (<= 1 level-exp)
                (let ((section-text (concat macro text "}")))
                  (if (<= (point) start-point)
                      (push section-text sections-before)
                    (push section-text sections-after))))))))
      (cons (nreverse sections-before) (nreverse sections-after))))

  (defun lps/LaTeX-beamer-narrow-to-frame ()
    (interactive)
    (save-mark-and-excursion
      (lps/LaTeX-beamer-mark-frame)
      (narrow-to-region (region-beginning) (region-end))))

  (defun lps/LaTeX-beamer-frame-as-section ()
    (require 'reftex)
    (unless (assoc-string "frametitle" reftex-section-levels)
      (setq-local reftex-section-levels
                  (append reftex-section-levels
                          '(("frametitle" . -4)
                            ("framesubtitle" . -5)))))
    (reftex-reset-mode))

  (defun lps/LaTeX-beamer-remove-frame-as-section ()
    (require 'reftex)
    (when (assoc-string "frametitle" reftex-section-levels)
      (setq-local reftex-section-levels
                  (cl-remove-if
                   (lambda (item)
                     (let ((name (car item)))
                       (assoc-string name '("frametitle" "framesubtitle"))))
                   reftex-section-levels)))
    (reftex-reset-mode))

  (defun lps/LaTeX-beamer-fold-all-frames ()
    (interactive)
    (lps/TeX-fold-all-of-env "frame"))

  (defvar lps/LaTeX-beamer-pause-macros '("pause["
                                          "only<"
                                          "onslide<"
                                          "alt<"
                                          "item<")
    "List of LaTeX macros that specifying pauses or overlays in a beamer
  frame, and whose syntax rougly follows the one used by \\onslide<...>

  Time specifications in those macros will be modified by the
  `lps/LaTeX-beamer-change-all-pauses' function.

  The character introducting the parameter list, usually [, < or {, has
  to be added to the end, e.g. if you want to recognize the overlays
  attached to items in an itemize environment, add \"item<\" to this
  variable")

  (defun lps/LaTeX-beamer-change-all-pauses (n &optional from-here)
    "Increase by N all the pauses and overlays timesteps specified by
  the macros of `lps/LaTeX-beamer-pause-macros' contained in the
  current frame.

  If FROM-HERE is non-nil, only change the ones after the point.

  If the region is active, ignore FROM-HERE and only act on the region
  instead."
    (interactive "*nChange by steps: \nP")
    (save-mark-and-excursion
      (save-restriction
        (let ((beg (cond
                    (from-here (point))
                    ((region-active-p) (region-beginning))
                    (t nil))))
          (unless (region-active-p)
            (lps/LaTeX-beamer-mark-frame)
            (setq beg (or beg (region-beginning))))
          (narrow-to-region beg (region-end))
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\\\" (regexp-opt lps/LaTeX-beamer-pause-macros))
                  nil t)
            (let ((point (point))
                  (end (re-search-forward "[[:space:]]*]\\|>\\|}" nil t)))
              (when (and end (not (texmathp)))
                (goto-char point)
                (while (re-search-forward "[[:digit:]]+" end t)
                  (let ((num (string-to-number (match-string 0))))
                    (replace-match (number-to-string (+ num n)))))))))))))

(use-package latex
  :after tex
  :ensure nil
  :bind
  (:map LaTeX-mode-map
        ([remap beginning-of-defun] . LaTeX-find-matching-begin)
        ([remap end-of-defun] . LaTeX-find-matching-end)
        ("C-S-b" . lps/LaTeX-prev-math)
        ("C-S-f" . lps/LaTeX-next-math))
  :hook
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . lps/latex-fontification)
  ;; (LaTeX-mode . lps/latex-company-setup)
  ;; (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . cdlatex-mode)
  :custom
  ;; Automatically insert closing brackets
  (LaTeX-electric-left-right-brace t)
  ;; Also change the key to access LaTeX-math-mode
  (LaTeX-math-abbrev-prefix "°")
  ;; Improve fontification
  (font-latex-fontify-script 'multi-level)
  :config
  (dolist (env '(("tikzpicture") ("scope")))
    (add-to-list 'LaTeX-indent-environment-list env
                 nil 'equal))

  ;;;  Navigation
  ;; Slow: could be made faster by searching $ or \] characters rather
  ;; than calling `texmathp' at each step. However, this is the most
  ;; robust, and fast enough.
  (defun lps/LaTeX-prev-math ()
    (interactive)
    (while (not (texmathp))
      (backward-char 1)))

  (defun lps/LaTeX-next-math ()
    (interactive)
    (while (not (texmathp))
      (forward-char 1)))

  ;; Improve fontification
  (defun lps/latex-fontification ()
    (set-face-attribute 'font-latex-sedate-face nil :foreground "#aab5b8")
    (font-latex-add-keywords '(("newenvironment" "*{[[")
                               ("renewenvironment" "*{[[")
                               ("newcommand" "*|{\\[[")
                               ("renewcommand" "*|{\\[[")
                               ("providecommand" "*|{\\[[")
                               ("fbox" "")
                               ("mbox" "")
                               ("sbox" ""))
                             'function))

  ;; Improve compile region
  (defun lps/LaTeX-add-bib-to-compile-region ()
    (let (bibstyle bibpath)
      (with-current-buffer TeX-region-master-buffer
        (save-excursion
          (goto-char (point-min))
          (setq bibpath (reftex-locate-bibliography-files
                         (file-name-directory (reftex-TeX-master-file))))
          (re-search-forward "\\\\bibliographystyle{[a-z]+}" nil t)
          (setq bibstyle (or (match-string-no-properties 0) ""))))
      (save-excursion
        (goto-char (point-max))
        (re-search-backward "\\\\end{document}")
        (insert bibstyle
                "\n"
                "\\bibliography{"
                (mapconcat 'identity bibpath ",")
                "}\n"))))

  (add-hook 'TeX-region-hook 'lps/LaTeX-add-bib-to-compile-region))

(use-package tex-fold
  :defer t
  :ensure nil
  :hook (LaTeX-mode . TeX-fold-mode)
  :bind
  (:map TeX-fold-keymap
        ("C-a" . lps/TeX-fold-all-of-env))
  :custom
  ;; Folding
  (TeX-fold-command-prefix "\C-o")
  (TeX-fold-env-spec-list '(("[frame]" ("frame"))
                            ("[comment]" ("comment"))))
  :config
  ;; Not very robust
  (defun lps/TeX-fold-all-of-env (env)
    (interactive "MFold environment: ")
    (save-excursion
      (goto-char (point-min))
      (let ((env-start (format "\\begin{%s}" env)))
        (while (search-forward env-start nil t)
          (TeX-fold-env))))))

(use-package bibtex
  :defer t
  :init
  (defvar lps/bib-directory (lps/org-expand-file-name "biblio" t))
  (defvar lps/bib-bibliography-files
    (list (expand-file-name "biblio.bib" lps/bib-directory)))
  (defvar lps/bib-bibliography-library
    (file-name-as-directory (expand-file-name "articles" lps/bib-directory)))
  :bind
  (:map bibtex-mode-map
        ("C-c C-?" . bibtex-print-help-message)
        ([remap bibtex-clean-entry] . org-ref-clean-bibtex-entry))
  :custom
  (bibtex-entry-format '(realign
                         unify-case
                         opts-or-alts
                         required-fields
                         numerical-fields
                         last-comma))
  (bibtex-unify-case-function 'downcase)
  (bibtex-comma-after-last-field t)
  (bibtex-autokey-title-terminators "[.!?;]\\|--") ; removed : from default
  ;; minor changes + also re-specify in case default changes
  (bibtex-autokey-prefix-string "")
  (bibtex-autokey-names 3)
  (bibtex-autokey-names-stretch 0)
  (bibtex-autokey-name-case-convert-function 'lps/bibtex-autokey-name-convert)
  (bibtex-autokey-name-length 'infty)
  (bibtex-autokey-name-separator "_")
  (bibtex-autokey-year-length 2)
  (bibtex-autokey-titlewords 5)
  (bibtex-autokey-titlewords-stretch 2)
  (bibtex-autokey-titleword-separator "_")
  (bibtex-autokey-name-year-separator "")
  (bibtex-autokey-year-title-separator "_")
  :config
  (require 'bibtex-completion)
  ;; Fix accentuation
  (dolist (a '((("\"o" "\\\"o" "\\o") . "o") ; "o,\"o,\o,\oe -> oe
               (("\"O" "\\\"O" "\\O") . "O") ; "O,\"O,\O,\OE -> Oe
               (("\"a" "\\\"a") . "a")       ; "a,\"a,\ae -> ae
               (("\"A" "\\\"A") . "A")       ; "A,\"A,\AE    -> Ae
               (("\"u" "\\\"u") . "u")       ; "u,\"u        -> ue
               (("\"U" "\\\"U") . "U")))     ; "U,\"U        -> Ue
    (add-to-list 'bibtex-autokey-transcriptions
                 (cons (regexp-opt (car a)) (cdr a))
                 nil
                 'equal))

  ;; Need to reevaluate them ...
  (setq bibtex-autokey-name-change-strings bibtex-autokey-transcriptions
        bibtex-autokey-titleword-change-strings bibtex-autokey-transcriptions)

  ;; From https://emacs.stackexchange.com/a/75531
  (defun lps/string-try-remove-accentuation (string)
    (mapconcat (lambda (c)
                 (char-to-string
                  (car (get-char-code-property c 'decomposition))))
               string))

  (defun lps/bibtex-autokey-name-convert (name)
    (downcase (lps/string-try-remove-accentuation name)))

  (defun lps/bibtex-fix-long-field-format ()
    "Slow version which fixes a problem of alignment/line breaks
when formatting Bibtex entries. Hacky workaround."
    (save-excursion
      (bibtex-beginning-of-entry)
      (dolist (field (bibtex-parse-entry))
        (bibtex-beginning-of-entry)
        (let* ((bounds (or
                        (bibtex-search-forward-field (downcase (car field)))
                        (bibtex-search-forward-field (upcase (car field)))))
               (end-field (and bounds (copy-marker (bibtex-end-of-field bounds)))))
          (when end-field
            (goto-char (bibtex-start-of-field bounds))
            (forward-char)              ; leading comma
            (bibtex-delete-whitespace)
            (insert "\n")
            (indent-to-column (+ bibtex-entry-offset
                                 bibtex-field-indentation))
            (re-search-forward "[ \t\n]*=" end-field)
            (replace-match "=")
            (forward-char -1)
            (if bibtex-align-at-equal-sign
                (indent-to-column
                 (+ bibtex-entry-offset (- bibtex-text-indentation 2)))
              (insert " "))
            (forward-char)
            (bibtex-delete-whitespace)
            (if bibtex-align-at-equal-sign
                (insert " ")
              (indent-to-column bibtex-text-indentation)))))))

  (add-hook 'bibtex-clean-entry-hook 'lps/bibtex-fix-long-field-format)

  (defun lps/bibtex-remove-field (field)
    (save-excursion
      (bibtex-beginning-of-entry)
      (let ((bounds (bibtex-search-forward-field field t)))
        (when bounds
          (delete-region (bibtex-start-of-field bounds)
                         (bibtex-end-of-field bounds))
          (insert "\n")))))

  (add-hook 'bibtex-clean-entry-hook (lambda ()
                                       (lps/bibtex-remove-field "file")))

  (defun lps/bibtex-keep-only-doi-if-url (&rest args)
    "Delete the URL field when there is also a DOI field present.

Does not read any argument: this is to be able to add it both to
`bibtex-clean-entry-hook' and to be passed to
`bibtex-map-entries'."
    (interactive)
    (let* ((fields (bibtex-parse-entry))
           (doi (assoc "doi" fields))
           (url (assoc "url" fields)))
      (when (and doi url)
        (save-excursion
          (goto-char (car (cdr (bibtex-search-backward-field "url" t))))
          (bibtex-kill-field)))))

  (add-hook 'bibtex-clean-entry-hook 'lps/bibtex-keep-only-doi-if-url)

  ;; Bibtex tags
  (defvar lps/bibtex-tags '(("Topological Full Group" . "tfg")
                            ("Projective Fundamental Group" . "pfg")
                            ("Cocycles" . "coc")
                            ("Recursion Theory" . "rec")
                            ("Subshift" . "shf")
                            ("Group Theory" . "grp")
                            ("Graph Theory" . "gph")
                            ("Substitution" . "sub")
                            ("Hom-Shifts" . "hom")
                            ("Complexity Theory" . "cpl")))

  (defun lps/bibtex-tag-description-to-tag (desc)
    (cdr (assoc-string desc lps/bibtex-tags)))

  (defun lps/bibtex-replace-tags (key beg end)
    (interactive (list (save-excursion
                         (bibtex-beginning-of-entry)
                         (and (looking-at bibtex-entry-maybe-empty-head)
                              (bibtex-key-in-head)))
                       (save-excursion
                         (bibtex-beginning-of-entry)
                         (point))
                       (save-excursion
                         (bibtex-end-of-entry)
                         (point))))
    (let* ((descs (completing-read-multiple (format-prompt key nil)
                                            lps/bibtex-tags))
           (tags (mapcar 'lps/bibtex-tag-description-to-tag descs)))
      (bibtex-set-field "tags" (mapconcat 'identity tags ","))))

  (defun lps/bibtex-filter-by-tags (tags)
    (interactive (list
                  (let ((descs (completing-read-multiple
                                (format-prompt "Tags: " nil)
                                lps/bibtex-tags)))
                    (mapcar 'lps/bibtex-tag-description-to-tag descs))))
    ())

  ;; Biblio additions with org-capture
  (defun lps/bibtex-capture-format-and-prompt-for-pdf-file ()
    (org-ref-clean-bibtex-entry)
    (when (y-or-n-p "Link to local article ? ")
      (org-ref-bibtex-assoc-pdf-with-entry)))

  (require 'org-capture)
  (add-to-list 'org-capture-templates
               `("b" "Biblio" plain
                 (file ,(car lps/bib-bibliography-files))
                 "%x\n"
                 :empty-lines 1
                 :before-finalize lps/bibtex-capture-format-and-prompt-for-pdf-file)
               t 'equal))

(use-package reftex
  :after latex
  :hook (LaTeX-mode . reftex-mode)
  :bind
  (:map TeX-mode-map
        ("C-c M-n" . reftex-parse-all))
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-trust-label-prefix '("sec:" "def:" "ex:"
                               "lem:" "prop:" "thm:"
                               "rem:" "cor:" "conj:"
                               "claim:" "obs:" "proof:"
                               "fig:"))
  (reftex-toc-split-windows-horizontally nil)
  (reftex-toc-split-windows-fraction 0.5)
  (reftex-cite-format "~\\cite[]{%l}")
  (reftex-cite-prompt-optional-args t)
  (reftex-label-alist
   '(("section"     ?s "sec:"   "~\\ref{%s}" t (regexp "[Ss]ection\\(s\\)?"))
     ("definition"  ?d "def:"   "~\\ref{%s}" 1 (regexp "[Dd]efinition\\(s\\)?"))
     ("example"     ?x "ex:"    "~\\ref{%s}" 1 (regexp "[Ee]xample\\(s\\)?"))
     ("lemma"       ?l "lem:"   "~\\ref{%s}" 1 (regexp "[Ll]emma\\(s\\|ta\\)?"))
     ("proposition" ?p "prop:"  "~\\ref{%s}" 1 (regexp "[Pp]roposition\\(s\\)?"))
     ("theorem"     ?h "thm:"   "~\\ref{%s}" 1 (regexp "[Tt]heorem\\(s\\)?"))
     ("remark"      ?r "rem:"   "~\\ref{%s}" t (regexp "[Rr]emark\\(s\\)?"))
     ("corollary"   ?c "cor:"   "~\\ref{%s}" 1 (regexp "[Cc]orollar\\(y\\|ies\\)"))
     ("conjecture"  ?c "conj:"  "~\\ref{%s}" 1 (regexp "[Cc]onjecture\\(s\\)?"))
     ("claim"       ?c "claim:" "~\\ref{%s}" 1 (regexp "[Cc]laim\\(s\\)?"))
     ("observation" ?o "obs:"   "~\\ref{%s}" 1 (regexp "[Oo]bservation\\(s\\)?"))
     ("proof"       ?p "proof:" "~\\ref{%s}" 1 (regexp "[Pp]roof\\(s\\)?"))))
  (reftex-insert-label-flags '(t t))
  (reftex-derive-label-parameters `(4 25 t 1 "-"
                                      ,lps/do-not-capitalize-list
                                      t)))

(use-package reftex-cite
  :diminish
  :after reftex
  :config
  ;; From https://stackoverflow.com/questions/9682592/setting-up-reftex-tab-completion-in-emacs
  (defun lps/get-bibtex-keys (file)
    (with-current-buffer (find-file-noselect file)
      (mapcar 'car (bibtex-parse-keys))))

  (defun lps/LaTeX-add-all-bibitems-from-bibtex ()
    (interactive)
    (mapc 'LaTeX-add-bibitems
          (apply 'append
                 (mapcar 'lps/get-bibtex-keys (reftex-get-bibfile-list)))))

  ;; Override this function to have a better completion
  (defun-override lps/reftex--query-search-regexps (default)
    "Query for regexps for searching entries using DEFAULT as default.
Return a list of regular expressions."
    (split-string
     (completing-read
      (concat
       "Search bibliography for: "
       "[" default "]: ")
      ;; Ensure default is always in the completion list.
      (let ((def (when default (list default)))
            (coll (if reftex-mode
                      (if (fboundp 'LaTeX-bibitem-list)
                          (progn
                            ;; FIXME: don't do it every time ?
                            (lps/LaTeX-add-all-bibitems-from-bibtex)
                            (LaTeX-bibitem-list))
                        (cdr (assoc 'bibview-cache
                                    (symbol-value reftex-docstruct-symbol))))
                    nil)))
        (if (and def (member def coll))
            coll
          (cons def coll)))
      nil nil nil 'reftex-cite-regexp-hist))))

(use-package biblio
  :bind
  (:map bibtex-mode-map
        ("C-c ?" . biblio-lookup))
  (:map biblio-selection-mode-map
        ("M-RET" . biblio--selection-browse)
        ("RET" . lps/biblio-download-and-insert-bibtex))
  :custom
  (biblio-arxiv-bibtex-header "misc")
  (biblio-download-directory lps/bib-bibliography-library)
  (biblio-bibtex-use-autokey t)
  :config
  (defun lps/biblio-download-and-insert-bibtex ()
    (interactive)
    (let* ((bibfile (completing-read "Bibfile: " (org-ref-find-bibliography)))
           (record (biblio--selection-metadata-at-point))
           (bibtex-entry
            (funcall (biblio-alist-get 'backend record)
                     'forward-bibtex
                     record
                     (lambda (bibtex)
                       (biblio-format-bibtex bibtex biblio-bibtex-use-autokey))))
           (url (cdr (assq 'direct-url record))))
      ;; Add bibtex entry to biblio
      (save-window-excursion
        (with-current-buffer
            (find-file-noselect bibfile)
          ;; FIXME: check if entry already exists !
          (goto-char (point-max))
          (when (not (looking-back "\n\n" (min 3 (point))))
            (insert "\n\n"))
          (insert bibtex-entry)
          (let ((org-ref-title-case-types (cons '("misc" "title")
                                                org-ref-title-case-types)))
            (org-ref-title-case))
          (setq bibkey (bibtex-completion-get-key-bibtex))
          (save-buffer)))
      (if url
          (url-copy-file url (expand-file-name (concat bibkey ".pdf")
                                               biblio-download-directory))
        (user-error "No direct URL (try arXiv or HAL)")))))

(use-package bibtex-completion
  :init
  (defvar bibtex-completion-candidates-filter-functions nil
    "List of functions called to filter the candidates returned
by `bibtex-completion-candidates'

Each function will be called with the entire list of candidates.
It should return the list of filtered candidates, modifying the
entries if needed.")
  :commands bibtex-completion-key-at-point
  :custom
  (bibtex-completion-bibliography lps/bib-bibliography-files)
  (bibtex-completion-library-path lps/bib-bibliography-library)
  (bibtex-completion-display-formats
   '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7}")))
  :config
  (defun bibtex-completion--filter-candidates (fun &rest args)
    (let ((res (apply fun args)))
      (cl-reduce (lambda (candidates filter-fun)
                   (funcall filter-fun candidates))
                 bibtex-completion-candidates-filter-functions
                 :initial-value res)))

  (advice-add 'bibtex-completion-candidates
              :around 'bibtex-completion--filter-candidates)

  ;; Rewrite: shortcuts the evaluation !
  ;; Now only executes up until finding a non-NIL return value
  (defun bibtex-completion-key-at-point ()
    "Return the key of the BibTeX entry at point.
The functions used to match the keys are defined in
`bibtex-completion-key-at-point-functions'."
    (cl-some #'funcall bibtex-completion-key-at-point-functions))

  (defun lps/bibtex-completion-current-pdf-key ()
    (when (and (derived-mode-p 'doc-view-mode 'pdf-view-mode)
               buffer-file-name
               (or (and (file-directory-p bibtex-completion-library-path)
                        (file-equal-p bibtex-completion-library-path
                                      (file-name-directory buffer-file-name)))
                   (and (listp bibtex-completion-library-path)
                        (-some (lambda (f)
                                 (file-equal-p f
                                               (file-name-directory
                                                buffer-file-name)))
                               bibtex-completion-library-path))))
      (file-name-sans-extension (file-name-nondirectory buffer-file-name))))

  (add-to-list 'bibtex-completion-key-at-point-functions 'org-ref-read-key t)

  ;; bibtex-completion-get-value strips too many brackets
  (defun lps/bibtex-completion-fix-stripped-brackets (s)
    (cl-loop with cur = 0
             minimize cur into min
             for c across s
             when (= c ?{) do (setf cur (1+ cur))
             when (= c ?}) do (setf cur (1- cur))
             finally (return (concat (make-string (- min) ?{)
                                     s
                                     (make-string (- cur min) ?})))))

  ;; We reverse engineer the "safe ascii encoding" of "special characters"
  (defun lps/bibtex-format-undo-nonascii (s)
    (replace-regexp-in-string
     "{\\\\.\\({.}\\|.\\)}"
     (lambda (match)
       (or (car (rassoc (regexp-quote match)
                        org-ref-nonascii-latex-replacements))
           (car (rassoc (regexp-quote (concat
                                       (substring match 0 -2)
                                       "{"
                                       (substring match -2)
                                       "}"))
                        org-ref-nonascii-latex-replacements))
           (regexp-quote match)))
     (lps/bibtex-completion-fix-stripped-brackets s)))

  ;; Override this, only ever used as an interfance: cannot break internal stuff
  (defun-override lps/bibtex-completion-clean-string (s)
    (if s
        (->> s
             (lps/bibtex-format-undo-nonascii)
             (replace-regexp-in-string "[\"{}]+" "")
             (replace-regexp-in-string "[\n\t ]+" " "))
      nil))

  ;; Improve visual look of org-ref-read-key
  (defvar lps/bibtex-completion-format-entry-properties
    '(("author" face (:inherit message-header-to :weight normal))
      ("date" face font-lock-variable-name-face)
      ("year" face font-lock-variable-name-face)
      ("tags" face font-lock-type-face))
    "Alist of (FIELD-NAME (PROPERTIES)*).
The properties will be applied as if by
(apply 'propertize STRING PROPERTIES)")

  ;; Copy of the original function
  ;; It simply propertizes every field by using the alist
  ;; lps/bibtex-completion-format-entry-properties
  (defun-override lps/bibtex-completion-format-entry (entry width)
    "Formats a BibTeX ENTRY for display in results list.
WIDTH is the width of the results list.  The display format is
governed by the variable `bibtex-completion-display-formats'."
    (let* ((format
            (or (assoc-string (bibtex-completion-get-value "=type=" entry)
                              bibtex-completion-display-formats-internal
                              'case-fold)
                (assoc t bibtex-completion-display-formats-internal)))
           (format-string (cadr format)))
      (s-format
       format-string
       (lambda (field)
         (let* ((field (split-string field ":"))
                (field-name (car field))
                (field-width (cadr field))
                (field-value (bibtex-completion-get-value field-name entry)))
           (when (and (string= field-name "author")
                      (not field-value))
             (setq field-value (bibtex-completion-get-value "editor" entry)))
           (when (and (string= field-name "year")
                      (not field-value))
             (setq field-value (car (split-string (bibtex-completion-get-value "date" entry "")
                                                  "-"))))
           (setq field-value (bibtex-completion-clean-string (or field-value " ")))
           (when (member field-name '("author" "editor"))
             (setq field-value (bibtex-completion-shorten-authors field-value)))
           (apply 'propertize
                  (if (not field-width)
                      field-value
                    (setq field-width (string-to-number field-width))
                    (truncate-string-to-width
                     field-value
                     (if (> field-width 0)
                         field-width
                       (- width (cddr format)))
                     0 ?\s))
                  (cdr (assoc field-name lps/bibtex-completion-format-entry-properties)))))))))

(use-package doi-utils
  :after org-ref-bibtex
  :ensure nil
  :init
  (defvar doi-utils-pdf-url-functions-from-doi '(doi-to-arxiv-pdf
                                                 doi-to-hal-pdf))
  ;; Redefine here, superset
  (setq org-ref-lower-case-words lps/do-not-capitalize-list)
  :custom
  (doi-utils-download-pdf nil)          ; buggy
  (doi-utils-async-download )           ; buggy
  (doi-utils-open-pdf-after-download t)
  :config
  (advice-ensure-bindings org-ref-read-key ((orderless-matching-styles
                                             (cons 'char-fold-to-regexp
                                                   orderless-matching-styles))))

  ;; Define custom functions to download from math websites
  (defun aom-pdf-url (*doi-utils-redirect*)
    "Get a url to the pdf from *DOI-UTILS-REDIRECT* for Annals of Mathematics urls.
  A URL looks like https://annals.math.princeton.edu/YEAR/VOL-N/PAGE
  The PDF is typically (but not always ...) at
   https://annals.math.princeton.edu/wp-content/uploads/annals-vVOL-nN-PAGE-s.pdf"
    (when (string-match
           "^\\(https?://annals.math.princeton.edu\\)/\\([0-9]*\\)/\\([0-9]*\\)-\\([0-3]\\)/\\(p[0-9]*\\)"
           *doi-utils-redirect*)
      (let ((start (match-string-no-properties 1 *doi-utils-redirect*))
            (year (match-string-no-properties 2 *doi-utils-redirect*))
            (vol (match-string-no-properties 3 *doi-utils-redirect*))
            (issue (match-string-no-properties 4 *doi-utils-redirect*))
            (page (match-string-no-properties 5 *doi-utils-redirect*)))
        (concat start "/wp-content/uploads/annals-v"
                vol "-n" issue "-" page "-s.pdf"))))

  (defun doi-utils-get-ems-pdf-url (*doi-utils-redirect*)
    (let ((first-url
           (with-current-buffer (url-retrieve-synchronously *doi-utils-redirect*)
             (goto-char (point-min))
             (when (re-search-forward "/content/serial-article-files/[0-9]+" nil t)
               (match-string-no-properties 0)))))
      (when first-url
        (concat "https://ems.press" first-url))))

  (defun ems-pdf-url (*doi-utils-redirect*)
    "Get a url to the pdf from *DOI-UTILS-REDIRECT* for EMS (European
Mathematical Society) urls.
  A URL looks like https://ems.press/journals/...
  The PDF url is hidden in the page content"
    (when (string-match "^https?://ems.press/journals/" *doi-utils-redirect*)
      (doi-utils-get-ems-pdf-url *doi-utils-redirect*)))

  (add-to-list 'doi-utils-pdf-url-functions 'aom-pdf-url)
  (add-to-list 'doi-utils-pdf-url-functions 'ems-pdf-url)

  ;; Override to also use another list of functions
  (defun-override lps/doi-utils-get-pdf-url-from-anywhere (doi)
    "Return a url to a pdf for the DOI if one can be calculated.
Loops through the functions in `doi-utils-pdf-url-functions'
until one is found.
If none is found, loops through the functions in
`doi-utils-pdf-url-functions-from-doi' afterwards."
    (doi-utils-get-redirect doi)

    (unless *doi-utils-redirect*
      (error "No redirect found for %s" doi))
    (catch 'pdf-url
      ;; Try to find URL "properly" using per-journal info
      (dolist (func doi-utils-pdf-url-functions)
        (let ((this-pdf-url (funcall func *doi-utils-redirect*)))
          (when this-pdf-url
            (throw 'pdf-url this-pdf-url))))
      ;; Now more coarse methods, using the DOI on several
      ;; archives (arxiv, HAL ...)
      (dolist (func doi-utils-pdf-url-functions-from-doi)
        (let ((this-pdf-url (funcall func doi)))
          (when this-pdf-url
            (throw 'pdf-url this-pdf-url))))))

  (defun doi-to-arxiv-pdf (doi)
    (let* ((doi1 (url-hexify-string doi))
           (doi2 (url-hexify-string (upcase doi)))
           (num-regexp "http\\(s\\)?://arxiv.org/pdf/\\([0-9.]+\\)")
           (pdf-url-maybe
            (or
             (with-current-buffer (url-retrieve-synchronously
                                   (concat "https://arxiv.org/search/?query="
                                           doi1
                                           "&searchtype=doi"))
               (goto-char (point-min))
               (when (re-search-forward num-regexp nil t)
                 (match-string-no-properties 0)))
             (with-current-buffer (url-retrieve-synchronously
                                   (concat "https://arxiv.org/search/?query="
                                           doi2
                                           "&searchtype=doi"))
               (goto-char (point-min))
               (when (re-search-forward num-regexp nil t)
                 (match-string-no-properties 0))))))
      (when pdf-url-maybe
        (concat pdf-url-maybe ".pdf"))))

  (defun doi-to-hal-pdf (doi)
    (let* ((doi (url-hexify-string doi))
           (num-regexp "/hal-[0-9]+\\(v[0-9]+\\)/document")
           (pdf-url-maybe
            (with-current-buffer
                (url-retrieve-synchronously (concat "https://hal.science/search/index/?qa[doiId_id][]="
                                                    doi))
              (goto-char (point-min))
              (when (re-search-forward num-regexp nil t)
                (match-string-no-properties 0)))))
      (when pdf-url-maybe
        (concat "https://hal.science" pdf-url-maybe)))))

(use-package org-ref
  :defer t
  :config
  (require 'bibtex-completion))

(use-package org-ref-bibtex
  :defer t
  :ensure nil
  :commands org-ref-bibtex-hydra/body
  :bind
  ("C-c b" . org-ref-bibtex-hydra/body)
  (:map lps/all-hydras-map
        ("p" . org-ref-bibtex-hydra/body))
  :custom
  (org-ref-title-case-types '(("misc" "title")
                              ("inproceedings" "title")
                              ("phdthesis" "title")
                              ("article" "title")
                              ("book" "booktitle" "title")
                              ("inbook" "booktitle" "title")))
  :config
  (org-roam-bibtex-mode 1)

  (setq org-ref-clean-bibtex-entry-hook
        (remove 'orcb-check-journal org-ref-clean-bibtex-entry-hook))

  (add-to-list 'org-ref-clean-bibtex-entry-hook 'org-ref-title-case)

  (dolist (pair org-ref-nonascii-latex-replacements)
    (let ((in (car pair))
          (out (cdr pair)))
      (when-let ((new-in (and (= 2 (car (aref (syntax-table) (aref in 0))))
                              (s-upcase in))))
        (unless (or (string-equal in new-in)
                    (cl-every (lambda (char)
                                (eq 'ascii (char-charset char)))
                              new-in)   ; approximation ...
                    )
          (let ((new-out (with-temp-buffer
                           (insert out)
                           (capitalize-word -1)
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
            (cl-pushnew (cons new-in new-out)
                        org-ref-nonascii-latex-replacements))))))

  ;; Add a few things to the default hydra BIG Hack with eval:
  ;; otherwise, defhydra+ tries to expand, and it needs to know whta
  ;; org-ref-bibtex-hydra *is* ... but it can't before the package is
  ;; loaded
  (eval '(progn
           (defhydra+ org-ref-bibtex-hydra (:color blue :hint nil)
             "Bibtex actions:"
             ("p" lps/org-ref-open-bibtex-pdf "PDF" :column "Open" :exit t)
             ("n" lps/org-ref-open-bibtex-notes "Notes" :column "Open" :exit t)
             ("b" lps/org-ref-open-in-browser "URL" :column "Open" :exit t)
             ("B" (lambda ()
                    (interactive)
                    (bibtex-completion-show-entry (list (org-ref-read-key))))
              "Show entry"
              :column "Navigation"
              :color "red")
             ("e" org-ref-email-add-pdf "Email PDF only" :column "WWW")
             ("E" org-ref-email-bibtex-entry "Email PDF and bib entry" :column "WWW")
             ("]" org-ref-bibtex-next-entry "Next entry" :column "Navigation" :color red)
             ("[" org-ref-bibtex-previous-entry "Previous entry" :column "Navigation" :color red))
           ;; Add a command to search more sources at once
           ;; (defhydra+ org-ref-bibtex-new-entry (:color blue)
           ;;   "New Bibtex entry:"
           ;;   ("N" doi-utils-get-pdf-url-from-anywhere "from anywhere"
           ;;    :column "Automatic"))
           ))

  (defun lps/org-ref-email-add-pdf--internal (keys)
    "Does the real work of `org-ref-email-add-pdf'
KEYS is either a list of keys, or a single key, in which
case it has to be a string."
    (let ((bufs (gnus-dired-mail-buffers)))
      (unless (and (not bufs)
                   (not (and (y-or-n-p "No composition buffer. Compose new mail ?")
                             (compose-mail)
                             (setq bufs (gnus-dired-mail-buffers)))))
        (dolist (key (ensure-list keys))
          (let* ((pdf (car (bibtex-completion-find-pdf key)))
                 (buf (if (= (length bufs) 1)
                          (get-buffer (car bufs))
                        (gnus-completing-read "Attach to buffer"
                                              bufs t nil nil (car bufs)))))
            (when (or pdf
                      (and (y-or-n-p
                            (format "No pdf for Bibtex key %s. Find manually ?"
                                    key))
                           (setq pdf (read-file-name "Attach PDF: "))))
              (set-buffer buf)
              (goto-char (point-max))
              (mml-attach-file pdf (or (mm-default-file-type pdf)
                                       "application/octet-stream")
                               "attachment")
              (message "Attached file %s" pdf)))))))

  (defun org-ref-email-add-pdf ()
    (interactive)
    (lps/org-ref-email-add-pdf--internal (list (bibtex-completion-key-at-point))))

  ;; Redefine to use the interface that *should* be used ...
  (defun lps/org-ref-open-bibtex-pdf ()
    "Open pdf for a bibtex entry, if it exists."
    (interactive)
    (bibtex-completion-open-pdf (list (bibtex-completion-key-at-point))))

  (defun lps/org-ref-open-bibtex-notes ()
    "From a bibtex entry, open the notes if they exist."
    (interactive)
    (bibtex-completion-edit-notes (list (bibtex-completion-key-at-point))))

  (defun lps/org-ref-open-in-browser ()
    "Open the bibtex entry at point in a browser using the url field or doi field."
    (interactive)
    (bibtex-completion-open-url-or-doi (list (bibtex-completion-key-at-point))))

  (defun lps/org-ref-read-keys-multiple ()
    "Read keys with completion.
Each entry has to be separated by a slash /, as commas might be
present in the list of authors or in the title of the article"
    (unless bibtex-completion-display-formats-internal
      (bibtex-completion-init))
    (let* ((bibtex-completion-bibliography (org-ref-find-bibliography))
           (candidates
            (mapcar
             ;; reduce format length: if multiple entries,
             ;; gets really annoying to read otherwise
             (lambda (entry)
               (cons (bibtex-completion-format-entry entry
                                                     (- (frame-width) 30))
                     (cdr entry)))
             (bibtex-completion-candidates)))
           (choices (let ((crm-separator "[ ]*/[ ]*"))
                      (completing-read-multiple "BibTeX entries: "
                                                candidates))))
      (mapcar (lambda (choice)
                (cdr (assoc "=key=" (assoc choice candidates))))
              choices)))

  (add-to-list 'org-ref-clean-bibtex-entry-hook 'bibtex-clean-entry t)

  ;; Ensure org-ref-replace-nonascii does not case fold
  (advice-ensure-bindings org-ref-replace-nonascii ((case-fold-search nil))))

(use-package org-ref-arxiv
  :ensure nil
  :after (:or org-ref elfeed)
  :config
  (require 'bibtex)
  (setq arxiv-entry-format-string
        "\n@misc{%s,
  title =         {%s},
  author =        {%s},
  archivePrefix = {arXiv},
  year =          {%s},
  eprint =        {%s},
  primaryClass =  {%s},
}\n"))

(use-package org-roam-bibtex
  :bind
  ("C-&" . lps/org-roam-bibtex-dispatch)
  :custom
  (orb-note-actions-user
   '(("Open or create note" . orb-bibtex-completion-edit-note)
     ("Add to email" . lps/org-ref-email-add-pdf--internal)
     ("Act on other file" . lps/org-roam-bibtex-dispatch-change-file)))
  (orb-note-actions-interface 'hydra)
  :config
  (add-to-list
   'org-roam-capture-templates
   '("r"
     "bibliography reference" plain "* %?"
     :target (file+head "articles-notes/%<%Y%m%d%H%M%S>-${citekey}.org"
                        "#+title: ${title}
#+filetags: :phd:
#+startup: latexpreview")
     :unnarrowed t
     :empty-lines-before 1
     :kill-buffer t
     :hook (org-cdlatex-mode lps/org-roam-capture-set-category))
   t #'equal)

  (defun lps/org-roam-capture-set-category ()
    (save-excursion
      (goto-char (point-min))
      (org-set-property "CATEGORY" "PhDResearch")))

  (defun lps/org-roam-bibtex-dispatch-change-file (citekey)
    (ignore citekey)
    (lps/org-roam-bibtex-dispatch (org-ref-read-key)))

  (defun lps/org-roam-bibtex-dispatch (citekey)
    (interactive (list (or (lps/bibtex-completion-current-pdf-key)
                           (bibtex-completion-key-at-point))))
    (orb-note-actions--run orb-note-actions-interface citekey)))

(use-package biblio-list
  :ensure nil
  :defer t
  :bind
  ("C-c B" . biblio-list-bibliography)
  :config
  (require 'bibtex)
  (require 'bibtex-completion)
  (setq biblio-list-files lps/bib-bibliography-files)
  (setq biblio-list-library-path (ensure-list bibtex-completion-library-path)))

(use-package preview
  :ensure nil ;; Comes with AUCTeX
  :defer t
  :config
  (setq preview-auto-reveal t)
  (setq preview-auto-cache-preamble t)
  (setq preview-scale-function 1.5) ; hack ? Find a better way
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t)

  ;; Dirty way to reduce noise when idle-y
  (defun preview-active-string (ov)
    "Generate before-string for active image overlay OV."
    (preview-make-clickable
     (overlay-get ov 'preview-map)
     (car (overlay-get ov 'preview-image))
     ;; "%s opens text
     ;; %s more options"
     "")))

(use-package cdlatex
  :defer t
  :hook
  (cdlatex-tab . lps/LaTeX-indent)
  :bind
  (:map cdlatex-mode-map
        ("C-c ?" . nil)
        ("<C-return>" . lps/TeX-fast-insert-macro)
        ("<backtab>" . cdlatex-backtab))
  :custom
  (cdlatex-paired-parens "$([{")
  (cdlatex-make-sub-superscript-roman-if-pressed-twice t)
  (cdlatex-simplify-sub-super-scripts nil)
  (cdlatex-math-modify-prefix "C-°")
  (cdlatex-takeover-dollar nil)
  (cdlatex-auto-help-delay 1.0)
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-math-symbol-prefix ?°)
  (cdlatex-math-symbol-alist
   '((?< ("\\leq" "\\iff"))
     (?> ("\\geq" "\\implies"))
     (?\[ ("\\subseteq" "\\sqsubseteq" "\\sqsubset"))
     (?\] ("\\supseteq" "\\sqsupseteq" "\\sqsupset"))
     (?: ("\\colon"))
     (?  ("\\," "\\:" "\\;"))
     (?- ("\\cap" "\\bigcap"))
     (?+ ("\\cup" "\\bigcup" "\\sqcup" "\\bigsqcup"))
     (?v ("\\vee" "\\bigvee"))
     (?& ("\\wedge" "\\bigwedge"))
     (?* ("\\times" "\\ast"))
     (?L ("\\Lambda" "\\limits"))
     (?c ("\\mathcal{?}" "\\mathbb{?}" "\\mathfrak{?}"))
     (?\( ("\\langle ?\\rangle" "\\left"))
     (?N ("\\mathbb{N}" "\\mathbb{N}^{2}"))
     (?Z ("\\mathbb{Z}" "\\mathbb{Z}^{2}" "\\Zeta"))
     (?R ("\\mathbb{R}" "\\mathbb{R}^{2}"))
     (?1 ("^{-1}"))
     (?\; ("\\dots" "\\vdots" "\\ddots" "\\ldots"))
     (?\C-f ("\\to" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
     (?\C-b ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
     (?\C-p ("\\uparrow" "\\Uparrow" "\\longuparrow" "\\Longuparrow"))
     (?\C-n ("\\downarrow" "\\Downarrow" "\\longdownarrow" "\\Longdownarrow"))))
  (cdlatex-command-alist
   '(("prodl"       "Insert \\prod_{}^{}"
      "\\prod_{?}^{}" cdlatex-position-cursor nil nil t)
     ("suml"        "Insert \\sum_{}^{}"
      "\\sum_{?}^{}" cdlatex-position-cursor nil nil t)
     ("intl"        "Insert \\int_{}^{}"
      "\\int_{?}^{}" cdlatex-position-cursor nil nil t)
     ("lim"         "Insert \\lim_{}"
      "\\lim_{?}" cdlatex-position-cursor nil nil t)
     ("invlim"      "Insert \\varprojlim_{}{}"
      "\\varprojlim_{?}{}" cdlatex-position-cursor nil nil t)
     ("dirlim"      "Insert \\varinjlim_{}{}"
      "\\varinjlim_{?}{}" cdlatex-position-cursor nil nil t)))

  :config
  (defun lps/LaTeX-indent ()
    (if (use-region-p)
        (progn
          (indent-region (region-beginning) (region-end))
          t)
      (let ((old-indent (current-indentation)))
        (funcall indent-line-function)
        (/= old-indent (current-indentation)))))

  (defun lps/cdlatex-indent-after-expand (fun &rest args)
    (let ((beg (point))
          (end (point-marker)))
      (set-marker-insertion-type end t)
      (apply fun args)
      (indent-region beg end)))

  (advice-add 'cdlatex-environment :around #'lps/cdlatex-indent-after-expand)

  (defun lps/cdlatex-sub-superscript-wrap-region (fun &rest args)
    (if (region-active-p)
        (let* ((beg  (region-beginning))
               (end (region-end))
               (region-string (buffer-substring beg end)))
          (delete-region beg end)
          (apply fun args)
          (insert region-string))
      (apply fun args)))

  (advice-add 'cdlatex-sub-superscript
              :around #'lps/cdlatex-sub-superscript-wrap-region)

  (defun lps/TeX-fast-insert-macro ()
    "Insert a macro like `TeX-insert-macro' does, but pre-fill the
prompt with the word at point, using flex matching."
    (interactive)
    (let* ((completion-extra-properties
            (list :annotation-function
                  #'TeX--completion-annotation-function))
           (orderless-matching-styles '(orderless-flex))
           (word (thing-at-point 'word))
           (bounds (bounds-of-thing-at-point 'word))
           (symbol (completing-read (concat "Macro (default "
                                            TeX-default-macro
                                            "): "
                                            TeX-esc)
                                    (TeX--symbol-completion-table) nil nil word
                                    'TeX-macro-history TeX-default-macro)))
      (when (called-interactively-p 'any)
        (setq TeX-default-macro symbol))
      (atomic-change-group
        (TeX-parse-macro symbol (cdr-safe (assoc symbol (TeX-symbol-list))))
        (when bounds
          (delete-region (car bounds) (cdr bounds)))
        (run-hooks 'TeX-after-insert-macro-hook))))

  (defun cdlatex-backtab ()
    "Similar in spirit to `cdlatex-tab'.

Obtained by:

- flipping opening/closing delimiters

- changing the signs of arguments given to `forward-char'

- `looking-at' <-> `looking-back'

and similar changes.

Does not run `cdlatex-tab-hook.'"
    (interactive)
    (catch 'stop
      ;; Check for simplification of sub and superscripts
      (cond
       ((looking-back "{\\|\\[\\|(")
        (forward-char -1)
        (if (looking-back "[^_^({[]")
            ;; stop after closing bracket, unless ^_[{( follow
            (throw 'stop t)))
       ((= (preceding-char) ?$)
        (while (= (preceding-char) ?$) (forward-char -1))
        (throw 'stop t))
       ((= (preceding-char) ?\ )
        ;; stop after first of many spaces
        (forward-char -1)
        (re-search-backward "[^ ]")
        (if (/= (following-char) ?\n) (forward-char 1)))
       (t
        (forward-char -1)))

      ;; move to next possible stopping site and check out the place
      (while (re-search-backward "[ ({\n]\\|\\[" (point-min) t)
        (forward-char 1)
        (cond
         ((= (preceding-char) ?\ )
          ;; stop at first space or b-o-l
          (if (not (eolp)) (forward-char -1)) (throw 'stop t)) ; unsure here ?
         ((= (preceding-char) ?\n)
          ;; stop at line end, but not after \\
          (if (and (eolp) (not (bobp)))
              (throw 'stop t)
            (if (equal "\\\\" (buffer-substring-no-properties
                               (point) (+ 2 (point))))
                (forward-char -1)
              (throw 'stop t))))
         (t
          ;; Stop before )}] if preceding-char is any parenthesis
          (if (or (= (char-syntax (following-char)) ?\()
                  (= (char-syntax (following-char)) ?\))
                  (= (following-char) ?-))
              (throw 'stop t)
            (forward-char -1)
            (if (looking-at "[^]_^)}]")
                ;; stop after closing bracket, unless ^_[{( follow
                (throw 'stop t)))))))))

(use-package latex-table-wizard
  :defer t
  :bind
  (:map TeX-mode-map
        ("C-|" . latex-table-wizard)))

;; eshell
(use-package eshell-did-you-mean
  :hook (eshell-mode . eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell
  :ensure nil
  :defer t
  :custom
  ;; (eshell-prefer-lisp-variables t)
  ;; (eshell-prefer-lisp-functions t)
  (eshell-hist-ignoredups t)
  (eshell-history-size 1024)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt t)
  (eshell-prompt-function #'lps/eshell-prompt-function)
  (eshell-prompt-regexp "^[^#$\n]* [#$] ")

  :bind
  (:map lps/system-tools-map
        ("e" . eshell))
  (:map eshell-hist-mode-map
        ("C-c C-l" . nil)
        ("C-c M-l" . eshell-list-history))
  (:map eshell-mode-map
        ("C-c C-l" . eshell/clear))

  :config
  (require 'em-hist)
  ;; From https://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/
  (defun lps/eshell-pwd-repl-home (pwd)
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))

  (defun lps/eshell-remote-repl-info (remote)
    ;; Might not handle username/remote name with @ or : symbols in them
    ;; if such a thing is even possible
    ;; For a cleaner implementation, use `tramp-dissect-file-name'
    ;; or `with-parsed-tramp-file-name'
    (let* ((method-end (string-search ":" remote))
           (method (substring remote 1 method-end))
           (user (substring remote (1+ method-end) -1))
           (len-user (length user))
           (shortened-user (if (> len-user 13)
                               (concat (substring user 0 6)
                                       "…"
                                       (substring user -6))
                             user)))
      (concat "[" method "|" shortened-user "]")))

  (defun lps/eshell-abbreviate-short-dir-name (name)
    (if (zerop (length name))
        ""
      (substring name 0 1)))

  (defun lps/eshell-abbreviate-long-path (path)
    (let ((split-path (split-string path "/")))
      (if (> (length split-path) 3)
          (concat
           (mapconcat #'lps/eshell-abbreviate-short-dir-name
                      (butlast split-path 3)
                      "/")
           "/"
           (mapconcat #'identity (last split-path 3) "/"))
        path)))

  ;; See the possible colours: M-x list-colors-display
  (defun lps/eshell-curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (if (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (propertize (concat "["
                            (if (> (length git-output) 0)
                                (substring git-output 0 -1)
                              "(no branch)")
                            "]")
                    'face `(:foreground "green3")))
      ""))

  (defun lps/eshell-prompt-function ()
    (let* ((eshell-pwd (eshell/pwd))
           (remote (file-remote-p eshell-pwd)))
      (when remote
        (setq eshell-pwd (substring eshell-pwd (length remote))))
      (concat
       (if remote
           (propertize (lps/eshell-remote-repl-info remote)
                       'face '(:foreground "light slate blue"))
           "")
       (propertize (lps/eshell-abbreviate-long-path (lps/eshell-pwd-repl-home eshell-pwd))
                   'face `(:foreground "DeepSkyBlue1"))
       (unless remote
         (propertize (lps/eshell-curr-dir-git-branch-string eshell-pwd)
                     'face `(:foreground "green3")))
       (propertize " # " 'face 'default)))))

;; (use-package eshell-git-prompt
;;   :config (eshell-git-prompt-use-theme 'powerline)) ;; Visually buggy

(system-case
 (gnu/linux
  (use-package fish-completion
    :defer t
    :hook (eshell-mode . lps/start-fish-completion)
    :config
    (defun lps/start-fish-completion ()
      (when (executable-find "fish")
        (setq-local company-backends '(company-capf))
        (define-key eshell-mode-map (kbd "TAB") 'company-manual-begin)
        (fish-completion-mode 1)
        (setq fish-completion-fallback-on-bash-p t))))))

;; Inspired from https://github.com/daviderestivo/load-bash-alias/blob/master/load-bash-alias.el
;; WARNING: it is not very robust, and might mess up if bash aliases involve
;; complex nested single or double quotes !
(defun lps/eshell-load-bash-aliases ()
  "Reads bash aliases from ~/.bashrc and inserts
      them into the list of eshell aliases."
  (interactive)
  (let ((bashfile "~/.bashrc"))
    (if (file-exists-p bashfile)
        (with-temp-buffer
          (progn
            (insert-file-contents bashfile)
            ;; Merge continuation lines into single line. The below regexp
            ;; matches a '\' at the end of a line followed by one or
            ;; multiple TAB or spaces.
            (while (re-search-forward "\\\\[ \t]*\n" nil t)
              (replace-match ""))
            ;; Return a list of lines
            (let* ((bashfile-lines (split-string (buffer-string) "\n" t))
                   (bashfile-aliases (cl-remove-if-not (lambda (str)
                                                         (string-match-p "^alias" str))
                                                       bashfile-lines)))
              (dolist (line bashfile-aliases)
                (let* ((trimmed (replace-regexp-in-string "=\\|[ \t]+" " " line))
                       (alias-def (string-trim-left trimmed "^alias "))
                       (first-split (string-search " " alias-def))
                       (alias-name (substring alias-def 0 first-split))
                       (alias-definition-1 (substring alias-def (1+ first-split)))
                       (rem-quotes-regexp "['\"]")
                       (alias-definition-trimmed (string-trim alias-definition-1
                                                              rem-quotes-regexp
                                                              rem-quotes-regexp)))
                  (eshell/alias alias-name (eshell-flatten-and-stringify alias-definition-trimmed)))))))
      (message "File ~/.bashrc not found, no aliases were loaded"))))

(system-case
 (gnu/linux
  (add-hook 'eshell-mode-hook 'lps/eshell-load-bash-aliases)))

(use-package em-alias
  :ensure nil
  :hook (eshell-mode . lps/eshell-add-aliases)
  :config
  (defun lps/eshell-add-aliases ()
    (eshell/alias "f" "find-file $1")
    (eshell/alias "fo" "find-file-other-window $1")
    (eshell/alias "d" "dired $1")
    ;; Used for sudo and some other commands
    (require 'em-tramp)
    (eshell/alias "sudo" "eshell/sudo $*")))

(use-package dired
  :ensure nil
  :only-built-in t
  :defer t
  :bind
  (:map dired-mode-map
        ("F" . find-name-dired)
        ([remap ibuffer] . lps/ibuffer-dired-current-directory)
        ("<C-return>" . dired-do-open))
  :custom
  ;; Delete and copy directories recursively
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-isearch-filenames 'dwim)
  (dired-listing-switches "-l --almost-all --human-readable --classify --group-directories-first")
  (wdired-allow-to-change-permissions t)
  (dired-dwim-target t)
  :config
  (defun lps/ibuffer-dired-current-directory (&optional other-window-p)
    (interactive "P")
    (let ((dir (dired-current-directory)))
      (ibuffer other-window-p nil `((directory . ,dir))))))

(use-package dired-x
  :ensure nil
  :only-built-in t
  :after dired)

;; Make things prettier
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package dired-git-info
  :after dired
  :bind
  (:map dired-mode-map
        (")" . dired-git-info-mode))
  :custom
  (dgi-auto-hide-details-p nil)
  :config
  (set-face-attribute 'dgi-commit-message-face nil
                      :inherit 'font-lock-constant-face
                      :slant 'italic))

(use-package find-dired
  :ensure nil
  :bind
  (:map lps/system-tools-map
        ("f f" . find-name-dired)
        ("f g" . find-grep-dired)
        ("f l" . locate)
        ("f L" . locate-with-filter)))

(system-case
 (gnu/linux
  (use-package dirvish
    :after dired
    :bind
    (:map dirvish-mode-map
          ("TAB" . dirvish-subtree-toggle)
          ("a" . dirvish-quick-access)
          ("b" . dirvish-history-go-backward)
          ("f" . dirvish-history-go-forward)
          ("z" . dirvish-layout-toggle)
          ("h" . dirvish-history-jump)
          ("/" . dirvish-fd)
          ([remap dired-sort-toggle-or-edit] . dirvish-quicksort)
          ([remap dired-do-relsymlink] . dirvish-yank-menu) ; to be called from TARGET of symlink
          ([remap dired-show-file-type] . dirvish-file-info-menu))
    :init
    (dirvish-override-dired-mode)
    :custom
    (dirvish-hide-details nil)
    (dirvish-hide-cursor nil)
    (dirvish-use-mode-line t)
    (dirvish-subtree-state-style 'arrow)
    (dirvish-subtree-always-show-state t)
    (dirvish-mode-line-height 15)       ; same as doom-modeline, see above
    (dirvish-mode-line-format '(:left
                                (sort symlink vc-info)
                                :right
                                (omit yank index " " free-space)))
    (dirvish-attributes '(file-size file-time subtree-state all-the-icons))
    (dirvish-show-media-properties t)
    (dirvish-quick-access-entries `(("h" "~/" "Home")
                                    ("e" "~/.config/emacs/" "Emacs user directory")
                                    ("t"  "~/.local/share/Trash/" "Trashes")
                                    ("d"  "~/.dotfiles" "Dotfiles")
                                    ("m"  "/media/" "Mounting directory")
                                    ("oo" ,org-directory "Org Files")
                                    ("oa" ,(lps/org-expand-file-name "agenda" t) "Org Files")
                                    ,@(let (shortcuts)
                                        (dolist (key-name '(("B" "DESKTOP")
                                                            ("T" "DOWNLOAD")
                                                            ("D" "DOCUMENTS")
                                                            ("M" "MUSIC")
                                                            ("P" "PICTURES")
                                                            ("V" "VIDEOS")))
                                          (let* ((key (car key-name))
                                                 (name (cadr key-name))
                                                 (path (xdg-user-dir name))
                                                 (desc (capitalize name)))
                                            (push (list key path desc) shortcuts)))
                                        (nreverse shortcuts))))
    :config
    (remove-hook 'dired-mode-hook 'all-the-icons-dired-mode)

    ;; Swap the meaning of prefix arg
    (defun-override lps/dirvish-yank--read-dest (method)
      "Read dest dir for METHOD when prefixed with `current-prefix-arg'."
      (list (unless current-prefix-arg  ; was 'when current-prefix-arg'
              (read-file-name (format "%s files to: " method)
                              (dired-dwim-target-directory)
                              nil nil nil 'file-directory-p))))

    ;;;; Add a colourful directory preview.
    ;;;; Not really needed and somewhat noisy ...
    ;; (dirvish-define-preview exa (file)
    ;;   "Use `exa' to generate directory preview."
    ;;   :require ("exa")             ; tell Dirvish to check if we have the executable
    ;;   (when (file-directory-p file)         ; we only interest in directories here
    ;;     `(shell . ("exa" "-al" "--color=always" "--icons"
    ;;                "--group-directories-first" ,file))))

    ;; (add-to-list 'dirvish-preview-dispatchers 'exa)
    )))

(use-package tramp
  :bind
  ("C-x C-S-f" . lps/tramp-find-file)
  :hook
  (shell-mode . lps/remote-shell-setup)
  :config
  (defun lps/tramp-find-file (&optional method user host file)
    (interactive (list (completing-read "Method: "
                                        tramp-methods
                                        nil t nil nil "-" t)
                       (read-string "User: "
                                    nil nil nil t)
                       (read-string "Host: "
                                    nil nil nil t)
                       (read-string "File: "
                                    nil nil nil t)))
    (let ((port (when (member method '("ssh" "sshx"))
                  (read-string "Port: "))))
      (find-file (concat "/" method ":"
                         (unless (string-empty-p user)
                           (concat user "@"))
                         host
                         ":"
                         (when port
                           (concat "#" port))
                         file))))

  (defun lps/remote-shell-setup ()
    (when (and (fboundp 'company-mode)
               (file-remote-p default-directory))
      (company-mode -1))))

(use-package conf-mode
  :hook
  (conf-mode . rainbow-mode))

(use-package disk-usage
  :defer t
  :bind
  (:map lps/system-tools-map
        ("d" . disk-usage)))

(use-package proced
  :ensure nil
  :only-built-in t
  :bind
  (:map lps/system-tools-map
        ("p p" . proced))
  (:map proced-mode-map
        ("a" . proced-toggle-auto-update))
  :custom
  (proced-goal-attribute nil)
  (proced-format 'medium)
  :config
  ;; Bug when sorting by pcpu (CPU usage): uses the string "%CPU" in
  ;; the mode-line, but %C is a control char, so it prints the current
  ;; column number rather than the literal %C !
  (ensure-emacs-version 29.1
    (setq proced-enable-color-flag t))

  ;; Small bug in French version of %b: does not produce
  ;; 6 characters-long time string for processes started earlier
  ;; in the same year
  (defun proced-format-start (start)
    "Format time START.
The return string is always 6 characters wide."
    (let ((d-start (decode-time start))
          (d-current (decode-time))
          (colon (if proced-enable-color-flag
                     (propertize ":" 'font-lock-face 'proced-time-colon)
                   ":")))
      (cond (;; process started in previous years
             (< (decoded-time-year d-start) (decoded-time-year d-current))
             (format-time-string "  %Y" start))
            ;; process started today
            ((and (= (decoded-time-day d-start) (decoded-time-day d-current))
                  (= (decoded-time-month d-start) (decoded-time-month d-current)))
             (string-replace ":" colon (format-time-string " %H:%M" start)))
            (t ;; process started this year
             (format-time-string " %e/%m" start))))))

(use-package emacs
  :ensure nil
  :bind
  (:map lps/system-tools-map
        ("p l" . list-processes)))

(use-package systemd)

(use-package transient-extras-lp
  :after pdf-tools
  :bind
  (:map lps/system-tools-map
        ("i" . transient-extras-lp-menu))
  (:map pdf-view-mode-map
        ("i i" . transient-extras-lp-menu)))

(use-package emacs-everywhere
  :defer t
  :custom
  ;; Disambiguate name for i3 window management, increase initial size
  (emacs-everywhere-frame-parameters `((name . "emacs-everywhere-init")
                                       (width . 100)
                                       (height . 20)))
  ;; Don't insert selection/spellcheck initially: we only write text
  (emacs-everywhere-init-hooks '(emacs-everywhere-set-frame-name
                                 ;; emacs-everywhere-set-frame-position ; don't override WM settings
                                 emacs-everywhere-apply-major-mode))
  :config
  ;; Try to robustify against content loss: bugs, etc
  (defvar emacs-everywhere-last-texts-ring (make-ring 10)
    "Last texts copied by `emacs-everywhere'")

  (defun emacs-everywhere-save-last-text ()
    (ring-insert
     emacs-everywhere-last-texts-ring
     (buffer-substring-no-properties (point-min) (point-max))))

  (defun emacs-everywhere-insert-last-text ()
    (interactive)
    (if (ring-empty-p emacs-everywhere-last-texts-ring)
        (message "Empty ring")
      (insert (ring-ref emacs-everywhere-last-texts-ring 0))))

  (define-key emacs-everywhere-mode-map (kbd "C-c C-y") 'emacs-everywhere-insert-last-text)

  (add-hook 'emacs-everywhere-final-hooks 'emacs-everywhere-save-last-text))

(use-package message
  :ensure nil
  :init
  ;; Useless as long as I have not configured GPG properly
  (defvar lps/safe-mail-send nil
    "If non-nil, ask for a signature, an encryption, and ask
confirmation when sending a non-multipart MIME mail")
  :bind
  (:map message-mode-map
        ([remap message-tab] . lps/message-tab))
  :hook
  (message-send . lps/sign-or-encrypt-message)
  :custom
  (read-mail-command 'mu4e)
  (mail-user-agent 'mu4e-user-agent)
  (message-send-mail-function 'smtpmail-send-it)
  (message-kill-buffer-on-exit t)
  ;; Adapted from https://jherrlin.github.io/posts/emacs-mu4e/
  ;; See also https://etienne.depar.is/emacs.d/mu4e.html
  (mml-secure-cache-passphrase nil)
  (mml-secure-openpgp-encrypt-to-self t)
  (mml-smime-encrypt-to-self t) ;; encryption is not ready yet
  (mm-sign-option 'guided)
  ;;(mml-secure-openpgp-sign-with-sender t)
  ;;(mml-secure-smime-sign-with-sender t)
  :config
  ;; Prefer text compared to html
  (add-to-list 'mm-discouraged-alternatives "text/html")

  (defun lps/message-tab ()
    (interactive nil message-mode)
    (lps/with-completing-read-in-region
        (message-tab)))

  (defun lps/sign-or-encrypt-message ()
    (when lps/safe-mail-send
      (let ((answer (read-from-minibuffer
                     (concat "Sign or encrypt?\n"
                             "Empty to do nothing.\n[s/e]: "))))
        (cond
         ((string-equal answer "s")
          (progn
            (message "Sign this message.")
            ;; Why doesn't mml-secure-message-sign-pgpmime work ... ?
            (mml-secure-message-sign-pgpmime) ;; Works but only signs a
            ;; part of the message.
            (message "Done trying to sign the message")))
         ((string-equal answer "e")
          (progn
            (message "Encrypt and sign this message.")
            (mml-secure-message-encrypt-pgpmime)
            (message "Done trying to encrypt the message")))
         (t (progn
              (message "Not signing or encrypting this message.")
              nil)))))))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :custom
  ;; Default SMTP configuration
  ;; Don't forget that it uses authinfo(.gpg) ! So any password change
  ;; should be reflected there too !
  (smtpmail-debug-info t)
  (smtpmail-smtp-user "paviets201")
  (smtpmail-smtp-server "smtp.unicaen.fr")
  (smtpmail-smtp-service 465)
  (smtpmail-stream-type 'ssl))

(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e" ;; Might be needed.
  :commands mu4e
  :bind (("C-c e" . mu4e)
         :map mu4e-compose-mode-map
         ("C-c h" . lps/org-mime-htmlize-preserve-secure-and-attach)
         (:map mu4e-main-mode-map
               ("q" . lps/mu4e-kill-buffers)
               ("Q" . mu4e-quit))
         (:map mu4e-search-minor-mode-map
               ("C-S-s" . lps/mu4e-build-query))
         (:map mu4e-view-mode-map
               ("A" . lps/mu4e-view-mime-part-action))
         (:map mu4e-headers-mode-map
               ("<backspace>" . lps/mu4e-unmark-backward)))
  :hook
  ;; Security issues
  (mu4e-main-mode . lps/auth-source-define-cache-expiry)
  ;; Improve appearance
  (mu4e-compose-mode . olivetti-mode)
  ;; Might avoid unwanted drafts
  (mu4e-compose-mode . lps/disable-auto-save-mode)
  :custom
  (mu4e-compose-context-policy 'always-ask)
  (mu4e-context-policy 'pick-first)
  (mu4e-confirm-quit nil)
  (mu4e-use-fancy-chars t)              ; ASCII-only time is over
  (mu4e-headers-precise-alignment t)    ; and fix alignment !
  (mu4e-compose-format-flowed t)
  (mu4e-change-filenames-when-moving t) ; Avoid mail syncing issues with mbsync
  ;; Refresh mail every 5 minutes
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-index-update-in-background t)
  (mu4e-hide-index-messages t)
  (mu4e-headers-date-format "%d-%m-%Y [%H:%M]") ; Always show full date and time
  (mu4e-search-threads t)                       ; Also show full message threads
  (mu4e-headers-include-related t)
  ;; Todo: fix so that it updates when window is resized
  (mu4e-headers-fields '((:human-date . 22)
                         (:flags . 6)
                         (:mailing-list . 12)
                         (:from-or-to . 22)
                         (:subject . 100)))
  ;; Improve completion
  (mu4e-completing-read-function 'completing-read)
  (mu4e-headers-auto-update nil)        ; somewhat confusing otherwise
  :config
  (defun lps/mu4e-unmark-backward ()
    (interactive)
    (mu4e-headers-prev)
    (let ((mu4e-headers-advance-after-mark nil))
      (mu4e-headers-mark-for-unmark)))

  (defun lps/mu4e-view-mime-part-action ()
    "Wrapper around `mu4e-view-mime-part-action' with better prompt

This is just a call to `mu4e-view-mime-part-action' in more
recent versions of mu4e."
    (interactive)
    (version-case mu4e-mu
      ("1.11" (call-interactively 'mu4e-view-mime-part-action))
      (t
       (let* ((parts (mu4e~view-gather-mime-parts))
              attachments)
         (cl-loop for part in parts
                  for num = (car part)
                  for fn = (or (cdr (assoc 'filename
                                           (assoc "attachment" (cdr part))))
                               (cl-loop for item in part
                                        for name = (and (listp item)
                                                        (assoc-default 'name item))
                                        thereis (and (stringp name) name)))
                  when fn
                  do (push (cons fn num) attachments))
         (mu4e-view-mime-part-action (cdr (assoc
                                           (completing-read "MIME-part: "
                                                            attachments nil t)
                                           attachments)))))))

;;; Main view and global stuff
  ;; Bookmarks
  (add-to-list 'mu4e-bookmarks `(:name
                                 "Important"
                                 :query ,(concat "maildir:/Orange/Important"
                                                 " OR "
                                                 "maildir:/Unicaen/Important"
                                                 " OR "
                                                 "maildir:/ENS_Lyon/Important")
                                 :key   ?i)
               t)

  ;; Taken from mu4e~stop in mu4e-utils.el
  ;; Do not kill mu process
  (defun lps/mu4e-kill-buffers ()
    "Kill all mu4e buffers"
    (interactive)
    ;; kill all mu4e buffers
    (mapc
     (lambda (buf)
       ;; When using the Gnus-based viewer, the view buffer has the
       ;; kill-buffer-hook function mu4e~view-kill-buffer-hook-fn which kills the
       ;; mm-* buffers created by Gnus' article mode.  Those have been returned by
       ;; `buffer-list' but might already be deleted in case the view buffer has
       ;; been killed first.  So we need a `buffer-live-p' check here.
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (member major-mode
                         '(mu4e-headers-mode mu4e-view-mode mu4e-main-mode))
             (kill-buffer)))))
     (buffer-list))

    ;; Update mail and index when leaving
    (unless (and (buffer-live-p mu4e--update-buffer)
                 (process-live-p (get-buffer-process mu4e--update-buffer)))
      (mu4e-update-mail-and-index t)))

  ;; Better name for mu4e-view buffers
  (ensure-version mu4e-mu "1.10"
    (defun lps/mu4e-view-buffer-name-func (buf)
      (with-current-buffer buf
        (let ((name (mu4e-message-field (mu4e-message-at-point)
                                        :subject)))
          (when name
            (concat "*mu4e-view* " (truncate-string-to-width name 10))))))

    (setq mu4e-view-buffer-name-func 'lps/mu4e-view-buffer-name-func))

;;; Contexts
  ;; Before making a new context:
  ;; - Make sure that the [sent/trash/drafts] folders are correctly named, to avoid duplicates
  ;; - Don't forget to modify .mbsyncrc and .authinfo.gpg to correctly authenticate against
  ;; the IMAP and SMTP servers
  ;; - Make sure that your smpt-user ID, the port (smtp-service), etc, are the right ones; different
  ;; SMTP servers have different expectations, and there is no universal configuration

  (setq mu4e-contexts
        (list
         ;; School account
         (make-mu4e-context
          :name "Unicaen"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Unicaen" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "leo.paviet-salomon@unicaen.fr")
                  (user-full-name     . "Leo Paviet Salomon")
                  (mu4e-drafts-folder . "/Unicaen/Drafts")
                  (mu4e-sent-folder   . "/Unicaen/Sent")
                  (mu4e-refile-folder . "/Unicaen/Archive")
                  (mu4e-trash-folder  . "/Unicaen/Trash")
                  (smtpmail-smtp-user    . "paviets201")
                  (smtpmail-smtp-server  . "smtp.unicaen.fr")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)))

         (make-mu4e-context
          :name "Orange"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Orange" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "leo.paviet.salomon@orange.fr")
                  (user-full-name     . "Leo Paviet Salomon")
                  (mu4e-drafts-folder . "/Orange/DRAFT")
                  (mu4e-sent-folder   . "/Orange/OUTBOX")
                  (mu4e-refile-folder . "/Orange/Archive")
                  (mu4e-trash-folder  . "/Orange/TRASH")
                  (smtpmail-smtp-user    . "leo.paviet.salomon@orange.fr")
                  (smtpmail-smtp-server  . "smtp.orange.fr")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)))

         (make-mu4e-context
          :name "ENS_Lyon"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/ENS_Lyon" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address  . "leo.paviet-salomon@ens-lyon.fr")
                  (user-full-name     . "Leo Paviet Salomon")
                  (mu4e-drafts-folder . "/ENS_Lyon/Brouillons")
                  (mu4e-sent-folder   . "/ENS_Lyon/Elements_envoyes")
                  ;;(mu4e-sent-messages-behavior . 'delete) ;; Not sure yet, better be safe
                  (mu4e-refile-folder . "/ENS_Lyon/Archive")
                  (mu4e-trash-folder  . "/ENS_Lyon/Corbeille")
                  (smtpmail-smtp-user    . "lpaviets")
                  (smtpmail-smtp-server  . "smtp.ens-lyon.fr")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)))))

;;; Queries
  (defun lps/mu4e-read-range (fun initial-prompt)
    (concat (funcall fun initial-prompt)
            ".."
            (funcall fun "... and ...")))

  (defun lps/mu4e-read-date (&optional prompt)
    (let ((time (decode-time (org-read-date nil t))))
      (format "%04d-%02d-%02d"
              (decoded-time-year time)
              (decoded-time-month time)
              (decoded-time-day time))))

  (defun lps/mu4e-read-date-range (&optional prompt)
    (lps/mu4e-read-range 'lps/mu4e-read-date "Mail received or sent between ... "))

  (defun lps/mu4e-read-size-range (&optional prompt)
    (lps/mu4e-read-range 'read-string "Mail with size between ... "))

  (defun lps/mu4e-read-regexp (&optional prompt)
    (concat "/" (read-string prompt) "/"))

  (defun lps/mu4e-read-mime-type (&optional prompt)
    (require 'mailcap)
    (mailcap-parse-mimetypes)
    (completing-read (or prompt "Mime type: ") (mailcap-mime-types)))

  (defvar lps/mu4e-build-query-alist
    '((?q "confirm" "confirm")
      (?\  " anything" "" read-string)
      (?f "from" "from:" read-string)
      (?t "to" "to:" read-string)
      (?d "date" "date:" ((?d "date" "" lps/mu4e-read-date)
                          (?r "range" "" lps/mu4e-read-date-range)))
      (?F "Flag" "flag:" ((?u "unread" "unread")
                          (?d "draft" "draft")
                          (?f "flagged" "flagged")
                          (?n "new" "new")
                          (?p "passed" "passed")
                          (?r "replied" "replied")
                          (?s "seen" "seen")
                          (?t "trashed" "trashed")
                          (?a "attach" "attach")
                          (?e "encrypted" "encrypted")
                          (?S "Signed" "signed")
                          (?P "Personal" "personal")))
      (?s "subject" "subject:" read-string)
      (?m "mime-type" "mime:" lps/mu4e-read-mime-type)
      (?a "attachment" "file:" lps/mu4e-read-regexp)
      (?S "Size" "size:" lps/mu4e-read-size-range)))

  (defun lps/mu4e-parse-query (choices)
    (let* ((choice (read-multiple-choice "Query element: " choices))
           (rest (cddr choice))
           (str (car rest))
           (read-fun-or-continue (cadr rest)))
      (cond
       ;; RETURN key sends 'return', and it is not a *character*, so
       ;; we can't use it due to how read-multiple-choice is implemented ...
       ((eq (car choice) ?q)
        :quit)
       ((functionp read-fun-or-continue)
        (concat str (funcall read-fun-or-continue (concat str " "))))
       ((consp read-fun-or-continue)
        (concat str (lps/mu4e-parse-query read-fun-or-continue)))
       (t str))))

  (defun lps/mu4e-build-query (&optional start-query)
    "Provides a simpler interface to build mu4e search queries.
    A caveat is that it does not insert logical separators (NOT, AND,
    OR ...) between expressions, so the expression has to be modified
    by hand if needed"
    (interactive "P")
    (let ((choices lps/mu4e-build-query-alist)
          (query-list (if start-query
                          (list (completing-read "Search for: "
                                                 mu4e--search-hist
                                                 nil
                                                 nil
                                                 nil
                                                 'mu4e--search-hist))
                        nil))
          (choice nil))
      (while (not (eq (setq choice (lps/mu4e-parse-query choices)) :quit))
        (push choice query-list))
      (let ((query (mapconcat 'identity (reverse query-list) " ")))
        (mu4e-search query "Search for: " t))))

;;; Appearance
  ;; Redefine everything here
  (setq mu4e-headers-draft-mark     '("D" . "⚒")
        mu4e-headers-flagged-mark   '("F" . "✚")
        mu4e-headers-new-mark       '("N" . "✱")
        mu4e-headers-passed-mark    '("P" . "❯")
        mu4e-headers-replied-mark   '("R" . "❮")
        mu4e-headers-seen-mark      '("S" . "✔")
        mu4e-headers-trashed-mark   '("T" . "⏚")
        mu4e-headers-attach-mark    '("a" . "≡")
        mu4e-headers-encrypted-mark '("x" . "⸮")
        mu4e-headers-signed-mark    '("s" . "☡")
        mu4e-headers-unread-mark    '("u" . "⎕")
        mu4e-headers-list-mark      '("s" . "⁞")
        mu4e-headers-personal-mark  '("p" . "⍟")
        mu4e-headers-calendar-mark  '("c" . "Ⓒ"))

  ;; Change this face to make it more visible, at least for kaolin-ocean theme
  (set-face-attribute 'mu4e-replied-face nil :inherit 'font-lock-function-name-face)

  ;; This is ugly otherwise
  (when (facep 'mu4e-thread-fold-face)
    (set-face-attribute 'mu4e-thread-fold-face nil :inherit nil))

  (defun lps/resize-headers-fields ()
    (if (eq major-mode 'mu4e-headers-mode)
        (let ((width (window-body-width)))
          (setq-local mu4e-headers-fields `((:human-date . 20)
                                            (:flags . 6)
                                            (:mailing-list . 10)
                                            (:from-or-to . 22)
                                            (:subject . ,(- width
                                                            (+ 20 6 10 22 15))))))))

  ;; (add-hook 'mu4e-headers-mode-hook #'lps/resize-headers-fields)

  ;; Inspired by mu4e-column-faces-mode
  ;; Taken from mu4e-column-faces
  (defvar lps/mu4e-headers-fields-propertize
    `((:human-date font-lock-string-face)
      (:flags font-lock-type-face)
      (:mailing-list font-lock-builtin-face)
      (:from-or-to font-lock-variable-name-face)
      (:subject lps/mu4e~headers-format-subject)
      (:bcc font-lock-variable-name-face)
      (:cc font-lock-variable-name-face)
      (:changed)
      (:date font-lock-string-face)
      (:from font-lock-variable-name-face)
      (:maildir font-lock-function-name-face)
      (:list font-lock-builtin-face)
      (:message-id font-lock-keyword-face)
      (:path font-lock-function-name-face)
      (:size font-lock-string-face)
      (:tags font-lock-keyword-face)
      (:thread-subject font-lock-doc-face)
      (:to font-lock-variable-name-face))
    "Alist of (FIELD-NAME FACE-OR-FUNCTION).

    If the second element is a function, it will be called with two
    elements: the field content, and the message itself.")

  (defvar lps/mu4e-headers-propertize-context
    '(("Orange" "red4")
      ("Unicaen" "blue3")
      ("ENS_Lyon" "green4"))
    "Alist of (CONTEXT-NAME FACE-OR-FUNCTION)

    If the second element is a function, it will be called with the
    header corresponding to the current message.

    If it is a string, it will be assumed to be a colour; in that
    case, header will be prefixed with \"█ \", coloured in this
    colour.

    Otherwise, it is a symbol naming a face.")

  (defun lps/mu4e~headers-field-handler (f-w msg)
    "Create a description of the field of MSG described by F-W."
    (let* ((field (car f-w))
           (width (cdr f-w))
           (face (cadr (assoc field lps/mu4e-headers-fields-propertize)))
           (val (mu4e~headers-field-value msg field))
           (val (and val (cond
                          ((facep face)
                           (propertize val 'face face))
                          ((functionp face)
                           (funcall face val msg))
                          (t val))))
           (val (and val
                     width
                     (mu4e~headers-truncate-field field val width))))
      val))

  (defun lps/mu4e-headers-prettify-message (msg)
    (let* ((msg-prop-fields (mapconcat
                             (lambda (f-w)
                               (lps/mu4e~headers-field-handler f-w msg))
                             mu4e-headers-fields " "))
           (context (mu4e-context-name (mu4e-context-determine msg)))
           (face (cadr (assoc-string context lps/mu4e-headers-propertize-context)))
           (new-msg (cond
                     ((facep face)
                      (propertize msg-prop-fields 'face face))
                     ((functionp face)
                      (funcall face msg-prop-fields))
                     ((stringp face)
                      (let ((boxes (propertize "█ " 'face `(:foreground ,face))))
                        (concat boxes msg-prop-fields)))
                     (t msg-prop-fields))))
      new-msg))

  (defun lps/mu4e~headers-format-subject (sub msg)
    (with-temp-buffer
      (insert sub)
      (goto-char (point-min))
      (when (re-search-forward "\\(Re:\\|Fwd:\\| \\)*\\[\\([-a-zA-Z0-9., ]+?\\)\\]"
                               nil t 1)
        (add-face-text-property (match-beginning 2)
                                (match-end 2)
                                '(:weight bold :underline t)))
      (buffer-substring (point-min) (point-max))))

  ;; This overrides the previous definition of this function, inlines
  ;; everything called by the original function, and replace an internal
  ;; function call by our own function.

  ;; Because mu4e is somewhat unstable, we need to duplicate some code
  ;; depending on the installed version ...
  (version-case mu4e-mu
    ("1.8"
     (defun-override lps/mu4e~headers-append-handler (msglst)
       "Append one-line descriptions of messages in MSGLIST.
    Do this at the end of the headers-buffer."
       (when (buffer-live-p (mu4e-get-headers-buffer))
         (with-current-buffer (mu4e-get-headers-buffer)
           (save-excursion
             (let ((inhibit-read-only t))
               (seq-do
                (lambda (msg)
                  (when-let
                      ((line (unless (and mu4e-headers-hide-predicate
                                          (funcall mu4e-headers-hide-predicate msg))
                               (mu4e~headers-apply-flags
                                msg
                                (lps/mu4e-headers-prettify-message msg))))
                       (docid (plist-get msg :docid)))
                    (goto-char (point-max))
                    (insert
                     (propertize
                      (concat
                       (mu4e~headers-docid-cookie docid)
                       mu4e--mark-fringe line "\n")
                      'docid docid 'msg msg))))
                msglst)))))))

    ("1.7"
     (defun-override lps/mu4e~headers-header-handler (msg &optional point)
       "Create a one line description of MSG in this buffer, at POINT,
    if provided, or at the end of the buffer otherwise."
       (when (buffer-live-p (mu4e-get-headers-buffer))
         (with-current-buffer (mu4e-get-headers-buffer)
           (save-excursion
             (let ((inhibit-read-only t))
               (when-let ((line
                           (unless (and mu4e-headers-hide-predicate
                                        (funcall mu4e-headers-hide-predicate msg))
                             (mu4e~headers-apply-flags
                              msg
                              (lps/mu4e-headers-prettify-message msg))))
                          (docid (plist-get msg :docid)))
                 (when line
                   (goto-char (if point point (point-max)))
                   (insert
                    (propertize
                     (concat
                      (mu4e~headers-docid-cookie docid)
                      mu4e~mark-fringe line "\n")
                     'docid docid 'msg msg))))))))))))

;; From https://github.com/iqbalansari/dotEmacs/blob/master/config/mail.org
(use-package gnus-dired
  :after mu4e
  :ensure nil
  :hook (dired-mode . turn-on-gnus-dired-mode)
  :config
  (defun-override lps/gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent))

(use-package dired
  :after gnus-dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("E" . gnus-dired-attach)))

(use-package org-mime
  :after mu4e
  :custom
  (org-mime-export-options (list :section-numbers nil
                                 :with-author nil
                                 :with-toc nil))
  :config
  (defun lps/safe-org-mime-confirm-when-no-multipart ()
    (when lps/safe-mail-send
      (org-mime-confirm-when-no-multipart)))

  ;; Make sure that this hook is added AFTER
  ;; lps/sign-or-encrypt-message so that it is executed BEFORE it. We
  ;; want to htmlize, then sign/encrypt, not the other way around !
  (add-hook 'message-send-hook 'lps/safe-org-mime-confirm-when-no-multipart)

  ;; Hacky function to avoid big formatting problems when calling
  ;; org-mime-htmlize after having linked attachments, or
  ;; signing/encrypting the message
  (defun lps/org-mime-htmlize-preserve-secure-and-attach ()
    (interactive)
    (let ((re-secure "<#secure method=[a-z]+ mode=[a-z]+>\n?")
          (re-attachment "<#part type=.* disposition=attachment.*>\n?<#/part>\n?")) ;; make sure that \n needs no escaping/formatting
      (let ((secure (lps/find-delete-forward-all-regexp re-secure (point-min)))
            (attachments (lps/find-delete-forward-all-regexp re-attachment (point-min))))
        (org-mime-htmlize)
        (save-excursion
          (goto-char (point-max))
          (while attachments
            (insert (pop attachments)))
          (message-goto-body)
          (while secure
            (insert (pop secure))))))))

(use-package gnus-icalendar
  :defer t
  :config
  (defun lps/gnus-icalendar-org-setup ()
    (progn
      (lps/gnus-icalendar-insinuate-org-templates)
      (setq gnus-icalendar-org-enabled-p t)))

  (defun lps/gnus-icalendar-insinuate-org-templates ()
    (unless (gnus-icalendar-find-if
             (lambda (x)
               (string= (cadr x) gnus-icalendar-org-template-name))
             org-capture-templates)
      (setq org-capture-templates
            (cons `(,gnus-icalendar-org-template-key
                    ,gnus-icalendar-org-template-name
                    entry
                    (function lps/org-capture-agenda-any-location)
                    "\n%i\n"
                    :empty-lines 1)
                  org-capture-templates))))

  (lps/gnus-icalendar-org-setup)

  ;; Override this function. At the date of <2024-04-10 mer.> there is a TODO in
  ;; the gnus-icalendar.el file to make the template customizable. Until this is
  ;; done, we use the following hack.
  (defun-override lps/gnus-icalendar:org-event-save (event reply-status)
    (with-temp-buffer
      (let ((short (y-or-n-p "Capture a short version of the invitation ?")))
        (org-capture-string
         (if short
             (lps/gnus-icalendar-event->org-entry-short event reply-status)
           (gnus-icalendar-event->org-entry entry reply-status))
         gnus-icalendar-org-template-key))))

  (cl-defun lps/gnus-icalendar-event->org-entry-short (event reply-status)
    "Return string with new `org-mode' entry describing EVENT."
    (with-temp-buffer
      (org-mode)
      (with-slots (summary)
          event
        (insert (format "* %s\n\n"
                        (gnus-icalendar--format-summary-line summary)))

        (save-restriction
          (narrow-to-region (point) (point))
          (insert (gnus-icalendar-event:org-timestamp event)
                  "\n")
          (indent-region (point-min) (point-max) 2)
          (fill-region (point-min) (point-max)))

        (buffer-string)))))

(use-package message-attachment-reminder
  :after mu4e
  :custom
  (message-attachment-reminder-regexp
   (regexp-opt '(;; English
                 "attached"
                 "attachment"
                 "enclosed"
                 ;; French
                 "ci-joint"
                 "pièce-jointe"
                 "t'envoie"
                 "vous envoie"))))

(use-package elpher
  :defer t)

(use-package olivetti
  :defer t
  :custom
  (olivetti-body-width 90))

(use-package nov
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :custom
  (nov-variable-pitch t)
  (nov-text-width t)
  :config
  (defun lps/nov-mode-comfort-settings ()
    (setq visual-fill-column-width 90)
    (setq visual-fill-column-center-text t)
    (visual-line-mode 1)
    (visual-fill-column-mode 1))
  (add-hook 'nov-mode-hook #'lps/nov-mode-comfort-settings))

(use-package ispell
  :defer t
  :init
  (defvar lps/ispell-personal-dictionaries-dir
    (expand-file-name "ispell-dicts/"
                      user-emacs-directory)
    "Directory where ispell personal dictionaries are stored")
  (setq ispell-personal-dictionary
        (expand-file-name "fr" lps/ispell-personal-dictionaries-dir))
  :bind
  ("<f8>" . ispell)
  ("S-<f8>" . lps/ispell-change-dictionary)
  :hook (message-send . lps/ispell-message-ask)
  :custom
  (ispell-quietly t)
  (ispell-program-name (executable-find "aspell"))
  :config
  (defun lps/ispell-message-ask ()
    (when (y-or-n-p "Check spelling ?")
      (ispell-message)))

;;; Redefinition of ispell-message to work with mu4e
  (defun ispell-message ()
    "Check the spelling of a mail message or news post.
Don't check spelling of message headers except the Subject field.
Don't check included messages.

To abort spell checking of a message region and send the message anyway,
use the `x' command.  (Any subsequent regions will be checked.)
The `X' command aborts sending the message so that you can edit the buffer.

To spell-check whenever a message is sent, include the appropriate lines
in your init file:
   (add-hook \\='message-send-hook #\\='ispell-message)  ;; GNUS 5
   (add-hook \\='news-inews-hook #\\='ispell-message)    ;; GNUS 4
   (add-hook \\='mail-send-hook  #\\='ispell-message)
   (add-hook \\='mh-before-send-letter-hook #\\='ispell-message)

You can bind this to the key C-c i in GNUS or mail by adding to
`news-reply-mode-hook' or `mail-mode-hook' the following lambda expression:
   (lambda () (local-set-key \"\\C-ci\" \\='ispell-message))"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let* (boundary
             mimep
             (ispell-skip-region-alist-save ispell-skip-region-alist)
             ;; Nil when message came from outside (eg calling Emacs as editor)
             ;; Non-nil marker of end of headers.
             (internal-messagep
              (re-search-forward
               (concat "^" (regexp-quote mail-header-separator) "$") nil t))
             (end-of-headers            ; Start of body.
              (copy-marker
               (or internal-messagep
                   (re-search-forward "^$" nil t)
                   (point-min))))
             (limit (copy-marker  ; End of region we will spell check.
                     (cond
                      ((not ispell-message-text-end) (point-max))
                      ((char-or-string-p ispell-message-text-end)
                       (if (re-search-forward ispell-message-text-end nil t)
                           (match-beginning 0)
                         (point-max)))
                      (t (min (point-max)
                              (funcall ispell-message-text-end))))))
             (default-prefix ; Vanilla cite prefix used for cite-regexp)
              (if (ispell-non-empty-string mail-yank-prefix)
                  "   \\|\t"))
             (cite-regexp               ;Prefix of quoted text
              (cond
               ((functionp 'sc-cite-regexp) ; supercite >= 3.0
                (with-no-warnings
                  (concat "\\(" (sc-cite-regexp) "\\)" "\\|"
                          (ispell-non-empty-string
                           sc-reference-tag-string))))
               ((member major-mode '(message-mode
                                     mu4e-compose-mode)) ; GNUS >= 5
                (concat "In article <" "\\|"
                        "[^,;&+=\n]+ <[^,;&+=]+> writes:" "\\|"
                        (with-no-warnings message-cite-prefix-regexp)
                        "\\|"
                        default-prefix))
               ((equal major-mode 'mh-letter-mode) ; mh mail message
                (concat "[^,;&+=\n]+ writes:" "\\|"
                        (with-no-warnings
                          (ispell-non-empty-string mh-ins-buf-prefix))))
               ((not internal-messagep) ; Assume nn sent us this message.
                (concat "In [a-zA-Z.]+ you write:" "\\|"
                        "In <[^,;&+=]+> [^,;&+=]+ writes:" "\\|"
                        " *> *"))
               ((boundp 'vm-included-text-prefix) ; VM mail message
                (concat "[^,;&+=\n]+ writes:" "\\|"
                        (ispell-non-empty-string vm-included-text-prefix)))
               (t default-prefix)))
             (ispell-skip-region-alist
              (cons (list (ispell--make-filename-or-URL-re))
                    (cons (list (concat "^\\(" cite-regexp "\\)")
                                (function forward-line))
                          ispell-skip-region-alist)))
             (old-case-fold-search case-fold-search)
             (dictionary-alist ispell-message-dictionary-alist)
             (ispell-checking-message t))

        ;; Select dictionary for message
        (or (local-variable-p 'ispell-local-dictionary (current-buffer))
            (while dictionary-alist
              (goto-char (point-min))
              (if (re-search-forward (car (car dictionary-alist))
                                     end-of-headers t)
                  (setq ispell-local-dictionary (cdr (car dictionary-alist))
                        dictionary-alist nil)
                (setq dictionary-alist (cdr dictionary-alist)))))

        (unwind-protect
            (progn
              ;; Spell check any original Subject:
              (goto-char (point-min))
              (setq case-fold-search t
                    mimep (re-search-forward "MIME-Version:" end-of-headers t))
              (goto-char (point-min))
              (if (re-search-forward "^Subject: *" end-of-headers t)
                  (progn
                    (goto-char (match-end 0))
                    (if (and (not (looking-at ".*Re\\>"))
                             (not (looking-at "\\[")))
                        (progn
                          (setq case-fold-search old-case-fold-search)
                          (ispell-region (point)
                                         (progn ;Tab-initiated continuation lns.
                                           (end-of-line)
                                           (while (looking-at "\n[ \t]")
                                             (end-of-line 2))
                                           (point)))))))
              (if mimep
                  (progn
                    (goto-char (point-min))
                    (setq boundary (ispell-mime-multipartp end-of-headers))))
              ;; Adjust message limit to MIME message if necessary.
              (and boundary
                   (re-search-forward (concat boundary "--") nil t)
                   (re-search-backward boundary nil t)
                   (< (point) (marker-position limit))
                   (set-marker limit (point)))
              (goto-char (point-min))
              ;; Select type or skip checking if this is a non-multipart message
              ;; Point moved to end of buffer if region is encoded.
              (when (and mimep (not boundary))
                (goto-char (point-min))
                (re-search-forward "Content-[^ \t]*:" end-of-headers t)
                (forward-line -1) ; following fn starts one line above
                (ispell-mime-skip-part nil)
                ;; if message-text-end region, limit may be less than point.
                (if (> (point) limit)
                    (set-marker limit (point))))
              (goto-char (max end-of-headers (point)))
              (forward-line 1)
              (setq case-fold-search old-case-fold-search)
              ;; Define MIME regions to skip.
              (if boundary
                  (setq ispell-checking-message
                        (list (list boundary 'ispell-mime-skip-part boundary))))
              (ispell-region (point) limit))
          (set-marker end-of-headers nil)
          (set-marker limit nil)
          (setq ispell-skip-region-alist ispell-skip-region-alist-save
                ispell-skip-html nil
                case-fold-search old-case-fold-search)))))

  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))

  (defun lps/ispell-change-personal-dictionary (code &optional kill-ispell)
    (let ((perso-dict (expand-file-name code lps/ispell-personal-dictionaries-dir)))
      (when (file-exists-p perso-dict)
        (setq ispell-personal-dictionary perso-dict
              ispell-current-personal-dictionary (print perso-dict))))
    (when (and ispell-process kill-ispell)
      (ispell-kill-ispell)))

  (defun lps/ispell-change-dictionary (dict)
    (interactive
     (list
      (completing-read
       "Use new dictionary (RET for current, SPC to complete): "
       (and (fboundp 'ispell-valid-dictionary-list)
            (mapcar #'list (ispell-valid-dictionary-list)))
       nil t)))
    (ispell-change-dictionary dict)
    (lps/ispell-change-personal-dictionary dict)))

(use-package flyspell
  :bind
  ("C-S-<f8>" . lps/flyspell-toggle)
  :config
  ;; From https://www.emacswiki.org/emacs/FlySpell
  (defun lps/flyspell-on-for-buffer-type ()
    "Enable Flyspell appropriately for the major mode of the current
buffer. Uses `flyspell-prog-mode' for modes derived from `prog-mode', so
only strings and comments get checked. All other buffers get `flyspell-mode'
to check all text. If flyspell is already enabled, does nothing."
    (interactive)
    (unless flyspell-mode               ; if not already on
      (if (derived-mode-p 'prog-mode)
          (progn
            (message "Flyspell on (code)")
            (flyspell-prog-mode))
        (progn
          (message "Flyspell on (text)")
          (flyspell-mode 1)))))

  (defun lps/flyspell-toggle ()
    "Turn Flyspell on if it is off, or off if it is on.
When turning on, it uses `lps/flyspell-on-for-buffer-type' so code-vs-text
is handled appropriately."
    (interactive)
    (if flyspell-mode
        (progn
          (message "Flyspell off")
          (flyspell-mode -1))
      (lps/flyspell-on-for-buffer-type))))

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-$" . flyspell-correct-wrapper)))

(use-package guess-language
  :disabled t
  :hook
  (text-mode . guess-language-mode)
  :custom
  (guess-language-languages '(en fr))
  :config
  (defun lps/guess-language-switch-personal-dict (lang beg end)
    (lps/ispell-change-personal-dictionary (symbol-name lang)))

  (add-hook 'guess-language-after-detection-functions
            'lps/guess-language-switch-personal-dict))

(use-package artist
  :ensure nil
  :defer t
  :bind
  (:map artist-mode-map
        ([remap artist-next-line] . lps/artist-next-line))
  :config
  (defun lps/artist-next-line (&optional n)
    "Move cursor down N lines (default is 1), updating current shape.
If N is negative, move cursor up.
If N is greater than the number of remaining lines in the buffer,
insert as many blank lines as necessary."
    (interactive "p")
    (let* ((col (artist-current-column))
           (max-line (save-excursion
                       (goto-char (point-max))
                       (artist-current-line)))
           (current-line (artist-current-line))
           (diff (- max-line current-line)))
      (when (>= n diff)
        (save-excursion
          (goto-char (point-max))
          (open-line (- n diff))))
      (forward-line n)
      (move-to-column col t))

    (when artist-key-is-drawing
      (artist-key-do-continously-common)))

  (setq artist-arrows [ ?> ?⌟ ?v ?⌞ ?< ?⌜ ?^ ?⌝ ])

  (when (fboundp 'which-key-mode)
    (add-to-list 'which-key-replacement-alist
                 '((nil . "\\<artist-select-op") . (nil . "🎨"))
                 nil
                 'equal)))

(use-package calendar
  :ensure nil
  :custom
  (calendar-view-holidays-initially-flag t)
  (calendar-mark-holidays-flag t)
  (calendar-latitude 47)
  (calendar-longitude 3) ; random place, roughly middle of France
  (cal-tex-preamble-extra (mapconcat 'identity
                                     '("\\usepackage[utf8]{inputenc}"
                                       "\\usepackage[T1]{fontenc}")
                                     "\n"))
  :hook
  (calendar-mode . lps/windmove-mode-local-off)
  :bind
  (:map calendar-mode-map          ; Mimic bindings from org-read-date
        ("M-S-<left>"  . calendar-backward-month)
        ("M-S-<right>" . calendar-forward-month)
        ("M-S-<up>"    . calendar-backward-year)
        ("M-S-<down>"  . calendar-forward-year)
        ("S-<up>"      . calendar-backward-week)
        ("S-<down>"    . calendar-forward-week)
        ("S-<left>"    . calendar-backward-day)
        ("S-<right>"   . calendar-forward-day))
  :config
  (calendar-set-date-style 'european)

  (defvar holiday-french-holidays
    `((holiday-fixed 1 1 "Jour de l'an")
      (holiday-fixed 1 6 "Épiphanie")
      (holiday-fixed 2 2 "Chandeleur")
      (holiday-fixed 2 14 "Saint Valentin")
      (holiday-fixed 5 1 "Fête du travail")
      (holiday-fixed 5 8 "Fête de la Victoire")
      (holiday-fixed 6 21 "Fête de la musique")
      (holiday-fixed 7 14 "Fête nationale")
      (holiday-fixed 8 15 "Assomption (Religieux)")
      (holiday-fixed 11 11 "Armistice de 1918")
      (holiday-fixed 11 1 "Toussaint")
      (holiday-fixed 12 25 "Noël")
      ;; Not fixed
      (holiday-easter-etc 0 "Pâques")
      (holiday-easter-etc 1 "Lundi de Pâques")
      (holiday-easter-etc 39 "Ascension")
      (holiday-easter-etc 49 "Pentecôte")
      (holiday-easter-etc -47 "Mardi gras")
      (holiday-float 5 0 4 "Fête des mères")
      (holiday-float 6 0 3 "Fête des pères")) ;; June's third Sunday
    "French holidays")

  (setq holiday-local-holidays holiday-french-holidays)

  (setq calendar-holidays
        (append holiday-general-holidays
                holiday-local-holidays
                holiday-other-holidays
                ;; holiday-christian-holidays
                ;; holiday-hebrew-holidays
                ;; holiday-islamic-holidays
                ;; holiday-bahai-holidays
                ;; holiday-oriental-holidays
                ;; holiday-solar-holidays
                )))

(use-package calfw
  :defer t
  :hook
  (cfw:calendar-mode . lps/enable-auto-hscroll-scroll)
  :bind
  (:map cfw:calendar-mode-map
        ("RET" . cfw:show-details-command))
  :custom
  ;; Taken from https://github.com/kiwanami/emacs-calfw

  ;; If it does not show, comment this, and it will use the defaults which are
  ;; normal ascii chars instead.
  (cfw:fchar-junction ?┼)
  (cfw:fchar-vertical-line ?│)
  (cfw:fchar-horizontal-line ?─)
  (cfw:fchar-left-junction ?├)
  (cfw:fchar-right-junction ?┤)
  (cfw:fchar-top-junction ?┬)
  (cfw:fchar-top-left-corner ?┌)
  (cfw:fchar-top-right-corner ?┐)
  (cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap))

(use-package calfw-org
  :defer t
  :init
  (setq cfw:org-overwrite-default-keybinding t) ; dumb here but it is not a custom var ...
  :bind ("C-c A" . cfw:open-org-calendar)
  :config
  ;; There is a problem with the original function: periods are displayed at the
  ;; wrong dates + multiple times. AFAICT, the computations of start-date and
  ;; end-date are bonkers, so we fix those.
  (defun-override lps/cfw:org-get-timerange (text)
    "Return a range object (begin end text).
If TEXT does not have a range, return nil."
    (let* ((dotime (cfw:org-tp text 'dotime)))
      (and (stringp dotime)
           (string-match org-ts-regexp dotime)
           (let ((date-string  (match-string 1 dotime))
                 (extra (cfw:org-tp text 'extra)))
             (when (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
               (let* ((range-beg (save-match-data (org-read-date nil t date-string)))
                      (total-days (string-to-number
                                   (match-string 2 extra)))
                      (start-date range-beg)
                      (end-date (time-add
                                 start-date
                                 (seconds-to-time (* 3600 24 (- total-days 1))))))
                 (list (calendar-gregorian-from-absolute (time-to-days start-date))
                       (calendar-gregorian-from-absolute (time-to-days end-date))
                       text)))))))

  ;;;; Still a problem: Emoji size makes alignment weird, as characters are too wide.
  ;;;; Possible fix/solution: either use font-rescaling, or find a monospace font
  ;;;; with Emoji symbols.
  ;; (defun lps/cfw:org-summary-format (item)
  ;;   (let* ((original (cfw:org-summary-format item))
  ;;          (props (text-properties-at 0 original))
  ;;          (category (cfw:org-tp item 'org-category))
  ;;          (icon (caadr (assoc-string category org-agenda-category-icon-alist))))
  ;;     (if icon
  ;;         (concat (apply 'propertize icon props)
  ;;                 original)
  ;;       original)))

  ;; Buggy
  ;; (setq cfw:org-schedule-summary-transformer 'lps/cfw:org-summary-format)
  )

(use-package elfeed
  :defer t
  :bind
  ("C-c f" . lps/elfeed-dashboard)
  (:map elfeed-search-mode-map
        ("w" . elfeed-search-browse-url)
        ("C-S-s" . lps/elfeed-search-filter-interactive)
        ("*" . lps/elfeed-toggle-star)
        ("U" . elfeed-update)
        ("b" . lps/elfeed-search-bookmark))
  (:map elfeed-show-mode-map
        ("D" . lps/elfeed-arxiv-get-pdf-add-bibtex-entry))
  :init
  (defvar lps/elfeed-search-arxiv-authors-max-width 30)
  (defvar lps/elfeed-default-days-range 7
    "Range of days to filter by default in elfeed search queries")
  :custom
  (elfeed-db-directory (locate-user-emacs-file ".elfeed"))
  (elfeed-search-title-max-width 110)
  (elfeed-search-filter "@1-week-ago +unread -arxiv -youtube")
  :config
  (defun lps/elfeed-toggle-star ()
    (interactive)
    (elfeed-search-toggle-all 'star))

  ;; Online science articles archives
  (defface elfeed-search-arxiv-authors
    '((t (:inherit message-header-to :weight normal)))
    "Faced used in *elfeed-search* buffer to show authors of Arxiv papers"
    :group 'elfeed)

  (defun lps/elfeed-search-format-arxiv-authors (entry)
    (let* ((authors (elfeed-meta entry :authors))
           (authors-no-dup (cl-remove-duplicates authors
                                                 :test #'string-equal
                                                 :key (lambda (author)
                                                        (plist-get author :name))))
           (formatted-authors
            (mapconcat (lambda (author)
                         (when-let ((auth (plist-get author :name))
                                    (splitted (s-split-up-to " " auth 1)))
                           (concat
                            (substring (car splitted) 0 1)
                            "."
                            (cadr splitted))))
                       authors-no-dup
                       ", "))
           (width (length formatted-authors))
           (max-width (- lps/elfeed-search-arxiv-authors-max-width 6)))
      (propertize
       (if (<  max-width width)
           (concat "[" (substring formatted-authors 0 max-width) "...] ")
         (elfeed-format-column
          (concat "[" formatted-authors "] ")
          lps/elfeed-search-arxiv-authors-max-width
          :left))
       'face 'elfeed-search-arxiv-authors
       'kbd-help formatted-authors)))

  (defun lps/elfeed-search-print-entry--arxiv (entry &optional format-title)
    (let* ((date
            (elfeed-search-format-date (elfeed-entry-date entry)))
           (raw-title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
           (title (if format-title (funcall format-title raw-title) raw-title))
           (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
           (feed (elfeed-entry-feed entry))
           (feed-title
            (when feed
              (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
           (authors (lps/elfeed-search-format-arxiv-authors entry))
           (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
           (tags-str (mapconcat
                      (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                      tags ","))
           (title-width (- (window-width)
                           lps/elfeed-search-arxiv-authors-max-width
                           10
                           elfeed-search-trailing-width))
           (title-column (elfeed-format-column
                          title (elfeed-clamp
                                 elfeed-search-title-min-width
                                 title-width
                                 (- elfeed-search-title-max-width
                                    lps/elfeed-search-arxiv-authors-max-width))
                          :left)))
      (insert (propertize date 'face 'elfeed-search-date-face) " ")
      (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
      (insert authors)
      (when feed-title
        (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
      (when tags
        (insert "(" tags-str ")"))))

  (defun lps/elfeed-search-print-entry--hal (entry)
    (let* ((hal-prefix-regexp "^ *\\[[a-z]+-[0-9]+\\] *")
           (format-title (lambda (title)
                           (replace-regexp-in-string hal-prefix-regexp "" title))))
      (lps/elfeed-search-print-entry--arxiv entry format-title)))

  (defun lps/elfeed-search-show-entry-function (entry)
    (let ((tags (elfeed-entry-tags entry)))
      (cond
       ((member 'arxiv tags) (lps/elfeed-search-print-entry--arxiv entry))
       ((member 'hal tags) (lps/elfeed-search-print-entry--hal entry))
       (t (elfeed-search-print-entry--default entry)))))

  (setq elfeed-search-print-entry-function
        'lps/elfeed-search-show-entry-function)

  (defun lps/elfeed-search-filter-prompt-time-range ()
    (let* ((default-time
            (time-subtract (current-time)
                           (days-to-time lps/elfeed-default-days-range)))
           (from (org-read-date nil nil nil nil default-time)))
      (concat "@" from)))

  (defun lps/elfeed--org-tags ()
    (let* ((elfeed-org-buffers (cl-loop for file in rmh-elfeed-org-files
                                        for buffer = (get-file-buffer file)
                                        when buffer
                                        collect buffer))
           (tags (cl-loop for buffer in elfeed-org-buffers
                          append (with-current-buffer buffer
                                   (org-get-buffer-tags)))))
      (cl-remove-duplicates (mapcar #'car tags))))

  (defun lps/elfeed-search-filter-add-tags ()
    (let ((with-tags (completing-read-multiple "Add tags: " (lps/elfeed--org-tags))))
      (if with-tags
          (concat "+" (mapconcat #'identity with-tags " +"))
        "")))

  (defun lps/elfeed-search-filter-remove-tags ()
    (let ((without-tags (completing-read-multiple "Remove tags: " (lps/elfeed--org-tags))))
      (if without-tags
          (concat "-" (mapconcat #'identity without-tags " -"))
        "")))

  (defun lps/elfeed-search-filter-interactive ()
    (interactive)
    (let ((time (lps/elfeed-search-filter-prompt-time-range))
          (with-tags (lps/elfeed-search-filter-add-tags))
          (without-tags (lps/elfeed-search-filter-remove-tags)))
      (let ((filter (concat time " " with-tags " " without-tags ))
            (elfeed-search-filter-active :non-interactive))
        (elfeed-search--prompt filter)
        (with-current-buffer (elfeed-search-buffer)
          (setf elfeed-search-filter
                (or filter (default-value 'elfeed-search-filter)))
          (elfeed-search-update :force)))))

  (defun lps/elfeed-arxiv-get-pdf-add-bibtex-entry ()
    (interactive)
    (require 'org-ref-bibtex)
    (require 'org-ref-arxiv)
    (let* ((entry elfeed-show-entry)
           (id (cdr (elfeed-entry-id entry)))
           (num (progn
                  (string-match "^https?://arxiv.org/abs/\\([0-9.]+\\)" id)
                  (match-string 1 id)))
           (bibfile (completing-read "Bibfile: " (org-ref-possible-bibfiles)))
           (pdfdir (cond
                    ((stringp bibtex-completion-library-path)
                     bibtex-completion-library-path)
                    ((= 1 (length bibtex-completion-library-path))
                     (car bibtex-completion-library-path))
                    (t
                     (completing-read "PDF dir: "
                                      bibtex-completion-library-path)))))
      (arxiv-get-pdf-add-bibtex-entry num bibfile pdfdir)))

  ;; Dashboard
  (defmacro lps/elfeed-wrap-before-elfeed (fun)
    `(lambda ()
       (interactive)
       (call-interactively ,fun)
       (elfeed)))

  (defvar lps/elfeed-dashboard-buffer "*elfeed-dashboard*")
  (defcustom lps/elfeed-bookmarks
    '(( :name  "All unread entries"
        :query "+unread"
        :key ?u)
      ( :name "Today's entries"
        :query "@1-day-ago"
        :key ?t)
      ( :name "Last 7 days"
        :query "@1-week-ago"
        :hide-unread t
        :key ?w)
      ( :name "Science articles"
        :query "+unread +articles"
        :key ?a)
      ( :name "YT Videos"
        :query "+unread +youtube"
        :key ?y)
      ( :name "Politique"
        :query "+unread +politique"
        :key ?p)
      ( :name "Favourite"
        :query "+star"
        :key ?f))
    "See `mu4e-bookmarks' for some documentation")
  :config
  ;; mu4e-like dashboard !
  (require 'elfeed-org)
  (require 'mu4e)

  (defvar lps/elfeed-dashboard-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map "U" (lambda ()
                            (interactive)
                            (elfeed-update)
                            (lps/elfeed-dashboard--redraw nil nil)))
      (define-key map "r" 'lps/elfeed-org-reread)
      (define-key map "s" (lps/elfeed-wrap-before-elfeed
                           #'lps/elfeed-search-filter-interactive))
      (define-key map "S" (lps/elfeed-wrap-before-elfeed
                           #'elfeed-search-set-filter))
      (define-key map "b" 'lps/elfeed-search-bookmark)
      (define-key map "$" (lambda ()
                            (interactive)
                            (pop-to-buffer (elfeed-log-buffer))))
      map)
    "Keymap for the *elfeed-dashboard* buffer.")

  ;; Temporary remappings while mu4e is normalizing names
  (defvar lps/mu4e-main-action-str
    (version-case mu4e-mu
      ("1.11" (lambda (title cmd)
                (string-match "\\[\\([a-zA-Z]+?\\)\\]" title)
                (let ((binding (match-string 1 title)))
                  (mu4e--main-action (replace-match  "[@]" nil nil title)
                                     cmd
                                     binding))))
      ("1.8" 'mu4e--main-action-str)
      (t 'mu4e~main-action-str)))

  (defvar lps/mu4e-key-val
    (version-case mu4e-mu
      ("1.8" 'mu4e--key-val)
      (t 'mu4e~key-val)))

  (defun lps/elfeed-ask-bookmark (prompt)
    "Ask the user for a bookmark (using PROMPT) as defined in
  `lps/elfeed-bookmarks', then return the corresponding query."
    (unless lps/elfeed-bookmarks (mu4e-error "No bookmarks defined"))
    (let* ((prompt (mu4e-format "%s" prompt))
           (bmarks
            (mapconcat
             (lambda (bm)
               (concat
                "[" (propertize (make-string 1 (plist-get bm :key))
                                'face 'mu4e-highlight-face)
                "]"
                (plist-get bm :name)))
             lps/elfeed-bookmarks ", "))
           (kar (read-char (concat prompt bmarks))))
      (let* ((chosen-bm
              (or (cl-find-if
                   (lambda (bm)
                     (= kar (plist-get bm :key)))
                   lps/elfeed-bookmarks)
                  (error "Unknown shortcut '%c'" kar)))
             (expr (plist-get chosen-bm :query))
             (expr (if (not (functionp expr)) expr
                     (funcall expr)))
             (query (eval expr)))
        (if (stringp query)
            query
          (error "Expression must evaluate to query string ('%S')" expr)))))

  (defun lps/elfeed-search-bookmark (&optional expr)
    "Search using some bookmarked query EXPR."
    (interactive)
    (let ((expr
           (or expr (lps/elfeed-ask-bookmark "Select bookmark: "))))
      (elfeed-search-set-filter expr)
      (elfeed)))

  (defun lps/elfeed-dashboard-query-count (query)
    (let* ((count 0)
           (filter
            (elfeed-search-parse-filter query))
           (func (byte-compile
                  (elfeed-search-compile-filter filter))))
      (with-elfeed-db-visit (entry feed)
        (when (funcall func entry feed count)
          (setf count (1+ count))))
      count))

  (defun lps/elfeed-bookmarks-dashboard ()
    (cl-loop with bmks = lps/elfeed-bookmarks
             with longest = (cl-loop for b in lps/elfeed-bookmarks
                                     maximize (string-width (plist-get b :name)))
             ;; with queries = (mu4e-last-query-results)
             for bm in bmks
             for key = (string (plist-get bm :key))
             for name = (plist-get bm :name)
             for query = (plist-get bm :query)
             for count-u = (lps/elfeed-dashboard-query-count (concat "+unread "
                                                                     query))
             for count = (lps/elfeed-dashboard-query-count query)
             when (not (plist-get bm :hide))
             concat (concat
                     ;; menu entry
                     (funcall lps/mu4e-main-action-str
                              (concat "\t* [b" key "] " name)
                              (concat "b" key))
                     ;; append all/unread numbers, if available.
                     (if (and count count-u)
                         (format
                          "%s (%s/%s)"
                          (make-string (- longest (string-width name)) ? )
                          (propertize (number-to-string count-u)
                                      'face 'mu4e-header-key-face)
                          count)
                       "")
                     "\n")))

  (defun lps/elfeed-dashboard ()
    (interactive)
    (with-current-buffer (get-buffer-create lps/elfeed-dashboard-buffer)
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        (insert
         "* "
         (propertize "elfeed" 'face 'mu4e-header-key-face)
         (propertize " - Elfeed reader for Emacs version " 'face 'mu4e-title-face)
         (propertize  elfeed-version 'face 'mu4e-header-key-face)
         "\n"
         (propertize "  Last updated at " 'face 'mu4e-title-face)
         (let* ((db-time (seconds-to-time (elfeed-db-last-update)))
                (update (format-time-string "%Y-%m-%d %H:%M" db-time)))
           (propertize update 'face 'mu4e-header-key-face))
         "\n\n"
         (propertize "  Basics\n\n" 'face 'mu4e-title-face)
         ;; (funcall lps/mu4e-main-action-str
         ;;  "\t* [j]ump to some maildir\n" 'mu4e-jump-to-maildir)
         (funcall lps/mu4e-main-action-str
                  "\t* enter a [s]earch query interactively\n"
                  'lps/elfeed-search-filter-interactive)
         "\n"
         (funcall lps/mu4e-main-action-str
                  "\t* modify the [S]earch query\n" 'elfeed-search-set-filter)
         "\n"
         (propertize "  Bookmarks\n\n" 'face 'mu4e-title-face)
         (lps/elfeed-bookmarks-dashboard)
         "\n"
         (propertize "  Misc\n\n" 'face 'mu4e-title-face)

         (funcall lps/mu4e-main-action-str "\t* [U]pdate feeds & database\n"
                  (lambda ()
                    (interactive)
                    (elfeed-update)
                    (lps/elfeed-dashboard--redraw nil nil)))
         (funcall lps/mu4e-main-action-str "\t* [r]ead elfeed-org files" 'lps/elfeed-org-reread)
         "\n"
         ;; (funcall lps/mu4e-main-action-str "\t* [H]elp\n" 'mu4e-display-manual)
         (funcall lps/mu4e-main-action-str "\t* [q]uit\n" 'bury-buffer)

         "\n"
         (propertize "  Info\n\n" 'face 'mu4e-title-face)
         (funcall lps/mu4e-key-val "database-path" elfeed-db-directory)
         (funcall lps/mu4e-key-val "in store"
                  (format "%d" (elfeed-db-size))
                  "entries"))
        (goto-char pos))
      (lps/elfeed-dashboard-mode))
    (pop-to-buffer lps/elfeed-dashboard-buffer))

  (defun lps/elfeed-dashboard--redraw (_ignore-auto _noconfirm)
    "The revert buffer function for `lsp/elfeed-dashboard-mode'."
    (lps/elfeed-dashboard))

  (define-derived-mode lps/elfeed-dashboard-mode special-mode "elfeed:dashboard"
    "Major mode for the elfeed dashboard.
  \\{lps/elfeed-dashboard-mode-map}."
    (setq truncate-lines t)
    (setq-local revert-buffer-function #'lps/elfeed-dashboard--redraw)
    (read-only-mode 1)))

(use-package elfeed-org
  :defer t
  :custom
  (rmh-elfeed-org-files (list
                         (expand-file-name "elfeed.org"
                                           org-directory)))
  :init
  (defun lps/elfeed-org-lazy-load ()
    (elfeed-org)
    (advice-remove 'elfeed 'lps/elfeed-org-lazy-load))

  (advice-add 'elfeed :before 'lps/elfeed-org-lazy-load)
  :config
  (defun lps/elfeed-org-reread ()
    (interactive)
    (rmh-elfeed-org-process rmh-elfeed-org-files
                            rmh-elfeed-org-tree-id)))

(use-package elfeed-tube
  :after elfeed
  :custom
  (elfeed-tube-auto-save-p nil)
  (elfeed-tube-auto-fetch-p t)
  (elfeed-tube-captions-languages '("fr" "french"
                                    "en" "english"
                                    "english (auto generated)"
                                    "french (auto generated)"))
  :bind
  (:map elfeed-show-mode-map
        ("F" . elfeed-tube-fetch)
        ([remap save-buffer] . elfeed-tube-save)
        :map elfeed-search-mode-map
        ("F" . elfeed-tube-fetch)
        ([remap save-buffer] . elfeed-tube-save))
  :config
  (elfeed-tube-setup))

(use-package elfeed-tube-mpv
  :after elfeed-tube
  :hook
  (elfeed-tube-mpv . elfeed-tube-mpv-follow-mode)
  :bind
  (:map elfeed-show-mode-map
        ("C-c C-f" . elfeed-tube-mpv-follow-mode)
        ("C-c C-w" . elfeed-tube-mpv-where)
        ("C-c C-y" . elfeed-tube-mpv)))

(use-package mpv
  :defer t
  :init
  ;; Used to be 0.5 in the initial mpv.el package
  ;; Not a customizable option: need to redefine some functions ...
  (defvar lps/mpv-on-start-timeout 5)
  :config
  ;; Redefine it to use a custom timeout duration
  (defun mpv-start (&rest args)
    "Start an mpv process with the specified ARGS.

If there already is an mpv process controlled by this Emacs
instance, it will be killed. Options specified in
`mpv-default-options' will be prepended to ARGS."
    (mpv-kill)
    (let ((socket (make-temp-name
                   (expand-file-name "mpv-" temporary-file-directory))))
      (setq mpv--process
            (apply #'start-process "mpv-player" nil mpv-executable
                   "--no-terminal"
                   (concat "--input-unix-socket=" socket)
                   (append mpv-default-options args)))
      (set-process-query-on-exit-flag mpv--process nil)
      (set-process-sentinel
       mpv--process
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (mpv-kill)
           (when (file-exists-p socket)
             (with-demoted-errors "Socket does not exist: %S"
                 (delete-file socket)))
           (run-hooks 'mpv-on-exit-hook))))
      (with-timeout
          (lps/mpv-on-start-timeout (mpv-kill)
                                    (error "Failed to connect to mpv"))
        (while (not (file-exists-p socket))
          (sleep-for 0.05)))
      (setq mpv--queue (tq-create
                        (make-network-process :name "mpv-socket"
                                              :family 'local
                                              :service socket)))
      (set-process-filter
       (tq-process mpv--queue)
       (lambda (_proc string)
         (mpv--tq-filter mpv--queue string)))
      (run-hook-with-args 'mpv-on-start-hook args)
      t)))

(use-package subed
  :custom
  (subed-auto-play-media nil))
