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

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents t)) ; Async

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

;; (let* ((extra-package-dir (expand-file-name "extra-packages" user-emacs-directory))
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

(require 'use-package)
;; Comment this line if you don't want to automatically install
;; all the packages that you are missing
;; (setq use-package-always-ensure t)
;; Uncomment the folllowing line to have a detailed startup log
;; (setq use-package-verbose t)

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

(defmacro ensure-version (version &rest body)
  "Execute BODY when the current Emacs version is larger than VERSION"
  (declare (indent 1))
  `(when (version<= ,(lps/versionify version) emacs-version)
     ,@body))

(defmacro version-case (&rest cases)
  "CASES is a list of (VERSION BODY) where version is a version
number or a string. The macro expands to the code associated the
latest possible version.
As a special case, the version T is considered to be smaller than
all the other versions"
  (let ((versions (sort cases (lambda (v1 v2)
                                (version<= (lps/versionify (car v1))
                                           (lps/versionify (car v2))))))
        (gver (make-symbol "version"))
        version-conds)
    (dolist (ver versions)
      (let ((v-num (car ver))
            (v-body (cdr ver)))
        (push (cons `(version<= ,(lps/versionify v-num) ,gver) v-body)
              version-conds)))
    `(let ((,gver emacs-version))
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
        ("P l" . package-list-packages)))

(system-case
 (gnu/linux
  (use-package password-cache
    :ensure nil
    :custom
    (password-cache t)
    (password-cache-expiry 300))

  (use-package pinentry
    :custom
    (epg-pinentry-mode 'loopback)
    :config
    (pinentry-start))

  (use-package auth-source
    :ensure nil
    :custom
    (auth-sources (remove "~/.authinfo" auth-sources))
    (auth-source-cache-expiry 86400) ;; All day

    :config
    (defvar lps/--auth-cache-expiry-setup-p nil)

    (defun lps/auth-source-define-cache-expiry ()
      (interactive)
      (unless lps/--auth-cache-expiry-setup-p
        (setq lps/--auth-cache-expiry-setup-p t)
        (when (y-or-n-p (concat "Change default auth-cache-expiry value "
                                "(default "
                                (number-to-string auth-source-cache-expiry)
                                ") ?"))
          (setq auth-source-cache-expiry (read-number "New cache expiry value in seconds: " auth-source-cache-expiry)))))

    (defun lps/force-forget-all-passwords ()
      (interactive)
      (auth-source-forget-all-cached)
      (shell-command "gpgconf --kill gpg-agent")
      ;; (shell-command "gpgconf -- reload gpg-agent")
      (setq lps/--auth-cache-expiry-setup-p nil)))))

(use-package restart-emacs
  :commands
  (restart-emacs restart-emacs-start-new-emacs)
  :bind
  (:map lps/system-tools-map
        ("r" . restart-emacs)))

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
  (desktop-path (list (expand-file-name "desktop-saves/" user-emacs-directory)))
  (desktop-restore-eager 10)
  (desktop-lazy-verbose nil))

(use-package server
  :custom
  (server-client-instructions nil))

(setq custom-file (concat user-emacs-directory "custom-file.el"))
(load custom-file 'noerror)

(ensure-version 28
 (use-package emacs
   :custom
   (native-comp-async-report-warnings-errors 'silent)))

(use-package emacs
  :custom
  (locale-coding-system 'utf-8)
  (display-raw-bytes-as-hex t)
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
  (define-key key-translation-map (kbd "<M-dead-circumflex>") (kbd "M-^")))

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
    (let ((font-list (font-family-list)))
      (when (member lps/variable-font font-list)
        (set-face-font 'variable-pitch lps/variable-font))

      ;; Default fixed-pitch
      (when (member lps/fixed-font font-list)
        (set-face-font 'fixed-pitch lps/fixed-font))

      ;; Default (not used in the same place as fixed-pitch)
      (when (member lps/default-font font-list)
        (set-face-font 'default lps/default-font))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (lps/set-default-fonts))))
    (lps/set-default-fonts)))

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

;; Disable the message on top of the Scratch buffer
(setq initial-scratch-message nil)

;; Maximize the Emacs frame at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(ensure-version 29
 (add-to-list 'default-frame-alist '(alpha-background . 95))
 (set-frame-parameter nil 'alpha-background 95))

(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Line/column numbering modes

(use-package emacs
  :init
  (column-number-mode t)

  (defun lps/activate-truncate-lines ()
    (toggle-truncate-lines 1))

  (defvar lps/truncate-lines-modes-hook '(dired-mode-hook
                                          outline-mode-hook
                                          tabulated-list-mode-hook)
    "Modes in which `truncate-lines' will be set to `t' automatically")

  (dolist (hook lps/truncate-lines-modes-hook)
    (add-hook hook 'lps/activate-truncate-lines))
  :custom
  (hscroll-margin 10)
  (hscroll-step 10)
  (auto-hscroll-mode 'current-line)
  (display-line-numbers-width 3)
  (display-line-numbers-grow-only t)
  :hook
  ((prog-mode LaTeX-mode) . display-line-numbers-mode)
  ((text-mode org-mode LaTeX-mode comint-mode) . visual-line-mode))

;; Themes
(use-package solarized-theme)

(use-package kaolin-themes
  :custom
  (kaolin-themes-comments-style 'alt)
  (kaolin-themes-distinct-parentheses t)
  (kaolin-themes-italic-comments t)
  (kaolin-themes-hl-line-colored t))

(use-package modus-themes)
(use-package doom-themes)

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
  (defun lps/resize-and-color-region (beg end)
    "Resize/recolour selected region;defaulting to blue at size 300,for titles.
Note gray80 at size 10 is useful for side remarks."
    (interactive "r")
    (let ((contents (buffer-substring beg end))
          (color (read-color "Colour: "))
          (size (read-number "Size: ")))
      (when contents
        (delete-region beg end)
        (insert (propertize contents
                            'font-lock-face
                            `(:foreground ,color :height ,size)))))))

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
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-project-detection 'project)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-mu4e t)
  (mode-line-compact 'long)
  :config
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
  :config
  (when (and battery-status-function
              (let ((status (battery-format "%B" (funcall battery-status-function))))
                (not (or (string-match-p "N/A" status)
                         (string-match-p "unknown" status)))))
      (display-battery-mode 1)))

(use-package time
  :ensure nil
  :custom
  (display-time-24hr-format t)
  (display-time-format "[%H:%M]")
  :init
  (display-time-mode 1))

;; Generic UI modes

(use-package beacon
  :custom
  (beacon-blink-when-point-moves-vertically 30)
  (beacon-size 20)
  :init (beacon-mode))
(use-package rainbow-mode
  :defer t)
(use-package fill-column-indicator
  :defer t)
(use-package visual-fill-column
  :defer t)
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-line
  :hook ((tabulated-list-mode
          ibuffer-mode)
         . hl-line-mode))

(use-package emacs
  :ensure nil
  :hook (before-save . delete-trailing-whitespace)
  :init
  ;; Tab behaviour and whitespaces
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
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

(use-package hungry-delete
  :defer t
  :init
  ;; (global-hungry-delete-mode 1)
  (setq hungry-delete-join-reluctantly t))

(use-package hydra
  :defer t
  :bind-keymap ("C-c h" . lps/all-hydras-map))

;; Easier hydra definition
(use-package pretty-hydra
  :after hydra)

(use-package emacs
  :ensure nil
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
  :config
  (minibuffer-depth-indicate-mode 1))

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
        (backward-kill-word arg))))

  (defun lps/disable-minibuffer-completion-help (fun &rest args)
    (cl-letf (((symbol-function #'minibuffer-completion-help)
               #'ignore))
      (apply fun args))))

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
    (or (member (buffer-local-value 'major-mode (get-buffer buffer))
                lps/help-modes)
        (member (if (stringp buffer)
                    buffer
                  (buffer-name buffer))
                lps/help-buffers)))

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
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-saved-filter-groups
   '(("default"
      ("Dired" (mode . dired-mode))
      ("Emacs" (or
                (name . "^\\*scratch\\*$")
                (name . "^\\*Messages\\*$")))
      ("Help" (predicate lps/buffer-help-p (current-buffer)))
      ("Special" (and
                  (not (process))
                  (or
                   (starred-name)
                   (mode . special-mode))))
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
  :commands (winner-undo winner-redo)
  :init
  (setq winner-boring-buffers '("*Completions*"
                                "*Compile-Log*"
                                "*Fuzzy Completions*"
                                "*Apropos*"
                                "*Help*"
                                "*Buffer List*"
                                "*Ibuffer*"))
  (winner-mode 1))

(use-package windmove
  ;; Make windmove work in Org mode:
  :hook
  (org-shiftup-final . windmove-up)
  (org-shiftleft-final . windmove-left)
  (org-shiftdown-final . windmove-down)
  (org-shiftright-final . windmove-right)

  :init
  (windmove-default-keybindings 'shift)
  (windmove-swap-states-default-keybindings '(ctrl shift))

  :config
  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
        (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
        (shrink-window arg)
      (enlarge-window arg))))

(use-package emacs
  :ensure nil
  :bind (:map lps/all-hydras-map
              ("w" . hydra-window/body))

  :init
  (defhydra hydra-window (:color red
                                 :hint nil)
   "
    ^Focus^           ^Resize^       ^Split^                 ^Delete^          ^Other
    ^^^^^^^^^-------------------------------------------------------------------------------
    _b_move left      _B_left        _V_split-vert-move      _o_del-other      _c_new-frame
    _n_move down      _N_down        _H_split-horiz-move     _da_ace-del       _u_winner-undo
    _p_move up        _P_up          _v_split-vert           _dw_del-window    _r_winner-redo
    _f_move right     _F_right       _h_split-horiz          _df_del-frame
    _q_uit
    "
   ;; Move the focus around
   ("b" windmove-left)
   ("n" windmove-down)
   ("p" windmove-up)
   ("f" windmove-right)

   ;; Changes the size of the current window
   ("B" hydra-move-splitter-left)
   ("N" hydra-move-splitter-down)
   ("P" hydra-move-splitter-up)
   ("F" hydra-move-splitter-right)

   ;; Split and move (or not)
   ("V" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right)))
   ("H" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down)))
   ("v" split-window-right)
   ("h" split-window-below)

   ;; winner-mode must be enabled
   ("u" winner-undo)
   ("r" winner-redo) ;;Fixme, not working?

   ;; Delete windows
   ("o" delete-other-windows :exit t)
   ("da" ace-delete-window)
   ("dw" delete-window)
   ("db" kill-this-buffer)
   ("df" delete-frame :exit t)

   ;; Other stuff
   ("a" ace-window :exit t)
   ("c" make-frame :exit t)
   ("s" ace-swap-window)
   ("q" nil)))

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

(global-set-key (kbd "C-c t") 'lps/toggle-window-dedicated)

(use-package ffap
  :ensure nil
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
  (dolist (excl (list (expand-file-name (concat user-emacs-directory "eshell/"))
                      (expand-file-name (concat user-emacs-directory "\\.elfeed/"))
                      "\\.synctex\\.gz" "\\.out$" "\\.toc" "\\.log"
                      (expand-file-name recentf-save-file)
                      "/usr/local/share/emacs/"
                      "bookmarks$"
                      (expand-file-name "~/Mail/")))
    (add-to-list 'recentf-exclude excl)))

(use-package emacs
  :ensure nil
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
  :init
  (defvar lps/backup-directory (concat user-emacs-directory ".backups/"))
  (unless (file-exists-p lps/backup-directory)
    (make-directory lps/backup-directory))

  (setq backup-directory-alist `(("." . ,lps/backup-directory))))

(use-package outline
  :ensure nil
  :defer t
  :hook (prog-mode . outline-minor-mode)
  :custom
  (outline-minor-mode-prefix "\C-o")
  :config
  ;; Problems with TAB -> completely override cycle keymap
  (setq outline-mode-cycle-map (make-sparse-keymap)))

(use-package emacs
  :ensure nil
  :custom
  (scroll-preserve-screen-position t)
  (mouse-wheel-tilt-scroll t))

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :custom
  (describe-char-unidata-list t)
  :bind (:map help-map
              (";" . helpful-at-point))
  :init
  (require 'helpful) ;; somewhat hacky, would like to autoload ...
  (defalias 'describe-function 'helpful-callable)
  (defalias 'describe-variable 'helpful-variable)
  (defalias 'describe-symbol 'helpful-symbol)
  (defalias 'describe-key 'helpful-key))

(use-package emacs
  :ensure nil
  :custom
  (apropos-documentation-sort-by-scores t)
  :bind
  (:map help-map
        ("u" . describe-face)
        ("U" . describe-font)
        ("C-k" . describe-keymap)
        ("M" . man)))

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
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.5))

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
  :init
  (savehist-mode))

(use-package keycast
  :defer t
  :custom
  (keycast-mode-line-remove-tail-elements nil)
  (keycast-mode-line-insert-after "%e")
  (keycast-mode-line-format "%10s%k%c%r%10s"))

;; Type "y" instead of "yes RET" for confirmation
(version-case
 (28 (setq use-short-answers t))
 (t (defalias 'yes-or-no-p 'y-or-n-p)))

(use-package consult
  :defer t
  :bind
  ("C-S-s" . lps/consult-line-strict-match)
  ("C-c i" . lps/consult-imenu-or-org-heading)
  ("C-c r r" . consult-register-load)
  ("C-c r s" . consult-register-store)
  ("C-x b" . consult-buffer)
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
  ("C-h b" . embark-bindings)
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

(ensure-version 28.0
  (use-package repeat
    :bind
    (:map lps/quick-edit-map
          ("z" . repeat))
    :init
    (repeat-mode 1)))

(use-package emacs
  :ensure nil
  :bind
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  ([remap count-words-region] . count-words)
  ([remap count-words-region] . count-words))

(use-package multiple-cursors
  :defer t
  :init
  (defvar lps/multiple-cursors-map (make-sparse-keymap))
  (defvar lps/multiple-cursors-repeat-map (make-sparse-keymap))
  :bind
  ("<M-S-mouse-1>" . mc/add-cursor-on-click)
  (:map lps/all-hydras-map
        ("M" . hydra-multiple-cursors/body))
  (:map lps/multiple-cursors-map
        ("<down>" . mc/mark-next-like-this)
        ("<up>" . mc/mark-previous-like-this)
        ("<right>" . mc/unmark-next-like-this)
        ("<left>" . mc/unmark-previous-like-this)
        ("a" . mc/mark-all-like-this)
        ("A" . mc/mark-all-dwim))
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
      ("S" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
      ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
      ("r" mc/mark-all-in-region "Mark all in region" :exit t)
      ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
      ("d" mc/mark-all-dwim "Mark all dwim"))
     "Mark same word (next)"
     (("n" mc/mark-next-like-this "Mark next like this")
      ("N" mc/skip-to-next-like-this "Skip to next like this"))
     "Mark same word (previous)"
     (("p" mc/mark-previous-like-this "Mark previous like this")
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

(use-package iedit
  :defer t
  :bind
  ("C-;" . iedit-mode))

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
  (defun lps/company-set-completion-styles (fun &rest args)
    (let ((completion-styles '(basic partial-completion emacs22)))
      (apply fun args)))

  (advice-add 'company--perform :around #'lps/company-set-completion-styles)

  ;; Use our personal default backends
  (defun lps/company-default-backends-prog ()
    (setq-local company-backends '((company-capf company-files company-dabbrev)
                                   (company-dabbrev-code company-gtags company-etags company-keywords company-clang)
                                   company-oddmuse)))

  (defun lps/company-default-backends-text ()
    (setq-local company-backends '((company-capf company-files company-dabbrev company-ispell)
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
  (setq company-box-backends-colors '((company-yasnippet :all "dark turquoise"
                                                         :selected (:background "slate blue"
                                                                                :foreground "white")))))

(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :diminish
  :custom
  (company-quickhelp-delay 0.2))

(use-package emacs
  :ensure nil
  :bind (:map lps/all-hydras-map
              ("m" . hydra-move/body))
  :init
  (defhydra hydra-move ()
    "Movement" ; m as in movement
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
  :config
  ;; Change this face to distinguish between current match and other ones
  (set-face-foreground 'isearch "#98f5ff"))

(use-package replace
  :ensure nil
  :bind
  (:map query-replace-map
        ("RET" . act)
        ("<return>" . act))
  (:map lps/quick-edit-map
        ("%" . replace-string)
        ("C-%" . replace-regexp)))

(use-package avy
  :defer t
  :bind
  ("M-é" . avy-goto-char-2)
  :custom
  ;; Using an AZERTY keyboard home row
  (avy-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l ?m))
  (avy-all-windows nil)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.5)
  (avy-translate-char-function '(lambda (c) (if (= c 32) ?q c))))

(use-package emacs
  :ensure nil
  :bind
  (:map lps/all-hydras-map
        ("r" . hydra-rectangle/body))
  :init
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
           (rectangle-mark-mode 1)))        ;; C-x SPC
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
  :custom
  (kill-read-only-ok t)
  (kill-ring-max 100)
  (kill-do-not-save-duplicates t)
  :config
  (defun lps/copy-line-at-point (arg)
    "Copy lines in the kill ring, starting from the line at point.
If ARG is not specified or equalt to 1, do not copy the indentation.
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
    "Select the current line. If the region is already active, extends the current selection by line."
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
        (indent-region point (point))))))

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
  (drag-stuff-global-mode 1)
  (add-to-list 'drag-stuff-except-modes 'org-mode)
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
          (next-line (* arg (count-lines-region (region-beginning) (region-end)))))

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
    (save-excursion
      (previous-logical-line arg)
      (setq final (point)))
    (kill-whole-line (- arg))
    (goto-char final)))

(use-package undo-tree
  :diminish
  :custom
  (undo-tree-visualizer-timestamps t)
  (undo-tree-enable-undo-in-region t)
  (undo-tree-visualizer-diff t)
  (undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode))

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
        ("C-t" . lps/make-filename-from-sentence))

  :config
  (defun lps/--fill-width-repeat-string (width str)
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
              (lps/--fill-width-repeat-string width str)
              (forward-line 1)
              (end-of-line)
              (insert "\n")
              (indent-to col)
              (lps/--fill-width-repeat-string width str))
          (progn
            (end-of-line)
            (insert "\n")
            (indent-to col)
            (lps/--fill-width-repeat-string width str))))))

  (defvar lps/do-not-capitalize-list '("the" "a" "of" "in" "on"
                                       "no" "or" "and" "if" "for"
                                       "le" "la" "les" "et" "ou"
                                       "si" "un" "une" "de" "des"
                                       "du" "d" "l" "ni"))

  (defun lps/make-filename-from-sentence ()
    "Create a title from the current line or region and add it to the
 kill-ring"
    (interactive)
    (let* ((bounds (if (region-active-p)
                       (car (region-bounds))
                     (cons (line-beginning-position)
                           (line-end-position))))
           (start (car bounds))
           (end (set-marker (make-marker) (cdr bounds))))
      (goto-char start)
      (capitalize-word 1)
      (while (< (point) (marker-position end))
        (let ((num-spaces (skip-chars-forward "[:punct:][:space:]")))
          (if (> num-spaces 0)
              (progn
                (delete-backward-char num-spaces)
                (insert "_"))
            (forward-char 1)))
        (let ((word-at-pt (word-at-point)))
          (if (or (not word-at-pt)
                  (member (downcase word-at-pt)
                          lps/do-not-capitalize-list))
              (downcase-word 1)
            (capitalize-word 1))))
      (kill-ring-save start (point)))))

(use-package projectile
  :diminish
  :disabled t ;; try Project.el instead
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (let ((path-project "~/Documents/Projects"))
    (when (file-directory-p path-project)
      (setq projectile-project-search-path (list path-project))))
  (projectile-mode))

(use-package magit
  :defer t
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer
  :bind
  ("C-x g" . magit-status)
  (:map magit-section-mode-map
        ("M-^" . magit-section-up))
  :config
  (dolist (action '(stage-all-changes unstage-all-changes))
    (add-to-list 'magit-no-confirm action)))

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
  :bind
  (:map lps/all-hydras-map
        ("s" . hydra-smerge/body))
  :bind-keymap
  ("C-c m" . smerge-basic-map)
  :init
  (defun lps/smerge-maybe-start ()
    (when (and buffer-file-name (vc-backend buffer-file-name))
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<< " nil t)
          (message "Smerge-mode automatically enabled: there seem to be conflicts !")
          (smerge-mode 1)))))

  :config
  (pretty-hydra-define hydra-smerge (:title "Smerge Hydra"
                                            :post (smerge-auto-leave)
                                            :color pink
                                            :hint nil
                                            :quit-key "q")
    ("Move"
     (("n" smerge-next "Next")
      ("p" smerge-prev "Prev"))
     "Keep"
     (("b" smerge-keep-base "Base")
      ("u" smerge-keep-upper "Upper")
      ("m" smerge-keep-upper "Upper")
      ("l" smerge-keep-lower "Lower")
      ("o" smerge-keep-lower "Lower")
      ("a" smerge-keep-all "All")
      ("RET" smerge-keep-current "Current")
      ("\C-m" smerge-keep-current "Current"))
     "Diff"
     (("<" smerge-diff-base-upper "Upper/Base")
      ("=" smerge-diff-upper-lower "Upper/Lower")
      (">" smerge-diff-base-lower "Lower/Base")
      ("R" smerge-refine "Refine")
      ("E" smerge-ediff "Ediff"))
     "Other"
     (("C" smerge-combine-with-next "Combine")
      ("r" smerge-resolve "Resolve")
      ("k" smerge-kill-current "Kill current")
      ("ZZ" (lambda ()
              (interactive)
              (save-buffer)
              (bury-buffer))
       "Save and bury buffer" :color blue)
      ("q" nil "Cancel" :color blue)))))

;; Always highlight matching parenthesis
(use-package paren
  :ensure nil
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
        ("C-M-," . paredit-convolute-sexp)
        ([remap newline] . paredit-newline)
        ("<C-backspace>" . paredit-delete-region)
        ("M-S-<left>" . lps/transpose-sexp-backward)
        ("M-S-<right>" . lps/transpose-sexp-forward))
  :config
  ;; Version 29 or 30 broke something ?!
  ;; Remove paredit broken RET key
  (when (version< "29" emacs-version)
    (define-key paredit-mode-map (kbd "RET") nil t))

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
  (company-dabbrev-downcase nil))

(use-package company-math
  :after company)

(use-package company-shell
  :disabled t
  :after eshell
  :hook (eshell-mode . lps/company-shell-modes)
  :config
  (defun lps/company-shell-modes ()
    ;; Not satisfying: duplicates from company-capf and company-shell, so we disable the 2nd one but we lose some documentation ...
    (setq-local company-backends '((company-shell-env company-fish-shell company-capf company-files company-dabbrev company-shell)))
    (push 'elisp-completion-at-point completion-at-point-functions)))

(use-package emacs
  :ensure nil
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
  (defvar lps/--default-lsp-mode 0)

  (defun lps/lsp-by-default-in-session ()
    (if (> lps/--default-lsp-mode 0)
        (lps/start-language-server)
      (if (and (= lps/--default-lsp-mode 0)
               (y-or-n-p "Automatically use lsp-mode in the current session ?"))
          (progn
            (setq lps/--default-lsp-mode 1)
            (lps/start-language-server)))
      (setq lps/--default-lsp-mode -1)))

  (defun lps/toggle-lsp-by-default-in-session ()
    (interactive)
    (setq lps/--default-lsp-mode (not lps/--default-lsp-mode)))

  ;; Fix documentation: don't want to start a server to view some
  ;; C code in helpful buffers !
  (defun lps/--no-lsp-here (fun &rest args)
    (let ((lps/--default-lsp-mode -1))
      (apply fun args)))

  (advice-add 'helpful-update :around 'lps/--no-lsp-here))

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  (lsp-diagnostics-provider :flycheck)  ; :none if none wanted
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
  :ensure nil
  :bind-keymap ("C-c l" . eglot-mode-map)
  :bind (:map eglot-mode-map
              ("r" . eglot-rename)
              ("g g" . xref-find-definitions)
              ("g r" . xref-find-references)
              ("h" . eldoc))
  :custom
  (eglot-ignored-server-capabilities '(:documentHighlightProvider)))

;; Flycheck
(use-package flycheck
  :defer t
  :custom
  ;; (setq flycheck-relevant-error-other-file-show nil) ;might be useful
  (flycheck-indication-mode 'left-margin)
  (flycheck-display-errors-delay 0.3))

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

(use-package devdocs
  :defer t
  :bind
  (:map prog-mode-map
        ("<f6>" . devdocs-lookup))
  :init
  (defvar lps/devdocs-alist
    '((python-mode-hook . "python~3.8")
      (haskell-mode-hook . "haskell~8")
      (c-mode-hook . "c")
      (c++-mode-hook . "cpp")
      (tuareg-mode-hook . "ocaml")
      (LaTeX-mode-hook . "latex")))

  (dolist (pair lps/devdocs-alist)
    (let ((hook (car pair))
          (doc (cdr pair)))
      (add-hook hook `(lambda () (setq-local devdocs-current-docs (list ,doc)))))))

(use-package emacs
  :ensure nil
  :custom
  (comint-scroll-to-bottom-on-input t)
  (comint-prompt-read-only t))

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
               (cons 'python-mode 'python-shell-send-buffer)))

(use-package lsp-pyright
  :after python
  :defer t)

(use-package python-mls
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
  (:map sly-editing-mode-map
        ("M-*" . lps/earmuffify))
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
        (concat "file://" (expand-file-name "~/Documents/Other/HyperSpec/")))

  (define-key sly-prefix-map (kbd "C-v") sly-selector-map)

  (defun lps/sly-company-setup ()
    (setq-local company-prescient-sort-length-enable nil)
    (setq-local company-backends '(company-capf)))

  (defun lps/sly-start-repl ()
    (unless (sly-connected-p)
      (sly)))

  (add-hook 'sly-editing-mode-hook 'lps/sly-start-repl)
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

  (add-to-list 'display-buffer-alist
               '("*sly-db" . ((display-buffer-reuse-mode-window
                               display-buffer-below-selected)
                              . ((inhibit-same-window . nil)
                                 (mode . sly-db-mode)))))

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
          (insert ?*))))))

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
    (flycheck-mode)
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
  :hook
  ((sgml-mode css-mode web-mode) . emmet-mode)
  :custom
  (emmet-move-cursor-between-quotes t))

(use-package gdb-mi
  :ensure nil
  :defer t
  :hook (gdb-mode . gdb-many-windows))

(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-mode))

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

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list
   '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇")
   ;; '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")
   ;; '("◉" "○" "●" "○" "●" "○" "●")
   ))

(use-package org
  :commands org-capture
  :hook (org-mode . lps/org-mode-setup)
  :bind
  ("C-c o" . org-capture)
  ("C-c a" . org-agenda)
  (:map org-mode-map
        ("<C-S-return>" . org-insert-subheading)
        ("<C-S-left>" . nil)
        ("<C-S-right>" . nil)
        ("<C-S-up>" . nil)
        ("<C-S-down>" . nil)
        ("C-," . nil)
        ("C-a" . org-beginning-of-line)
        ("C-e" . org-end-of-line))
  (:map org-src-mode-map
        ("C-c C-c" . org-edit-src-exit))

  :custom
  ;; Coding in blocks
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-use-speed-commands t)
  (org-directory "~/Documents/OrgFiles/")
  (org-special-ctrl-a/e t) ;; Not enough with visual-line-mode, need to bind C-a/C-e too
  (org-return-follows-link t)
  (org-catch-invisible-edits 'show)
  :config
  (defun lps/windmove-mode-local-off ()
    ;; Hack to disable windmove locally
    (setq-local windmove-mode nil))

  (defun lps/windmove-mode-local-off-around (fun &rest args)
    (unwind-protect
        (progn
          (add-hook 'minibuffer-setup-hook 'lps/windmove-mode-local-off)
          (apply fun args))
      (remove-hook 'minibuffer-setup-hook 'lps/windmove-mode-local-off)))

  (advice-add 'org-read-date :around 'lps/windmove-mode-local-off-around)

  (defun lps/org-mode-setup ()
    (lps/org-font-setup)
    (org-indent-mode 1)
    ;; (variable-pitch-mode 1)
    (visual-line-mode 1)
    (lps/windmove-mode-local-off))

  (setq org-imenu-depth 4)

  (setq org-ellipsis " ▾")

(defun lps/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; For non-headers: org-default

  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face) :inherit 'fixed-pitch))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :extend t)
  (set-face-attribute 'org-block-begin-line nil :slant 'italic :foreground "dark gray" :background "#1d1d2b" :inherit 'fixed-pitch :height 1.0)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-link nil :inherit '(link fixed-pitch)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (latex . t)
   (lisp . t)))

;; (setq org-confirm-babel-evaluate nil) ; Take care if executing someone
                                         ; else code

(if (version<= "9.2" org-version) ; ) parsing bug
    ;; This is needed as of Org 9.2
    (progn
      (require 'org-tempo)

      (let ((bound-key-templates
             (mapcar #'car org-structure-template-alist)))
        (dolist (key-template '(("sh" . "src shell")
                                ("el" . "src emacs-lisp")
                                ("py" . "src python")
                                ("latex" . "src latex")
                                ("cl" . "src lisp")))

          (unless
              (member (car key-template) bound-key-templates)
            (push key-template org-structure-template-alist))))))

(setq org-agenda-files (list (concat org-directory "agenda/")))
(setq org-log-into-drawer t)
(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-show-inherited-tags nil)

(setq org-tag-alist
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

(with-eval-after-load "org-agenda"
  (dolist (tag-and-icon `(("Lectures" ,(all-the-icons-faicon "book"))
                          ("Conference" ,(all-the-icons-faicon "users"))
                          ("Talk" ,(all-the-icons-faicon "volume-up"))
                          ("Exam" ,(all-the-icons-octicon "mortar-board"))
                          ("Seminar" ,(all-the-icons-faicon "pencil"))
                          ("Workshop" ,(all-the-icons-material "group_work"))))
    (cl-pushnew (list (car tag-and-icon)
                      (cdr tag-and-icon)
                      nil nil
                      :ascent 'center)
                org-agenda-category-icon-alist
                :test 'equal)))

;; From https://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture
(defun lps/org-ask-location ()
  (let* ((org-refile-targets '((nil :maxlevel . 2)))
         (hd (condition-case nil
                 (car (org-refile-get-location nil nil t))
               (error (car org-refile-history)))))
    (goto-char (point-min))
    (outline-next-heading)
    (if (re-search-forward
         (format org-complex-heading-regexp-format (regexp-quote hd))
         nil t)
        (goto-char (point-at-bol))
      (goto-char (point-max))
      (or (bolp) (insert "\n"))
      (insert "* " hd "\n")))
  (end-of-line))

(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry
         (file+olp ,(concat org-directory "agenda/Tasks.org") "Inbox")
         "* TODO %?\n  %U\n  %a\n  %i"
         :empty-lines 1)

        ("m" "Meeting" entry
         (file+olp+datetree ,(concat org-directory "agenda/Meetings.org"))
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :empty-lines 1)

        ("w" "Workflows")
        ("we" "Checking Email" entry
         (file+olp+datetree ,(concat org-directory "agenda/Tasks.org"))
         "* Checking Email :email:\n\n%?"
         :empty-lines 1)

        ("a" "Agenda (others)" entry
         (file ,(concat org-directory "agenda/Others.org"))
         "* %(call-interactively #'org-time-stamp) %? :agenda:\n"
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

(setq org-capture-bookmark nil))

(use-package org-roam
  :after org
  :custom
  (org-roam-directory (file-truename
                       (expand-file-name "RoamNotes/" org-directory)))
  (org-roam-node-display-template (concat "${title:*} "
                                          (propertize "${tags}"
                                                      'face
                                                      'org-tag)))
  :bind (("C-c n t" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode))

(use-package consult-org-roam
  :after consult org-roam
  :bind
  ("C-c n F" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n s" . consult-org-roam-search)
  :custom
  (consult-org-roam-grep-func #'consult-grep)
  :config
  (consult-org-roam-mode))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(system-case
 (gnu/linux
  (use-package pdf-tools
    :magic ("%PDF" . pdf-view-mode)
    :init
    ;; For some reason it doesn't work when put in the :custom section ?!
    (setq pdf-tools-enabled-modes '(pdf-history-minor-mode
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
                                    ;; pdf-occur-global-minor-mode ;; bugged autoload
                                    pdf-view-auto-slice-minor-mode ; add to defaults
                                    ;; pdf-virtual-global-minor-mode
                                    ))
    :bind (:map pdf-view-mode-map
                ("C-s" . isearch-forward)
                ("C-c ?" . lps/pdf-maybe-goto-index)
                ("<C-down>" . pdf-view-scroll-up-or-next-page)
                ("<C-up>" . pdf-view-scroll-down-or-previous-page)
                ("<C-left>" . image-scroll-right)
                ("<C-right>" . image-scroll-left))
    :custom
    (pdf-links-read-link-convert-commands '("-font" "FreeMono"
                                            "-pointsize" "%P"
                                            "-undercolor" "%f"
                                            "-fill" "%b"
                                            "-draw" "text %X,%Y '%c'"))
    (pdf-links-convert-pointsize-scale 0.015) ;; Slightly bigger than default
    (pdf-view-display-size 'fit-page)
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
        (isearch-backward))))))

(use-package pdf-view-restore
  :disabled t ; buggy ...
  :custom
  (pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore"))
  (use-file-base-name-flag nil))

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
        ("<f6>" . devdocs-lookup)
        ("<backtab>" . indent-for-tab-command)
        ("C-c M-%" . LaTeX-replace-in-math)
        ("C-c C-M-%" . LaTeX-replace-regexp-in-math)
        ("C-c C-d" . lps/TeX-remove-macro))
  :hook
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . lps/latex-fontification)
  (LaTeX-mode . lps/latex-add-environments)
  (LaTeX-mode . lps/latex-company-setup)
  ;; (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . cdlatex-mode)
  (LaTeX-mode . auto-insert)

  :custom
  ;; Automatically insert closing brackets
  (LaTeX-electric-left-right-brace t)
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
  ;; But do insert closing $ when inserting the first one
  (TeX-electric-math '("$" . "$"))
  ;; Also change the key to access LaTeX-math-mode
  (LaTeX-math-abbrev-prefix "°")
  ;; Don't ask for confirmation when cleaning
  (TeX-clean-confirm nil)
  ;; AucTeX doesn't search subdirectories for input/include ...
  (TeX-arg-input-file-search 'ask)

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

  ;; Auto-insert
  (with-eval-after-load 'autoinsert
    (add-to-list 'auto-insert-alist
                 '(latex-mode
                   nil
                   (LaTeX-environment-menu "document")
                   '(if (y-or-n-p "Insert default packages and commands ?")
                        (save-excursion
                          (forward-line -2)
                          (insert "\n\\usepackage[T1]{fontenc}\n"
                                  "\\usepackage[utf8]{inputenc}\n\n"
                                  "\\usepackage{tikz}\n"
                                  "\\usepackage{amsmath, amssymb, amsthm}\n"
                                  "\\usepackage{thm-restate}\n"
                                  "\\usepackage{hyperref}\n"
                                  ;; "\\usepackage{autoref}\n"
                                  "\\usepackage{cleveref}\n"
                                  "\\usepackage{url}\n\n"
                                  "\\newcommand\\NN{\\mathbb N}\n"
                                  "\\newcommand\\ZZ{\\mathbb Z}\n"
                                  "\\newcommand\\RR{\\mathbb R}\n\n"
                                  "\\newtheorem{conjecture}{Conjecture}\n"
                                  "\\newtheorem{proposition}{Proposition}\n"
                                  "\\newtheorem{definition}{Definition}\n"
                                  "\\newtheorem{corollary}{Corollary}\n"
                                  "\\newtheorem{lemma}{Lemma}\n"
                                  "\\newtheorem{theorem}{Theorem}\n"
                                  "\\newtheorem*{example}{Example}\n"
                                  "\\newtheorem*{notation}{Notation}\n"
                                  "\\newtheorem*{remark}{Remark}\n\n"
                                  "\\crefname{lemma}{Lemma}{Lemmas}\n"
                                  "\\crefname{theorem}{Theorem}{Theorems}\n\n")))
                   '(indent-region (point-min) (point-max)))))

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

  ;; SyncTeX forward and inverse search
  (setq TeX-source-correlate-mode t
        ;; Produce a PDF by default
        TeX-PDF-mode t)

  (unless (assoc "PDF tools" TeX-view-program-list-builtin)
    (push '("PDF tools" TeX-pdf-tools-sync-view) TeX-view-program-list))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Redefine TeX-documentation-texdoc to open the doc in Emacs
  (defun TeX-documentation-texdoc (&optional arg)
    "Run texdoc to read documentation.

Prompt for selection of the package of which to show the
documentation.

If called with a prefix argument ARG, after selecting the
package, prompt for selection of the manual of that package to
show."
    (interactive "P")
    (let ((pkg (thing-at-point 'symbol))
          buffer list doc)
      ;; Strip off properties.  XXX: XEmacs doesn't have
      ;; `substring-no-properties'.
      (set-text-properties 0 (length pkg) nil pkg)
      (setq pkg (TeX-read-string "View documentation for: " pkg))
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

  ;; Add environment for auto. insertion with C-c C-e, and some env. specific
  ;; configuration such as indentation, etc
  (defun lps/latex-add-environments ()
    ;; Should be done by auctex's tikz.el FILE
    ;; (LaTeX-add-environments '("tikzpicture" LaTeX-env-label))
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture")))

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

  (defun LaTeX-replace-in-math ()
    "Call `query-replace' with `isearch-filter-predicate'set to
filter out matches outside LaTeX math environments."
    (interactive)
    (let ((isearch-filter-predicate
           (lambda (beg end)
             (save-excursion (save-match-data (goto-char beg) (texmathp)))))
          (case-fold-search nil))
      (call-interactively 'query-replace)))

  (defun LaTeX-replace-regexp-in-math ()
    "Call `query-replace-regexp' with `isearch-filter-predicate'set
to filter out matches outside LaTeX math environments."
    (interactive)
    (let ((isearch-filter-predicate
           (lambda (beg end)
             (save-excursion (save-match-data (goto-char beg) (texmathp)))))
          (case-fold-search nil))
      (call-interactively 'query-replace-regexp)))

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
     (TeX-command-sequence t t))))

(use-package bibtex
  :defer t
  :bind
  (:map bibtex-mode-map
        ("C-c C-?" . bibtex-print-help-message)))

(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-toc-split-windows-horizontally t)
  (reftex-label-alist
   '(("section"     ?s "sec:"  "~\\ref{%s}" t (regexp "[Ss]ection\\(s\\)?"       ))
     ("definition"  ?d "def:"  "~\\ref{%s}" t (regexp "[Dd]efinition\\(s\\)?"    ))
     ("example"     ?x "ex:"   "~\\ref{%s}" t (regexp "[Ee]xample\\(s\\)?"       ))
     ("lemma"       ?l "lem:"  "~\\ref{%s}" t (regexp "[Ll]emma\\(s\\|ta\\)?"    ))
     ("proposition" ?p "prop:" "~\\ref{%s}" t (regexp "[Pp]roposition\\(s\\)?"   ))
     ("theorem"     ?h "thm:"  "~\\ref{%s}" t (regexp "[Tt]heorem\\(s\\)?"       ))
     ("remark"      ?r "rem:"  "~\\ref{%s}" t (regexp "[Rr]emark\\(s\\)?"        ))
     ("corollary"   ?c "cor:"  "~\\ref{%s}" t (regexp "[Cc]orollar\\(y\\|ies\\)")))))

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
  (defun reftex--query-search-regexps (default)
    "Query for regexps for searching entries using DEFAULT as default.
Return a list of regular expressions."
    (split-string
     (let ((orderless-component-separator "[ \t]*&&[ \t]*"))
       (completing-read
        (concat
         "Regex { && Regex...}: "
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
        nil nil nil 'reftex-cite-regexp-hist))
     "[ \t]*&&[ \t]*")))

(use-package biblio-core
  :config
  ;; We just override this function, to use our own completion
  ;; system.
  ;; We don't want packages to mess up our config !
  (defun biblio--completing-read-function ()
    completing-read-function))

(use-package biblio
  :bind
  (:map bibtex-mode-map
        ("C-c ?" . biblio-lookup))
  (:map pdf-view-mode-map
        ("d" . biblio-lookup))
  :custom
  (biblio-arxiv-bibtex-header "article")
  (biblio-download-directory "~/Documents/Other/articles/")
  (biblio-selection-mode-actions-alist
   '(("Dissemin (find open access copies of this article)" . biblio-dissemin--lookup-record)
     ("Download this article and format its name" . lps/biblio-download--action)))
  :init
  (defun lps/biblio-download--action (record)
    "Retrieve a RECORD from Dissemin, and display it.
RECORD is a formatted record as expected by `biblio-insert-result'.
The default filename is of the form \"[AUTHORS]TITLE.pdf\" where
AUTHORS is a list of the authors surnames, separated by underscores,
and TITLE is the result of `lps/make-filename-from-sentence' on the
article's title"
    (let-alist record
      (if .direct-url
          (let* ((fname (with-temp-buffer
                          (insert .title)
                          (goto-char (point-min))
                          (lps/make-filename-from-sentence)
                          (goto-char (point-max))
                          (insert ".pdf")
                          (goto-char (point-min))
                          (insert "[")

                          (seq-doseq (name .authors)
                            (when (and name (stringp name))
                              (let ((split-name (split-string name)))
                                (if (cdr split-name)
                                    (dolist (subname (cdr split-name))
                                      (insert subname))
                                  (insert name)))
                              (insert "_")))
                          (delete-backward-char 1)
                          (insert "]")
                          (buffer-substring-no-properties (point-min) (point-max))))
                 (target (read-file-name "Save as: "
                                         biblio-download-directory
                                         fname
                                         nil
                                         fname)))
            (url-copy-file .direct-url
                           (expand-file-name target
                                             biblio-download-directory)))
        (user-error "This record does not contain a direct URL")))))

(use-package preview
  :ensure nil ;; Comes with AUCTeX
  :defer t
  :config
  (setq preview-auto-reveal t)
  (setq preview-auto-cache-preamble t)
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
        ("<C-return>" . nil))
  :custom
  (cdlatex-paired-parens "$([{")
  (cdlatex-make-sub-superscript-roman-if-pressed-twice t)
  (cdlatex-simplify-sub-super-scripts nil)
  (cdlatex-math-modify-prefix "C-^")
  (cdlatex-takeover-dollar nil)
  (cdlatex-auto-help-delay 1.0)
  (cdlatex-takeover-parenthesis nil)
  (cdlatex-math-symbol-prefix ?°)
  (cdlatex-math-symbol-alist
   '((?< ("\\leq"))
     (?> ("\\geq"))
     (?\[ ("\\subseteq" "\\sqsubseteq" "\\sqsubset"))
     (?\] ("\\supseteq" "\\sqsupseteq" "\\sqsupset"))
     (?: ("\\colon"))
     (?- ("\\cap" "\\bigcap"))
     (?+ ("\\cup" "\\bigcup" "\\sqcup" "\\bigsqcup"))
     (?* ("\\times" "^{\\ast}"))
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
   '(("prodl"       "Insert \\prod\\limits_{}^{}"
      "\\prod\\limits_{?}^{}" cdlatex-position-cursor nil nil t)
     ("lim"         "Insert \\lim\\limits_{}"
      "\\lim\\limits_{?}" cdlatex-position-cursor nil nil t)))

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
              :around #'lps/cdlatex-sub-superscript-wrap-region))

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
  (use-package bash-completion
    :disabled t
    :hook (eshell-mode . bash-completion-setup))

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

;; Straight from Centaur Emacs
(use-package esh-autosuggest
  :disabled t
  :defer t
  :hook (eshell-mode . esh-autosuggest-mode)
  :custom
  (esh-autosuggest-use-company-map t))

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
  :defer t
  :bind
  (:map dired-mode-map
        ("F" . find-name-dired))
  :custom
  ;; Delete and copy directories recursively
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alFh")
  (dired-isearch-filenames 'dwim)
  (dired-listing-switches "-AlFh --group-directories-first")
  (wdired-allow-to-change-permissions t))

;; Make things prettier
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode)
  :custom
  (all-the-icons-dired-monochrome nil))

(use-package dired-x
  :ensure nil
  :after dired)

(use-package find-dired
  :ensure nil
  :bind
  (:map lps/system-tools-map
        ("f f" . find-name-dired)
        ("f g" . find-grep-dired)
        ("f l" . locate)
        ("f L" . locate-with-filter)))

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
  :bind
  (:map lps/system-tools-map
        ("p p" . proced))
  (:map proced-mode-map
        ("a" . proced-toggle-auto-update)))

(use-package emacs
  :ensure nil
  :bind
  (:map lps/system-tools-map
        ("p l" . list-processes)))

(use-package smtpmail
  :ensure nil
  :after mu4e
  :config
  (setq message-send-mail-function 'smtpmail-send-it)
  ;; Default SMTP configuration
  (setq smtpmail-debug-info t)
  (setq smtpmail-smtp-user "paviets201")
  (setq smtpmail-smtp-server "smtp.unicaen.fr")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'ssl))

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
               ("C-S-s" . lps/mu4e-build-query)))
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  (set-variable 'read-mail-command 'mu4e)
  :config
  (defvar lps/safe-mail-send t "If non-nil, ask for a signature, an encryption, and ask confirmation when sending a non-multipart MIME mail")

  (setq mu4e-completing-read-function 'completing-read)

  ;; Security issues
  (add-hook 'mu4e-main-mode-hook #'lps/auth-source-define-cache-expiry)

  ;; Might avoid unwanted drafts
  (add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1)))

  ;; Convenience functions
  (setq mu4e-compose-context-policy 'always-ask)
  (setq mu4e-context-policy 'pick-first)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-confirm-quit nil)

  ;; View images
  (when (version< mu4e-mu-version "1.7")
    (setq mu4e-view-show-images t))
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; ASCII-only time is over
  (setq mu4e-use-fancy-chars t)
  ;; and fix alignment !
  (setq mu4e-headers-precise-alignment t)

  ;; Unless we want to send mail to very old clients
  (setq mu4e-compose-format-flowed t)

  ;; Avoid mail syncing issues with mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail every 5 minutes
  (setq mu4e-update-interval (* 5 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-index-update-in-background t)
  (setq mu4e-hide-index-messages t)

  ;; Always show full date and time
  (setq mu4e-headers-date-format "%d-%m-%Y %H:%M")

  ;; Less redundant information
  (setq mu4e-headers-include-related nil)
  (setq mu4e-headers-show-threads nil)

  ;; Keep one mail per line
  ;; Todo: fix so that it updates when window is resized
  (setq mu4e-headers-fields '((:human-date . 20)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from-or-to . 22)
                              (:subject . 100)))

  (defun lps/resize-headers-fields ()
    (if (eq major-mode 'mu4e-headers-mode)
        (let ((width (window-body-width)))
          (setq-local mu4e-headers-fields `((:human-date . 20)
                                            (:flags . 6)
                                            (:mailing-list . 10)
                                            (:from-or-to . 22)
                                            (:subject . ,(- width (+ 20 6 10 22 15))))))))

  (add-hook 'mu4e-headers-mode-hook #'lps/resize-headers-fields)

  ;; Change: obsolete variable
  (setq mu4e-maildir "~/Mail")

  ;; Adapted from https://jherrlin.github.io/posts/emacs-mu4e/
  ;; See also https://etienne.depar.is/emacs.d/mu4e.html
  (setq mml-secure-cache-passphrase nil)
  ;;(setq mml-secure-openpgp-sign-with-sender t)
  (setq mml-secure-openpgp-encrypt-to-self t)
  ;;(setq mml-secure-smime-sign-with-sender t)
  (setq mml-smime-encrypt-to-self t) ;; encryption is not ready yet

  (setq mm-sign-option 'guided)

  (defun lps/sign-or-encrypt-message ()
    (when lps/safe-mail-send
      (let ((answer (read-from-minibuffer (concat "Sign or encrypt?\n"
                                                  "Empty to do nothing.\n[s/e]: "))))
        (cond
         ((string-equal answer "s") (progn
                                      (message "Sign this message.")
                                      ;; Why doesn't mml-secure-message-sign-pgpmime work ... ?
                                      (mml-secure-message-sign-pgpmime) ;;Works but only signs a part of the message.
                                      (message "Done trying to sign the message")))
         ((string-equal answer "e") (progn
                                      (message "Encrypt and sign this message.")
                                      (mml-secure-message-encrypt-pgpmime)
                                      (message "Done trying to encrypt the message")))
         (t (progn
              (message "Not signing or encrypting this message.")
              nil))))))

  (add-hook 'message-send-hook 'lps/sign-or-encrypt-message)


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

  ;; Override this function to have a more friendly name
  ;; for viewed message
  (defun mu4e-view (msg)
    "Display the message MSG in a new buffer, and keep in sync with HDRSBUF.
'In sync' here means that moving to the next/previous message in
the the message view affects HDRSBUF, as does marking etc.

As a side-effect, a message that is being viewed loses its 'unread'
marking if it still had that."

    (mu4e~headers-update-handler msg nil nil) ;; update headers, if necessary.

    (when (get-buffer gnus-article-buffer) ; modify: BUFFERP to GET-BUFFER
      (kill-buffer gnus-article-buffer))
    ;; add this expression compared to the original function
    (setq gnus-article-buffer (let* ((subj (mu4e-msg-field msg :subject))
                                     (subj (unless (and subj (string-match "^[:blank:]*$" subj)) subj))
                                     (str (or subj
                                              "*Article*")))
                                (generate-new-buffer-name
                                 (truncate-string-to-width str mu4e~compose-buffer-max-name-length)
                                 gnus-article-buffer)))
    (with-current-buffer (get-buffer-create gnus-article-buffer)
      (let ((inhibit-read-only t))
        (remove-overlays (point-min)(point-max) 'mu4e-overlay t)
        (erase-buffer)
        (insert-file-contents-literally
         (mu4e-message-field msg :path) nil nil nil t)))
    (switch-to-buffer gnus-article-buffer)
    (setq mu4e~view-message msg)
    (mu4e~view-render-buffer msg))

  (defun lps/--mu4e-read-date (&optional prompt)
    (let ((time (decode-time (org-read-date nil t))))
      (format "%04d%02d%02d"
              (decoded-time-year time)
              (decoded-time-month time)
              (decoded-time-day time))))

  (defun lps/--mu4e-read-date-range (&optional prompt)
    (concat (lps/--mu4e-read-date "Mail received or sent between ...")
            ".."
            (lps/--mu4e-read-date "... and ...")))

  (defun lps/--mu4e-read-mime-type (&optional prompt)
    (require 'mailcap)
    (mailcap-parse-mimetypes)
    (completing-read (or prompt "Mime type: ") (mailcap-mime-types)))

  (defvar lps/--mu4e-build-query-alist
    '((?\C-m "confirm" "confirm")
      (?\  " anything" "" read-string)
      (?f "from" "from:" read-string)
      (?t "to" "to:" read-string)
      (?d "date" "date:" ((?d "date" "" lps/--mu4e-read-date)
                          (?r "range" "" lps/--mu4e-read-date-range)))
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
                          (?S "Signed" "signed")))
      (?s "subject" "subject:" read-string)
      (?m "mime" "mime:" lps/--mu4e-read-mime-type)))

  (defun lps/--mu4e-parse-query (choices)
    (let* ((choice (read-multiple-choice "Query element: " choices))
           (rest (cddr choice))
           (str (car rest))
           (read-fun-or-continue (cadr rest)))
      (cond
       ((char-equal (car choice) ?\n)
        :quit)
       ((functionp read-fun-or-continue)
        (concat str (funcall read-fun-or-continue (concat str " "))))
       ((consp read-fun-or-continue)
        (concat str (lps/--mu4e-parse-query read-fun-or-continue)))
       (t str))))

  (defun lps/mu4e-build-query (&optional start-query)
    "Provides a simpler interface to build mu4e search queries.
A caveat is that it does not insert logical separators (NOT, AND,
OR ...) between expressions, so the expression has to be modified
by hand if needed"
    (interactive "P")
    (let ((choices lps/--mu4e-build-query-alist)
          (query-list (if start-query
                          (list (completing-read "Search for: "
                                                 mu4e--search-hist
                                                 nil
                                                 nil
                                                 nil
                                                 'mu4e--search-hist))
                        nil))
          (choice nil))
      (while (not (eq (setq choice (lps/--mu4e-parse-query choices)) :quit))
        (push choice query-list))
      (let ((query (mapconcat 'identity (reverse query-list) " ")))
        (mu4e-search query "Search for: " t)))))

(use-package mu4e-alert
  :after mu4e
  :disabled t
  :config
  ;; Temporary fix: mu4e and mu4e-alert are out of sync
  ;; while mu4e changes its naming conventions from the
  ;; old mu4e~<stuff> to mu4e--<stuff>

  (defadvice mu4e-context-switch (around mu4e-alert-update-mail-count-modeline disable)
    "Advice `mu4e-context-switch' to update mode-line after changing the context."
    (let ((context mu4e--context-current))
      ad-do-it
      (unless (equal context mu4e--context-current)
        (mu4e-alert-update-mail-count-modeline))))

  (mu4e-alert-enable-mode-line-display))

;; From https://github.com/iqbalansari/dotEmacs/blob/master/config/mail.org
(use-package gnus-dired
    :ensure nil
    :after mu4e
    :hook (dired-mode . turn-on-gnus-dired-mode)
    :config
    ;; This overrides a function !
    (defun gnus-dired-mail-buffers ()
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
    :ensure nil
    :after gnus-dired
    :bind (:map dired-mode-map
                ("E" . lps/mu4e-file-attach-marked-files))
    :config
    (defun lps/mu4e-file-attach-marked-files ()
      (interactive)
      (gnus-dired-attach (dired-map-over-marks (dired-get-file-for-visit) nil))))

(use-package org-mime
  :after mu4e
  :config
  (defun lps/safe-org-mime-confirm-when-no-multipart ()
    (when lps/safe-mail-send
      (org-mime-confirm-when-no-multipart)))

  ;; Make sure that this hook is added AFTER lps/sign-or-encrypt-message
  ;; so that it is executed BEFORE it.
  ;; We want to htmlize, then sign/encrypt, not the other way around !
  (add-hook 'message-send-hook 'lps/safe-org-mime-confirm-when-no-multipart)
  (setq org-mime-export-options'(:section-numbers nil
                                                  :with-author nil
                                                  :with-toc nil))

  ;; Hacky function to avoid big formatting problems when calling org-mime-htmlize
  ;; after having linked attachments, or signing/encrypting the message
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

(use-package mu4e-column-faces
  :after mu4e
  :config
  (when (version<= "1.8.0" mu4e-mu-version)
    (mu4e-column-faces-mode)))

(use-package elpher)

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

(use-package flyspell-correct
  :after flyspell
  :bind
  (:map flyspell-mode-map
        ("C-$" . flyspell-correct-wrapper)))

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
  ("C-S-<f8>" . lps/flyspell-toggle)
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
      (let* (boundary mimep
                      (ispell-skip-region-alist-save ispell-skip-region-alist)
                      ;; Nil when message came from outside (eg calling Emacs as editor)
                      ;; Non-nil marker of end of headers.
                      (internal-messagep
                       (re-search-forward
                        (concat "^" (regexp-quote mail-header-separator) "$") nil t))
                      (end-of-headers   ; Start of body.
                       (copy-marker
                        (or internal-messagep
                            (re-search-forward "^$" nil t)
                            (point-min))))
                      (limit (copy-marker ; End of region we will spell check.
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
                      (cite-regexp      ;Prefix of quoted text
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
            (flyspell-prog-mode 1))
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
      (lps/flyspell-on-for-buffer-type)))

  (defun lps/ispell-change-personal-dictionary (code &optional kill-ispell)
    (setq ispell-personal-dictionary
          (expand-file-name code lps/ispell-personal-dictionaries-dir))
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
    (when (member dict (directory-files lps/ispell-personal-dictionaries-dir))
      (lps/ispell-change-personal-dictionary dict)
      (ispell-change-dictionary dict))))

(use-package guess-language
  ;;:hook (text-mode . guess-language-mode)
  :custom
  (guess-language-languages '(en fr))
  (guess-language-after-detection-functions '(guess-language-switch-flyspell-function)))

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
      (artist-key-do-continously-common))))

(use-package calendar
  :ensure nil
  :custom
  (calendar-view-holidays-initially-flag t)
  (calendar-mark-holidays-flag t)
  :config
  (calendar-set-date-style 'european)

  (defvar holiday-french-holidays
    `((holiday-fixed 1 1 "Jour de l'an")
      (holiday-fixed 1 6 "Épiphanie")
      (holiday-fixed 2 2 "Chandeleur")
      (holiday-fixed 2 14 "Saint Valentin")
      (holiday-fixed 5 1 "Fête du travail")
      (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
      (holiday-fixed 6 21 "Fête de la musique")
      (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
      (holiday-fixed 8 15 "Assomption (Religieux)")
      (holiday-fixed 11 11 "Armistice de 1918")
      (holiday-fixed 11 1 "Toussaint")
      (holiday-fixed 11 2 "Commémoration des fidèles défunts")
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

(use-package elfeed
  :defer t
  :bind
  ("C-c f" . elfeed)
  (:map elfeed-search-mode-map
        ("w" . elfeed-search-browse-url))
  :custom
  (elfeed-db-directory (concat user-emacs-directory ".elfeed"))
  (elfeed-search-title-max-width 110)
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread -compsci -youtube"))

(use-package elfeed-org
  :after elfeed
  :init
  (defvar lps/elfeed-default-days-range 7
    "Range of days to filter by default in elfeed search queries")
  :bind (:map elfeed-search-mode-map
              ("s" . lps/elfeed-search-filter-interactive))
  :config
  (setq rmh-elfeed-org-files '("~/Documents/OrgFiles/elfeed.org"))
  (elfeed-org)

  (defun lps/elfeed-search-filter-prompt-time-range ()
    (let* ((default-time (time-subtract (current-time)
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
          (elfeed-search-update :force))))))

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
             (with-demoted-errors (delete-file socket)))
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

(use-package xkcd
  :defer t)

(use-package speed-type
  :defer t
  :custom (speed-type-default-lang 'French)) ; Todo: fix bad behaviour !

(use-package key-quiz
  :defer t)
