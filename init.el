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
  (package-refresh-contents))

(setq package-native-compile t)

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

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
  (auth-source-cache-expiry 86400);; All day

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
    (setq lps/--auth-cache-expiry-setup-p nil))

  (add-hook 'kill-emacs-hook #'lps/force-forget-all-passwords))

(use-package restart-emacs
  :commands
  (restart-emacs restart-emacs-start-new-emacs)
  :bind
  (:map lps/system-tools-map
        ("r" . restart-emacs)))

(setq custom-file (concat user-emacs-directory "custom-file.el"))
(load custom-file 'noerror)

(use-package emacs
  :custom
  (native-comp-async-report-warnings-errors 'silent))

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

;; Disable the message on top of the Scratch buffer
(setq initial-scratch-message nil)

;; Maximize the Emacs frame at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Global line/column numbering mode
;; Modes in which we might want to disable it:

(column-number-mode t)
(global-display-line-numbers-mode t)

(defun lps/disable-line-numbers ()
  (display-line-numbers-mode 0))

(defvar lps/disabled-line-numbers-modes-hooks '(org-mode-hook
                                                ;; Term & Shells
                                                eshell-mode-hook
                                                comint-mode-hook
                                                cider-repl-mode-hook ; does not derive from comint-mode ...
                                                ;; PDF viewers
                                                pdf-view-mode-hook
                                                doc-view-mode-hook
                                                ;; Help modes
                                                helpful-mode-hook
                                                help-mode-hook
                                                apropos-mode-hook
                                                Info-mode-hook
                                                ;; mu4e
                                                mu4e-main-mode-hook
                                                mu4e-view-mode-hook
                                                mu4e-headers-mode-hook
                                                ;; reading
                                                nov-mode-hook
                                                olivetti-mode-hook
                                                ;; Extra modes
                                                undo-tree-visualizer-mode-hook
                                                treemacs-mode-hook
                                                dired-mode-hook
                                                occur-mode-hook))

(dolist (mode-hook lps/disabled-line-numbers-modes-hooks)
  (add-hook mode-hook #'lps/disable-line-numbers))

(global-visual-line-mode 1)

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

(defvar lps/default-theme 'kaolin-ocean)
(defvar lps/default-light-theme 'modus-operandi)
(defvar lps/live-presentation-p nil)

(load-theme lps/default-theme t)

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

;; Use this to store your favourite themes
;; Save your usual, default theme in first position
;; so that you can easily switch back to it with
(setq lps/rotate-themes-list
      '(doom-Iosvkem
        kaolin-ocean
        kaolin-aurora
        doom-palenight
        tsdh-dark
        solarized-dark
        modus-vivendi))

;; Try to save the current theme
;; Be careful ! Some visual changes are NOT stored in
;; a theme, and will not be retrieved by the restoring
;; functions. For example, any font configuration might
;; be "lost" for this session
(setq lps/initial-enabled-themes custom-enabled-themes)

(setq lps/rotate-theme-index 0)

;; Still a bit buggy: forgets all the customizations done to e.g. Org Mode
(defun lps/rotate-through-themes ()
  "Cycles through the next theme in the `lps/rotate-themes-list'.
If this list is empty or does not exist, cycle through all the
installed themes instead."
  (interactive)
  (mapc #'disable-theme lps/initial-enabled-themes)
  (let* ((themes-list (or (and (boundp 'lps/rotate-themes-list) lps/rotate-themes-list)
                          (custom-available-themes)))
         (next-index (mod (+ lps/rotate-theme-index 1) (length themes-list)))
         (current-theme (nth lps/rotate-theme-index themes-list))
         (next-theme (nth next-index themes-list)))
    (setq lps/rotate-theme-index next-index)
    (disable-theme current-theme)
    (load-theme next-theme t)))

(defun lps/restore-initial-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (mapc (lambda (theme) (funcall #'load-theme theme t)) lps/initial-enabled-themes)
  (when (eq major-mode 'org-mode)
    (lps/org-mode-setup)
    (font-lock-update)))

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
  :config
  ;; Hide encoding in modeline when UTF-8(-unix)
  (defun lps/hide-utf-8-encoding ()
    (setq-local doom-modeline-buffer-encoding
                (not (or (eq buffer-file-coding-system 'utf-8-unix)
                         (eq buffer-file-coding-system 'utf-8)))))

  (add-hook 'after-change-major-mode-hook #'lps/hide-utf-8-encoding)

  ;; Add recursive-depth info to the mode line
  ;; Useful for e.g. Isearch sessions
  (setq global-mode-string (list
                            '(:eval
                              (let ((rec-depth (recursion-depth)))
                                (unless (zerop rec-depth)
                                  (propertize (format "[%d] " rec-depth)
                                              'face
                                              '(:foreground "orange red")))))
                            global-mode-string)))

(use-package battery
  :ensure nil
  :init
  (display-battery-mode 1))

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

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(use-package calendar
  :ensure nil
  :config
  (calendar-set-date-style 'european))

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  :bind
  ("s-g" . exit-recursive-edit)
  :config
  (minibuffer-depth-indicate-mode 1))

;; Ivy
(use-package ivy
  :diminish
  :disabled t
  :init
  (setq completing-read-function 'ivy-completing-read)
  :bind (("C-s" . swiper)
         :map swiper-map
         ("M-g" . swiper-avy)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial-or-done)
         ("C-l" . ivy-immediate-done)
         ("C-SPC" . lps/ivy-toggle-current-mark)
         ("<mouse-3>" . nil)
         ("<mouse-1>" . nil)
         ("<down-mouse-1>" . nil))
  :custom
  (ivy-count-format "(%d/%d)")
  (ivy-initial-inputs-alist nil)
  (ivy-extra-directories nil)

  :config
  (ivy-mode 1)

  (defun lps/ivy-toggle-current-mark ()
    (interactive)
    "Toggle mark for current candidate and move forwards."
    (if (ivy--marked-p)
        (ivy-unmark)
      (ivy-mark))))

(use-package ivy-hydra
  :after ivy
  :defer t)

;; Adds things to Ivy
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; Counsel. Adds things to Ivy
(use-package counsel
  :diminish
  :disabled t
  :after ivy
  :hook (ivy-mode . counsel-mode)
  :custom (counsel-find-file-at-point t)
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer) ;; counsel-ibuffer is a fancier option
         ("C-x C-f" . counsel-find-file)
         ("C-c i" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

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
  ("s-k" . kill-this-buffer)
  :init
  ;; Automatically reload a file if it has been modified
  (global-auto-revert-mode t)

  :custom
  (display-buffer-base-action
   '((display-buffer-reuse-window)
     (display-buffer-reuse-mode-window)
     (display-buffer-same-window)
     (display-buffer-in-previous-window)))

  :config

  ;; Display all the "help" buffers in the same window
  (defvar lps/help-modes '(helpful-mode
                           help-mode
                           Man-mode
                           apropos-mode
                           Info-mode))

  ;; Help buffers with special name
  (defvar lps/help-buffers nil)

  (defun lps/buffer-help-p (buffer action)
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

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

(use-package all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1))

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
      ("Help" (or
               (mode . helpful-mode)
               (mode . Info-mode)
               (mode . help-mode)))
      ("Special" (or
                  (name . "^\\*.*\\*$")
                  (mode . special-mode)))
      ("Images/PDF" (or
                     (file-extension . "pdf")
                     (mode . image-mode)))
      ("Programming" (and
                      (derived-mode . prog-mode)
                      (not (mode . fundamental-mode)))))))
  :config
  (add-to-list 'ibuffer-help-buffer-modes 'helpful-mode)

  (defun lps/ibuffer-switch-to-default-filter ()
    (ibuffer-switch-to-saved-filter-groups "default"))

  (add-hook 'ibuffer-mode-hook #'lps/ibuffer-switch-to-default-filter))

(use-package emacs
  :ensure nil
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
  :bind
  (:map ctl-x-x-map
        ("R" . lps/rename-current-buffer-file)))

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

(use-package outline
  :ensure nil
  :defer t
  :custom
  (outline-minor-mode-prefix "\C-o")
  (outline-minor-mode-cycle t))

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :custom
  (counsel-describe-symbol-function   #'helpful-symbol)
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-symbol]   . helpful-symbol)
  ([remap describe-key]      . helpful-key)
  ("C-h u"                   . helpful-at-point)) ;; Help "<u>nder" cursor

(use-package help-at-pt
  :ensure nil
  :custom
  (help-at-pt-display-when-idle t)
  (help-at-pt-timer-delay 0.5))

;; Inspired from https://emacs.stackexchange.com/questions/2777/how-to-get-the-function-help-without-typing

(use-package popup
  :init
  (defun lps/describe-thing-in-popup ()
    (interactive)
    (let* ((thing (symbol-at-point))
           (help-xref-following t)
           (description (save-window-excursion
                          (with-temp-buffer
                            (help-mode)
                            (help-xref-interned thing)
                            (buffer-string)))))
      (popup-tip description
                 :point (point)
                 :around t
                 :margin t
                 :height 20)))

  (global-set-key (kbd "C-&") #'lps/describe-thing-in-popup))

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

(use-package ivy-prescient
  :after ivy prescient
  :custom
  (ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode 1)
  (setq ivy-prescient-sort-commands
        (append ivy-prescient-sort-commands
                '(counsel-minibuffer-history
                  counsel-shell-history
                  imenu
                  counsel-imenu))))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package command-log-mode
  :defer t)

;; Type "y" instead of "yes RET" for confirmation
(if (version< emacs-version "28.0") ; ) parsing bug
    (defalias 'yes-or-no-p 'y-or-n-p)
  (setq use-short-answers t))

;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init
  (which-key-mode 1)
  (which-key-setup-side-window-bottom) ;; default
  :diminish
  :custom
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.05))

(use-package consult
  :defer t
  :bind
  ("C-s" . lps/consult-line-strict-match)
  ("C-c i" . lps/consult-imenu-or-org-heading)
  ("C-x b" . consult-buffer)
  :custom
  (consult-narrow-key "<")
  :config
  (defun lps/consult-imenu-or-org-heading ()
    (interactive)
    (if (equal major-mode 'org-mode)
        (consult-org-heading)
      (consult-imenu)))

  (defun lps/consult-line-strict-match (&optional initial start)
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((orderless-matching-styles '(orderless-literal)))
      (consult-line initial start))))

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

;; Macro to use "python-style" affectation in lexical bindings
(defmacro multi-let (vars values body)
  "Binds each symbol of VARS to its corresponding expression in VALUES,
  in order.
  multi-let (a b) (e1 e2) body is thus equivalent to
  (let ((a e1)) (let ((b e2)) body))
  Expressions at position k in VALUES might depend on symbol from
  VARS at position strictly less than k, as with let*"
  (defun rec-expand-let (vars values body)
    (if (= (length vars) (length values))
        (if (and vars (symbolp (car vars)))
            `(let ((,(car vars) ,(car values)))
               ,(rec-expand-let (cdr vars)
                                (cdr values)
                                body))
          body)
      (message
       (format "Trying to bind %d symbols to %d values"
               (length vars)
               (length values)))))

  (rec-expand-let vars values body))

(use-package ffap
  :ensure nil
  :init (ffap-bindings)
  :config
  (defun lps/find-file-as-root (filename)
    "Switch to a buffer visiting the file FILENAME as root, creating one if none exists."
    (interactive "P")
    (find-file (concat "/sudo:root@localhost:" filename))))

(when (version< "28.0" emacs-version)
  (use-package repeat
    :init
    (repeat-mode 1)
    :bind
    (:map lps/quick-edit-map
          ("z" . repeat))))

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
  :bind (:map lps/all-hydras-map
              ("M" . hydra-multiple-cursors/body))
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
      ("M-p" mc/unmark-previous-like-this "Unmark previous like this")))))

(use-package orderless
  :custom
  (completion-styles '(basic partial-completion orderless))
  (completion-auto-help t)
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-literal orderless-regexp))
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
      `(orderless-without-literal . ,(substring pattern 1))))))

;; Company. Auto-completion package
(use-package company
  :diminish
  :init
  (global-company-mode t)
  :hook
  (prog-mode . lps/company-default-backends-prog)
  (text-mode . lps/company-default-backends-text)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete)
        ("TAB" . company-complete)
        ("RET" . nil)
        ("<return>" . nil)
        ("<C-return>" . company-complete-selection)
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
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
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
    (setq-local company-backends '((company-capf company-files company-dabbrev company-yasnippet)
                                   (company-dabbrev-code company-gtags company-etags company-keywords company-clang)
                                   company-oddmuse)))

  (defun lps/company-default-backends-text ()
    (setq-local company-backends '((company-capf company-files company-dabbrev company-ispell company-yasnippet)
                                   company-oddmuse)))

  ;; AZERTY-friendly company number selection
  ;; Might lead to company-box being a bit broken ? Long function names are cut-off

  (dolist (map (list company-active-map company-search-map))
    (mapc (lambda (x) (define-key map (read-kbd-macro (format "M-%s" (cdr x)))
                        `(lambda () (interactive) (company-complete-number ,(car x)))))
          '((10 . "à")
            (1 . "&")
            (2 . "é")
            (3 . "\"")
            (4 . "'")
            (5 . "(")
            (6 . "-")
            (7 . "è")
            (8 . "_")
            (9 . "ç"))))

  ;; Experimental support for multi-backend 'keep-prefix behaviour
  ;; I simply deleted a test from the original function, which
  ;; used to set to t a value when I wanted it to be keep-prefix
  ;; If something breaks, just delete this from the config
  (defun company--multi-backend-adapter (backends command &rest args)
    (let ((backends (cl-loop for b in backends
                             when (or (keywordp b)
                                      (company--maybe-init-backend b))
                             collect b))
          (separate (memq :separate backends)))

      (when (eq command 'prefix)
        (setq backends (butlast backends (length (member :with backends)))))

      (setq backends (cl-delete-if #'keywordp backends))

      (pcase command
        (`candidates
         (company--multi-backend-adapter-candidates backends (car args) separate))
        (`sorted separate)
        (`duplicates (not separate))
        ((or `prefix `ignore-case `no-cache `require-match)
         (let (value)
           (cl-dolist (backend backends)
             (when (setq value (company--force-sync
                                backend (cons command args) backend))
               (cl-return value)))))
        (_
         (let ((arg (car args)))
           (when (> (length arg) 0)
             (let ((backend (or (get-text-property 0 'company-backend arg)
                                (car backends))))
               (apply backend command args)))))))))

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
  :custom
  (sentence-end-double-space nil))

(use-package isearch
  :ensure nil
  :bind
  (:map search-map
        ("s" . isearch-forward)
        ("M-s" . isearch-forward) ;; avoids early/late release of Meta
        ("r" . isearch-backward)
        ("x" . isearch-forward-regexp))
  :custom
  ;; Interpret whitespaces as "anything but a newline"
  (search-whitespace-regexp ".*?")
  (isearch-regexp-lax-whitespace t)
  (isearch-yank-on-move t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t))

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
  :bind ("C-ù" . avy-goto-char-timer)
  :custom
  ;; Using an AZERTY keyboard home row
  (avy-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l ?m))
  (avy-all-windows nil)
  (avy-single-candidate-jump nil )
  (avy-timeout-seconds 0.5)
  (avy-translate-char-function '(lambda (c) (if (= c 32) ?q c))))

(use-package emacs
  :ensure nil
  :bind (:map lps/all-hydras-map
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
    ^^^^          _u_ndo        _q_ quit     _i_nsert-string-rectangle      '---''(./..)-'(_\_)
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
    ("i" string-insert-rectangle)
    ("u" undo nil)
    ("q" nil)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package emacs
  :ensure nil
  :bind
  ("M-k" . lps/copy-line-at-point)
  ("M-à" . lps/select-line)
  :custom
  (kill-read-only-ok t)
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

  (defun lps/select-line ()
    "Select the current line. If the region is already active, extends the current selection by line."
    (interactive)
    (if (region-active-p)
        (progn
          (forward-line 1)
          (end-of-line))
      (progn
        (end-of-line)
        (set-mark (line-beginning-position))))))

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
  :init
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
    (let ((matches (lps/find-delete-forward-all-regexp re beg)))
      (prin1 matches)
      (goto-char (or move (point-max)))
      (while matches
        (insert (or split ""))
        (insert (pop matches))))))

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
  ("<M-dead-circumflex>" . delete-indentation)
  (:map lps/quick-edit-map
        ("C-u" . lps/underline-or-frame-dwim)
        ("k" . zap-up-to-char))

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
    "Underlines the current line with the string STR or with \"-\"
if none is provided.
If called interactively, prompt for STR.
With a prefix argument, frame the line using STR instead.
Breaks if region or line spans multiple visual lines"
    (interactive (list (let ((default "-"))
                         (read-string (concat "Use string (default " default " ): ") nil nil "-"))
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
              (goto-char (+ len to))
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
            (lps/--fill-width-repeat-string width str)))))))

(use-package projectile
  :diminish
  :disabled t ;; try Project.el instead
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-completion-system 'ivy)
  :config
  (let ((path-project "~/Documents/Projects"))
    (when (file-directory-p path-project)
      (setq projectile-project-search-path (list path-project))))
  (projectile-mode))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package magit
  :defer t
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer
  :bind
  ("C-x g" . magit-status)
  (:map magit-section-mode-map
        ("<M-dead-circumflex>" . magit-section-up)))

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
  (show-paren-when-point-in-periphery t))

;; rainbow-delimiters. Hightlights with the same colour matching parenthesis
(use-package rainbow-delimiters
  :hook ((prog-mode comint-mode fundamental-mode) . rainbow-delimiters-mode))

;; Smartparens is currently bugged
(use-package smartparens
  :disabled t
  :custom (sp-highlight-pair-overlay nil)
  :hook (smartparens-mode . show-smartparens-mode)
  :bind
  ("C-M-f" . sp-forward-sexp)
  ("C-M-b" . sp-backward-sexp)

  ;; Define those as in paredit
  ("C-M-n" . sp-up-sexp)
  ("C-M-d" . sp-down-sexp)
  ("C-M-u" . sp-backward-up-sexp)
  ("C-M-p" . sp-backward-down-sexp)

  ("C-S-a" . sp-beginning-of-sexp)
  ("C-S-e" . sp-end-of-sexp)

  ("C-M-t" . sp-transpose-sexp)

  ("C-M-k" . sp-kill-sexp)
  ("C-M-w" . sp-copy-sexp)

  ("M-<delete>" . sp-unwrap-sexp)
  ("M-<backspace>" . sp-backward-unwrap-sexp)

  ("C-<right>" . sp-forward-slurp-sexp)
  ("C-<left>" . sp-forward-barf-sexp)
  ("C-M-<left>" . sp-backward-slurp-sexp) ; kbd ghosting ?
  ("C-M-<right>" . sp-backward-barf-sexp) ; kbd ghosting ?

  ("M-s" . sp-splice-sexp) ; unbinds "occur"
  ;; ("C-M-<delete>" . sp-splice-sexp-killing-forward)
  ;; ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
  ;; ("C-S-<backspace>" . sp-splice-sexp-killing-around)

  ("M-F" . sp-forward-symbol)
  ("M-B" . sp-backward-symbol))


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
  (:map paredit-mode-map
        ("C-M-y" . paredit-copy-as-kill)
        ("M-s" . nil) ;; To get isearch-mode-map
        ("M-s M-s" . paredit-splice-sexp)
        ("C-M-," . paredit-convolute-sexp)
        ("C-S-t" . transpose-sexps)
        ("M-j" . eval-print-last-sexp)
        ([remap newline] . paredit-newline)
        ("<C-backspace>" . paredit-delete-region)))

(use-package elec-pair
  :hook ((prog-mode
          org-mode
          inferior-python-mode)
         . electric-pair-local-mode)) ;; needed for org-babel

;;YASnippet
(use-package yasnippet
  :diminish
  :config
  (setq yas-verbosity 1)
  :hook ((prog-mode LaTeX-mode) . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("<C-tab>" . yas-expand)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package common-lisp-snippets
  :after yasnippet)

(use-package company-yasnippet
  :ensure nil
  :after company)

(use-package company-dabbrev
  :ensure nil
  :after company
  :custom
  (company-dabbrev-other-buffers t)
  (company-dabbrev-ignore-case 'keep-prefix)
  (company-dabbrev-downcase 'case-replace))

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

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  ;; Sometimes, we don't want to start a full server just to check a file
  ;; or make a few edits to it. In my use, this mostly depends on the session:
  ;; In a quick session, I might not want to start a server for one or two files,
  ;; however, once I start using LSP, there is no reason not to assume that I
  ;; also want to use it by default for other files in the same session
  (defvar lps/--default-lsp-mode 0)
  (defun lps/lsp-by-default-in-session ()
    (if (> lps/--default-lsp-mode 0)
        (lsp-deferred)
      (if (and (= lps/--default-lsp-mode 0) (y-or-n-p "Automatically use lsp-mode in the current session ?"))
          (progn
            (setq lps/--default-lsp-mode 1)
            (lsp))
        (setq lps/--default-lsp-mode -1))))
  :custom
  (lsp-diagnostics-provider :flycheck) ;:none if none wanted

  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-enable-on-type-formatting nil)

  (defun lps/toggle-lsp-by-default-in-session ()
    (interactive)
    (setq lps/--default-lsp-mode (not lps/--default-lsp-mode)))

  :hook ((python-mode c-mode c++-mode) . lps/lsp-by-default-in-session))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  (lsp-ui-sideline-show-code-actions nil))

(use-package lsp-treemacs
  :after lsp-mode
  :config (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :after (lsp-mode ivy))

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
              ("h" . eldoc)))

;; Flycheck
(use-package flycheck
  :defer t
  :config
  ;(setq flycheck-relevant-error-other-file-show nil) ;might be useful
  (setq flycheck-indication-mode 'left-margin)
  :diminish)

(use-package emacs
  :ensure nil
  :bind
  (:map prog-mode-map
        ("<f5>" . compile)))

(use-package python
  :ensure nil
  :defer t
  :hook (python-mode . lps/run-python)
  :custom
  (python-shell-interpreter "python3")
  :config
  (require 'lsp-pyright)
  (defun lps/run-python ()
    (save-excursion
      (call-interactively 'run-python))))

(use-package lsp-pyright
  :defer t)

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
  :hook (lisp-mode . sly-editing-mode)
  :bind
  (:map sly-mode-map
        ("M-_" . nil))
  (:map sly-doc-map
        ("C-g" . nil)
        ("C-h" . nil)
        ("g" . common-lisp-hyperspec-glossary-term)
        ("h" . sly-documentation-lookup))
  (:map sly-prefix-map
        ("C-p" . nil)
        ("M-p" . sly-pprint-eval-last-expression)
        ("C-i" . consult-imenu))
  :custom
  ;; Clisp makes SLY crash ?!
  (inferior-lisp-program "sbcl")
  (sly-net-coding-system 'utf-8-unix)
  (sly-complete-symbol-function 'sly-flex-completions)
  :config
  (setq common-lisp-hyperspec-root (concat "file://" (expand-file-name "~/Documents/Other/HyperSpec/")))

  (define-key sly-prefix-map (kbd "C-v") sly-selector-map)

  (defun lps/sly-company-setup ()
    (setq-local company-prescient-sort-length-enable nil)
    (setq-local company-backends '(company-capf)))

  (defun lps/sly-start-repl ()
    (unless (sly-connected-p)
      (save-excursion (sly))))

  (add-hook 'sly-mode-hook #'lps/sly-start-repl)
  (add-hook 'sly-mode-hook #'lps/sly-company-setup)

  (add-hook 'sly-minibuffer-setup-hook #'paredit-mode)

  ;; Don't use Ido, just use our default
  (defalias 'sly-completing-read completing-read-function)

  ;; View HyperSpec within Emacs using EWW
  (setq browse-url-handlers
    '(("hyperspec" . eww-browse-url)
      ("." . browse-url-default-browser))))

(use-package sly-mrepl
  :ensure nil
  :after sly
  :bind
  (:map sly-mrepl-mode-map
        ("C-c C-n" . sly-mrepl-next-prompt)
        ("C-c C-p" . sly-mrepl-previous-prompt)
        ("C-c C-k" . sly-quit-lisp)
        ("<C-return>" . end-of-buffer))
  (:map sly-selector-map
        ("C-v" . lps/sly-mrepl-other-window))
  :config
  (defun lps/sly-mrepl-other-window ()
    (interactive)
    (sly-mrepl #'pop-to-buffer))

  (add-hook 'sly-mrepl-mode-hook #'lps/sly-company-setup))

(use-package sly-stickers
  :ensure nil
  :after sly
  :bind
  (:map sly-stickers-mode-map
        ("C-c C-s C-t" . sly-stickers-toggle-break-on-stickers)))

(use-package sly-quicklisp
  :after sly
  :hook (sly . sly-quicklisp-mode))

(use-package sly-macrostep
  :after sly)

(use-package sly-asdf
  :after sly)

(use-package cider
  :defer t)

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
  (emmet-mode . emmet-preview-mode)
  :custom
  (emmet-move-cursor-between-quotes t))

(use-package gdb-mi
  :ensure nil
  :defer t
  :hook (gdb-mode . gdb-many-windows))

(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-mode))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org
  :commands org-capture
  ;; :init
  ;; (global-set-key (kbd "C-c o") #'org-capture)
  :hook (org-mode . lps/org-mode-setup)
  :bind
  ("C-c o" . org-capture)
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
  (org-directory "~/Documents/OrgFiles/")
  (org-special-ctrl-a/e t) ;; Not enough with visual-line-mode, need to bind C-a/C-e too
  :config
  (defun lps/org-mode-setup ()
    (lps/org-font-setup)
    (org-indent-mode 1)
    (variable-pitch-mode 1)
    (visual-line-mode 1))

  (setq org-imenu-depth 4)

  (setq org-ellipsis " ▾")

;; Use the right font according to what is installed on the system

(let ((my-temp-org-font "Cantarell"))
  (if (member my-temp-org-font (font-family-list))
      (setq my-org-mode-font my-temp-org-font)
    (setq my-org-mode-font "Ubuntu Mono")))

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
    (set-face-attribute (car face) nil :font my-org-mode-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch :extend t)
  (set-face-attribute 'org-block-begin-line nil :slant 'italic :foreground "dark gray" :background "#1d1d2b" :inherit 'variable-pitch :height 1.0)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (latex . t)))

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
                                ("latex" . "src latex")))

          (unless
              (member (car key-template) bound-key-templates)
            (push key-template org-structure-template-alist))))))

;; Automatically tangles this emacs-config config file when we save it
(defun lps/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name (concat user-emacs-directory "emacs-config.org")))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'lps/org-babel-tangle-config)))

(defun lps/elisp-completion-in-user-init ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name (concat user-emacs-directory "emacs-config.org")))
    (setq-local completion-at-point-functions '(pcomplete-completions-at-point elisp-completion-at-point t))))

(add-hook 'org-mode-hook #'lps/elisp-completion-in-user-init)

(setq org-agenda-files (list (concat org-directory "agenda/")))
(setq org-log-into-drawer t)
(setq org-log-done 'time)
(setq org-agenda-start-with-log-mode t)

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
         (file+headline "everything.org"
                        "A trier")
         "%x%?\n%i"
         :empty-lines-after 1)

        ("rm" "Movie" checkitem
         (file+function "movies.org" lps/org-ask-location))

        ("rR" "Restaurant" checkitem
         (file+function "restaurants.org" lps/org-ask-location))))

(setq org-capture-bookmark nil))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-c ?" . lps/pdf-maybe-goto-index))
  :custom
  (pdf-links-read-link-convert-commands '("-font" "FreeMono"
                                          "-pointsize" "%P"
                                          "-undercolor" "%f"
                                          "-fill" "%b"
                                          "-draw" "text %X,%Y '%c'"))
  (pdf-links-convert-pointsize-scale 0.015) ;; Slightly bigger than default
  :config
  (pdf-tools-install :no-query)
  ;;(add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
  (add-hook 'pdf-view-mode-hook 'pdf-history-minor-mode)

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
      (isearch-backward))))

(use-package pdf-view-restore
  :custom
  (pdf-view-restore-filename (concat user-emacs-directory ".pdf-view-restore"))
  (use-file-base-name-flag nil)
  :hook (pdf-view-mode . pdf-view-restore-mode))

;; AUCTeX initialization
(use-package tex-site
  :ensure auctex) ;; Don't defer, buggy ?

(use-package tex
  :ensure auctex
  :defer t
  :bind
  (:map TeX-mode-map
        ("C-c '" . TeX-error-overview))
  :hook
  (LaTeX-mode . outline-minor-mode)
  (LaTeX-mode . lps/latex-fontification)
  (LaTeX-mode . lps/latex-add-environments)
  (LaTeX-mode . lps/latex-company-setup)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . TeX-fold-mode)

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
  (TeX-debug-bad-boxes t)

  :config
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
                      "^ *\\([0-9]+\\) +\\([-~/a-zA-Z0-9_.${}#%,:\\ ()]+\\)" nil t)
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

  ;; Add environment for auto. insertion with C-c C-e
  (defun lps/latex-add-environments ()
    ;;(LaTeX-add-environments '("tikzpicture" LaTeX-env-label)) ; Should be done by auctex's tikz.el file
    )

  ;; Better completion functions
  (defun lps/latex-company-setup () ;; TO FIX !
    (setq-local company-backends '((company-math-symbols-unicode company-math-symbols-latex company-latex-commands company-capf company-dabbrev company-ispell company-yasnippet)))))

(use-package bibtex
  :defer t
  :bind
  (:map bibtex-mode-map
        ("C-c C-?" . bibtex-print-help-message))
  :config
  ;; Use a modern BibTeX dialect
                                        ; (bibtex-set-dialect 'biblatex) ; Useful esp. in social sci.
  )

(use-package reftex
  :diminish
  :hook (LaTeX-mode . reftex-mode)
  :config
  ;; Plug into AUCTeX
  (setq reftex-plug-into-AUCTeX t
        ;; Provide basic RefTeX support for biblatex
        ;; (unless (assq 'biblatex reftex-cite-format-builtin)
        ;;   (add-to-list 'reftex-cite-format-builtin
        ;;                '(biblatex "The biblatex package"
        ;;                           ((?\C-m . "\\cite[]{%l}")
        ;;                            (?t . "\\textcite{%l}")
        ;;                            (?a . "\\autocite[]{%l}")
        ;;                            (?p . "\\parencite{%l}")
        ;;                            (?f . "\\footcite[][]{%l}")
        ;;                            (?F . "\\fullcite[]{%l}")
        ;;                            (?x . "[]{%l}")
        ;;                            (?X . "{%l}"))))
        ;;   (setq reftex-cite-format 'biblatex))
        ))

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
  :defer t
  :config
  ;; We just override this function, to use our own completion
  ;; system.
  ;; We don't want packages to mess up our config !
  (defun biblio--completing-read-function ()
    completing-read-function))

(use-package biblio
  :defer t
  :bind
  (:map bibtex-mode-map
        ("C-c ?" . biblio-lookup))
  :custom
  (biblio-arxiv-bibtex-header "article")
  (biblio-download-directory "~/Documents/Other/articles/"))

(use-package preview
  :ensure nil ;; Comes with AUCTeX
  :defer t
  :config
  (setq preview-auto-reveal t)
  (setq preview-auto-cache-preamble t)
  (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

;; eshell
(use-package eshell-did-you-mean
  :hook (eshell-mode . eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package eshell
  :ensure nil
  :defer t
  :custom
  (eshell-prefer-lisp-variables t)
  (eshell-prefer-lisp-functions t)
  (eshell-hist-ignoredups t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt t)
  (eshell-prompt-function #'lps/eshell-prompt-function)
  (eshell-prompt-regexp "^[^#$\n]* [#$] ")

  :hook (eshell-mode . (lambda ()
                         (bind-key "C-l" 'eshell/clear eshell-mode-map)))

  :bind
  (:map lps/system-tools-map
        ("e" . eshell))

  :config
  ;; From https://blog.liangzan.net/blog/2012/12/12/customizing-your-emacs-eshell-prompt/
  (defun lps/pwd-repl-home (pwd)
    (interactive)
    (let* ((home (expand-file-name (getenv "HOME")))
           (home-len (length home)))
      (if (and
           (>= (length pwd) home-len)
           (equal home (substring pwd 0 home-len)))
          (concat "~" (substring pwd home-len))
        pwd)))

  ;; See the possible colours: M-x list-colors-display
  (defun lps/curr-dir-git-branch-string (pwd)
    "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
    (interactive)
    (when (and (eshell-search-path "git")
               (locate-dominating-file pwd ".git"))
      (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
        (propertize (concat "["
                            (if (> (length git-output) 0)
                                (substring git-output 0 -1)
                              "(no branch)")
                            "]")
                    'face `(:foreground "green3")))))

  (defun lps/eshell-prompt-function ()
    (concat
     (propertize ((lambda (p-lst)
                    (if (> (length p-lst) 3)
                        (concat
                         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                                    (substring elm 0 1)))
                                    (butlast p-lst 3)
                                    "/")
                         "/"
                         (mapconcat (lambda (elm) elm)
                                    (last p-lst 3)
                                    "/"))
                      (mapconcat (lambda (elm) elm)
                                 p-lst
                                 "/")))
                  (split-string (lps/pwd-repl-home (eshell/pwd)) "/"))
                 'face `(:foreground "DeepSkyBlue1"))
     (or (lps/curr-dir-git-branch-string (eshell/pwd)))
     (propertize " # " 'face 'default))))

;; (use-package eshell-git-prompt
;;   :config (eshell-git-prompt-use-theme 'powerline)) ;; Visually buggy

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
      (setq fish-completion-fallback-on-bash-p t))))

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

(add-hook 'eshell-mode-hook 'lps/eshell-load-bash-aliases)

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
  :init
  (setq delete-by-moving-to-trash t)
  ;; Prevents dired from opening thousands of buffers
  :bind
  (:map dired-mode-map
        ("RET" . dired-find-alternate-file)
        ("^" . lps/dired-up-directory-same-buffer)
        ("F" . find-name-dired))
  :custom
  ;; Delete and copy directories recursively
  (dired-recursive-deletes 'top)
  (dired-recursive-copies 'always)
  (dired-auto-revert-buffer t)
  (dired-listing-switches "-alFh")

  :config
  (defun lps/dired-up-directory-same-buffer ()
    (interactive)
    (find-alternate-file "..")))

;; Make things prettier
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

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
  (setq smtpmail-smtp-user "lpaviets")
  (setq smtpmail-smtp-server "smtp.ens-lyon.fr")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-stream-type 'starttls))

(use-package mu4e
  :ensure nil
  :load-path "/usr/local/share/emacs/site-lisp/mu4e" ;; Might be needed.
  :commands mu4e
  :bind (("C-c e" . mu4e)
         :map mu4e-compose-mode-map
         ("C-c h" . lps/org-mime-htmlize-preserve-secure-and-attach)
         (:map mu4e-main-mode-map
               ("q" . lps/mu4e-kill-buffers)
               ("Q" . mu4e-quit)))
  :config
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
  (setq mu4e-view-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; ASCII-only time is over
  (setq mu4e-use-fancy-chars t)

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
            nil)))))

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
      (mu4e-update-mail-and-index t))))

(use-package mu4e-alert
  :after mu4e
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
  ;; Make sure that this hook is added AFTER lps/sign-or-encrypt-message
  ;; so that it is executed BEFORE it.
  ;; We want to htmlize, then sign/encrypt, not the other way around !
  (add-hook 'message-send-hook 'org-mime-confirm-when-no-multipart)
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

(use-package guess-language
  :hook (text-mode . guess-language-mode)
  :custom
  (guess-language-languages '(en fr))
  (guess-language-after-detection-functions '(guess-language-switch-flyspell-function)))

(use-package xkcd
  :defer t)

(use-package speed-type
  :defer t
  :custom (speed-type-default-lang 'French)) ; Todo: fix bad behaviour !

(use-package key-quiz
  :defer t)

(use-package elfeed
  :defer t
  :bind
  ("C-c f" . elfeed)
  :custom
  (elfeed-db-directory (concat user-emacs-directory ".elfeed"))
  (elfeed-search-title-max-width 110)
  :config
  (setq-default elfeed-search-filter "@1-week-ago +unread "))

(use-package elfeed-org
  :after elfeed
  :config
  (setq rmh-elfeed-org-files '("~/Documents/OrgFiles/elfeed.org"))
  (elfeed-org))
