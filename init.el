(setq gc-cons-threshold 100000000) ; 1e8 = 100 MB (default: 800kB)

(defun my-display-startup-time ()
  (message "Emacs started in %s seconds"
           (format "%.2f"
                   (float-time
                    (time-subtract after-init-time before-init-time)))))

(defun my-display-garbage-collection ()
  (message "Emacs performed %d garbage collection"
           gcs-done))

(defun my-restore-gc-cons ()
  ;; After startup, we restore gc-cons-threshold to a more reasonable value
  (setq gc-cons-threshold 10000000)) ; 1e7 = 10 MB

(add-hook 'emacs-startup-hook #'my-display-startup-time)
(add-hook 'emacs-startup-hook #'my-display-garbage-collection)
(add-hook 'emacs-startup-hook #'my-restore-gc-cons)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Other packages
(add-to-list 'load-path "~/.emacs.d/extra-packages")

(require 'use-package)
;; Comment this line if you don't want to automatically install
;; all the packages that you are missing
(setq use-package-always-ensure t)
;; Uncomment the folllowing line to have a detailed startup log
;; (setq use-package-verbose t)

(use-package restart-emacs
  :commands (restart-emacs restart-emacs-start-new-emacs))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

;; Disable the message on top of the Scratch buffer
(setq initial-scratch-message nil)

;; Maximize the Emacs frame at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'emacs-startup-hook (lambda () (eshell) (previous-buffer)))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Global line/column numbering mode
;; Modes in which we might want to disable it:

(column-number-mode t)
(global-display-line-numbers-mode t)

(defun my-disable-line-numbers ()
  (display-line-numbers-mode 0))

(dolist (mode '(org-mode-hook
               ; Term & Shells
                eshell-mode-hook
                comint-mode-hook
                ; PDF viewers
                pdf-view-mode-hook
                doc-view-mode-hook
                ; Help modes
                helpful-mode-hook
                help-mode-hook
                apropos-mode-hook
                ; Extra modes
                undo-tree-visualizer-mode-hook
                treemacs-mode-hook
                dired-mode-hook))

(add-hook mode #'my-disable-line-numbers))

(global-visual-line-mode 1)

;; Themes
(use-package solarized-theme
  :commands load-theme)
(use-package kaolin-themes
  :commands load-theme)
(use-package modus-themes
  :commands load-theme)

(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))

;; Use this to store your favourite themes
;; Save your usual, default theme in first position
;; so that you can easily switch back to it with
;; (load-theme (car lps/rotate-themes-list) t)
(setq lps/rotate-themes-list
      '(doom-Iosvkem kaolin-ocean kaolin-aurora doom-monokai-pro doom-palenight tsdh-dark solarized-dark modus-vivendi))

;; Try to save the current theme
;; Be careful ! Some visual changes are NOT stored in
;; a theme, and will not be retrieved by the restoring
;; functions. For example, any font configuration might
;; be "lost" for this session
(setq lps/initial-enabled-themes custom-enabled-themes)

(setq lps/rotate-theme-index 0)

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
  (my-org-mode-setup))

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
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(with-eval-after-load 'hydra
  ;; define a title function
  (defvar appearance-title (with-faicon "desktop" "Appearance"))

  ;; Other idea:
  ;; (defvar appearance-title (with-faicon "toggle-on" "Toggles" 1 -0.05))

  ;; generate hydra

  (pretty-hydra-define hydra-appearance (:title appearance-title
                                         :quit-key "q"
                                        ;:pre (hydra-posframe-mode t)
                                        ;:post (hydra-posframe-mode 0) ; dirty hack
                                                )
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

(global-set-key (kbd "C-c h a") 'hydra-appearance/body)

;; Generic UI modes

(use-package beacon
  :init (beacon-mode))
(use-package rainbow-mode
  :defer t)
(use-package fill-column-indicator
  :defer t)
(use-package visual-fill-column
  :defer t)
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Tab behaviour and whitespaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(use-package hungry-delete
  :ensure t
  :defer t
  :init
  (global-hungry-delete-mode 1))

(use-package hydra
  :defer t)

(use-package posframe
  :defer t)

;; Manual load and config of Hydra Posframe
;; To fix: find a way to override parameters ...
;; (load-file "~/.emacs.d/extra-packages/hydra-posframe.el")
;; (setq hydra-posframe-border-width 5)

;Pretty Hydra
(use-package pretty-hydra
  :defer t
  :after hydra)

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Don't disable any command
;; BE CAREFUL
;; If you are a new user, you might to comment out this line
(setq disabled-command-function nil)

(global-unset-key (kbd "C-z"))

(use-package command-log-mode
  :defer t
)

;; Type "y" instead of "yes RET" for confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish
  :custom (which-key-idle-delay 1))

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

;; Ivy
(use-package ivy
  :diminish 
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-partial-or-done)
         ("C-l" . my-ivy-alt-done-t) ; Small hack
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  ;; Todo: check if ivy-use-selectable-prompt does the trick
  (defun my-ivy-alt-done-t ()
    (interactive)
    (ivy-alt-done t))

  :init (ivy-mode 1))

;; Adds things to Ivy
(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode 1))

;; Counsel. Adds things to Ivy
(use-package counsel
  :init (counsel-mode 1)
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package amx
   :init (setq amx-history-length 10))

;; Automatically reload a file if it has been modified
(global-auto-revert-mode t)

;;Buffer management
(setq display-buffer-base-action
      '((display-buffer-reuse-window)
        (display-buffer-reuse-mode-window)
        (display-buffer-same-window)
        (display-buffer-in-previous-window)))

;; Can even have further control with
;; display-buffer-alist, or using extra-parameters

(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)

(use-package all-the-icons-ibuffer
  :after ibuffer counsel
  :init (all-the-icons-ibuffer-mode 1))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; From Magnars, from emacsrocks.com
(defun rename-current-buffer-file ()
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

(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*Buffer List*"
                                      "*Ibuffer*")))

;;* Helpers
(use-package windmove
  :config (windmove-default-keybindings))

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
    (enlarge-window arg)))

(global-set-key
 (kbd "C-c h w") ; w for window
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
   ("c" new-frame :exit t)
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

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :after counsel
  :custom
  (counsel-describe-symbol-function   #'helpful-symbol) 
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-symbol]   . counsel-describe-symbol)
  ([remap describe-key]      . helpful-key)
  ("C-h u"                   . helpful-at-point)) ;; Help "<u>nder" cursor

(use-package multiple-cursors
  :bind
  (("C-c m SPC>" . mc/vertical-align-with-space)
   ("C-c m a"     . mc/vertical-align)
   ("C-c m m"     . mc/mark-more-like-this-extended)
   ("C-c m h"     . mc/mark-all-like-this-dwim)
   ("C-c m l"     . mc/edit-lines)
   ("C-c m n"     . mc/mark-next-like-this)
   ("C-c m p"     . mc/mark-previous-like-this)
   ("C-c m C-,"   . mc/mark-all-like-this)
   ("C-c m C-a"   . mc/edit-beginnings-of-lines)
   ("C-c m C-e"   . mc/edit-ends-of-lines)
   ("C-c m r"     . mc/mark-all-in-region)))

(global-set-key
 (kbd "C-c h m")
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

(global-set-key
 (kbd "C-c h r") ; r as rectangle
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

(defun lps/copy-line-at-point (arg)
   "Copy ARG lines in the kill ring, starting from the line at point and copying subsequent ones if ARG > 1"
   (interactive "p")
   (kill-ring-save (line-beginning-position)
                   (line-end-position arg)))

; Note that this keybinding overrides other functions
; By default, M-k is kill-sentence, which I never use
; I bound it this way to mirror the C-w/M-w symmetry 

;; Might want to find a more clever way to use personal
;; keybindings, such as defining a minor mode ...
(global-set-key (kbd "M-k") 'lps/copy-line-at-point)

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :diminish (undo-tree-mode))

(use-package projectile
  :diminish
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;; (when (file-directory-p "path/to/project/dir")
  ;; (setq projectile-project-search-path '("path/to/project/dir")))
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode))

(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer
  :bind ("C-x g" . magit-status))

(use-package git-timemachine
  :defer t)

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
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :hook ((mrepl-mode
          eshell-mode
          ielm-mode
          eval-expression-minibuffer-setup
          emacs-lisp-mode
          lisp-mode
          lisp-interaction-mode) . paredit-mode))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))

;;YASnippet
(use-package yasnippet
  :diminish
  :init
  (setq yas-verbosity 1)
  :hook (prog-mode . yas-minor-mode)
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("<tab>" . nil)
              ("<C-tab>" . yas-expand)))

(use-package yasnippet-snippets
  :after yasnippet)

;; Company. Auto-completion package
(use-package company
  :diminish

  :init (global-company-mode t)

  :bind (:map company-active-map
        ("<tab>" . company-complete)
        ("TAB" . company-complete)
        ("RET" . nil)
        ("<return>" . nil)
        ("C-l" . company-complete-selection))

  :custom
     (company-minimum-prefix-length 1)
     (company-idle-delay 0.0)
     (company-selection-wrap-around t)
     (company-show-numbers t)
     (company-tooltip-align-annotations t)
     (company-tooltip-flip-when-above t))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :diminish)

(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :diminish
  :custom (company-quickhelp-delay 0.2))

;; (add-to-list 'company-backends 'company-yasnippet)

(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

(use-package company-shell
  :defer t
  :config
(defun my-company-shell-modes ()
  (setq-local company-backends '((company-capf company-shell company-shell-env company-files company-dabbrev)))

  (add-hook 'eshell-mode-hook #'my-company-shell-modes)))

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-flymake nil)
  (setq lsp-diagnostics-provider :flycheck) ;:none if none wanted
  (setq read-process-output-max (* 2 1024 1024)) ;; 2mb
  (setq lsp-enable-on-type-formatting nil)
  :hook
  ((python-mode c-mode c++-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1)
  (lsp-ui-sideline-show-code-actions nil)
  ;(lsp-ui-sideline-enable nil)
 )

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
  :diminish
  ;; :hook (python-mode . flycheck-mode)
  ) ; Temporary to avoid noise ...

;; Semantic
(use-package semantic
;; (require 'semantic/ia)
;; (require 'semantic/bovine/gcc)

;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)
  :defer t
  :config
  (semantic-mode t)
  (global-semanticdb-minor-mode t)
  (global-semantic-idle-scheduler-mode t))

;; Python
;; Before using LPS, make sure that the server has been installed !
;; pip install --user python-language-server[all]
;; Should be able to use the pyls command

(use-package python
  :ensure nil
  :defer t
  :custom
  (tab-width 4)
  (python-indent-offset 4)
  (python-shell-interpreter "python3"))

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

(use-package highlight-defined
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package elmacro
  :defer t)

(defun lps/eval-and-replace-last-sexp ()
  "Evaluate the last s-expression, and replace it with the result"
  (interactive)
  (let ((value (eval (preceding-sexp))))
      (kill-sexp -1)
      (insert (format "%S" value))))

(global-set-key (kbd "C-c C-e") 'lps/eval-and-replace-last-sexp)

;; Make sure that sbcl is available on PATH
(use-package sly
  :hook (lisp-mode . sly-editing-mode)
  :custom (inferior-lisp-program "sbcl") ; Clisp makes SLY crash
  :config
  (add-hook 'sly-mode-hook
            (lambda ()
               (unless (sly-connected-p)
                 (save-excursion (sly))))))

(use-package gdb-mi
  :ensure nil
  :defer t
  :hook (gdb-mode . gdb-many-windows))

(use-package antlr-mode
  :mode ("\\.g4\\'" . antlr-mode))

(let ((my-temp-org-font "Cantarell"))
    (if (member my-temp-org-font (font-family-list))
        (setq my-org-mode-font my-temp-org-font)
      (setq my-org-mode-font "Ubuntu Mono")))

(defun my-org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  ;; For non-headers: org-default

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font my-org-mode-font :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun my-org-mode-setup ()
  (my-org-font-setup)
  (org-indent-mode 1)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :config
  (setq org-ellipsis " ▾")

  ;; Coding in blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  :hook (org-mode . my-org-mode-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; (setq org-confirm-babel-evaluate nil) ; Take care if executing someone
                                         ; else code

(with-eval-after-load 'org
  (if (version<= "9.2" org-version)
      ;; This is needed as of Org 9.2
    (progn
      (require 'org-tempo)

      (let ((bound-key-templates
             (mapcar #'car org-structure-template-alist)))
        (dolist (key-template '(("sh" . "src shell")
                                ("el" . "src emacs-lisp")
                                ("py" . "src python")))

          (unless
              (member (car key-template) bound-key-templates)
            (push key-template org-structure-template-alist)))))))

;; Automatically tangles this emacs-config config file when we save it
(defun my-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs-config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my-org-babel-tangle-config)))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward))
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))

(use-package tex-site                   ; AUCTeX initialization
  :ensure auctex
  :defer t)

(use-package tex
  :ensure auctex
  :defer t
  :custom ;; Automatically insert closing brackets
  (LaTeX-electric-left-right-brace t)
  (TeX-parse-self t)                ; Parse documents to provide completion
  (TeX-auto-save t)                 ; Automatically save style information
  (TeX-electric-sub-and-superscript t)  ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
  ;; Don't insert magic quotes right away.
  (TeX-quote-after-quote t)
  ;; But do insert closing $ when inserting the first one
  (TeX-electric-math '("$" . "$"))

  ;; Don't ask for confirmation when cleaning
  (TeX-clean-confirm nil)

  (TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-start-server t)
  (TeX-view-program-selection '((output-pdf "PDF tools")))

  :config
  (setq TeX-master nil) ; Ask for the master file & don't assume anything

  (setq TeX-source-correlate-mode t ; SyncTeX forward and inverse search
        ;; Produce a PDF by default
        TeX-PDF-mode t)

  (unless (assoc "PDF tools" TeX-view-program-list-builtin)
    (push '("PDF tools" TeX-pdf-tools-sync-view) TeX-view-program-list))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; Insert math symbols quickly
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)

  ;; Add environment for auto. insertion with C-c C-e
  (add-hook 'LaTeX-mode-hook 'lps/latex-add-environments)
  (defun lps/latex-add-environments ()
    (LaTeX-add-environments '("tikzpicture" LaTeX-env-label))))

(use-package bibtex                     ; BibTeX editing
    :defer t
    :config
    ;; Use a modern BibTeX dialect
    ; (bibtex-set-dialect 'biblatex) ; Useful esp. in social sci.
)

(use-package reftex                     ; TeX/BibTeX cross-reference management
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

;; eshell

(setq eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t)

(use-package eshell-did-you-mean
  :commands eshell
  :config (eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

(use-package dired
  :ensure nil
  :defer t
  :config
  ;; Delete and copy directories recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  (setq dired-auto-revert-buffer t))

;; Make things prettier 
(use-package all-the-icons-dired
  :diminish
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-x
  :ensure nil
  :after dired)

(use-package xkcd
  :defer t)

(use-package speed-type
  :defer t
  :custom (speed-type-default-lang 'French)) ; Todo: fix bad behaviour !

(use-package key-quiz
  :defer t)
