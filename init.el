(setq gc-cons-threshold 100000000) ; 1e8 = 100 MB (default: 800kB)

(defun my-display-startup-time ()
  (message "Emacs started in %s seconds"
    (format "%.2f"
      (float-time
        (time-subtract after-init-time before-init-time)))))

(defun my-display-garbage-collection ()
  (message "Emacs performed %d garbage collection"
    gcs-done))

(add-hook 'emacs-startup-hook #'my-display-startup-time)
(add-hook 'emacs-startup-hook #'my-display-garbage-collection)

; After startup, we restore gc-cons-threshold to a more reasonable value
(setq gc-cons-threshold 2000000) ; 2e6 = 2 MB

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
;; Comment this line if you don't want to automatically install packages
(setq use-package-always-ensure t)

(use-package restart-emacs
  :commands (restart-emacs restart-emacs-start-new-emacs))

;; Whenever a region is activated, inserting a symbol will first delete the region
; (delete-selection-mode 1)

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

;; Maximize the frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
                shell-mode-hook
                term-mode-hook
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
                dired-mode-hook
                ))

(add-hook mode #'my-disable-line-numbers))

;; Automatically reload a file if it has been modified
(global-auto-revert-mode t)

;; Tab behaviour and whitespaces
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)

;; Type "y" instead of "yes" for confirmation
(defalias 'yes-or-no-p 'y-or-n-p)

;; Always highlight matching parenthesis
(show-paren-mode t)

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :diminish (undo-tree-mode))

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

;; Generic UI modes

(use-package beacon
  :init (beacon-mode))
(use-package rainbow-mode
  :defer t)
(use-package fill-column-indicator
  :defer t)
(use-package visual-fill-column
  :defer t)

(use-package command-log-mode
;; :hook (<your-favourite-mode> . command-log-mode) ; Add here modes in which you want to run the command-log-mode
  :commands command-log-mode
)

;; Themes
(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))

(use-package cycle-themes
  :defer t
;; :config
;; (setq cycle-themes-theme-list
;;        '(leuven monokai solarized-dark)) ; Your favourite themes list
)

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
  ;filecon
  (defun with-fileicon (icon str &rest height v-adjust)
     (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
)

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(with-eval-after-load 'hydra
;define a title function
  (defvar appearance-title (with-faicon "desktop" "Appearance"))

  ; Other idea:
  ; (defvar appearance-title (with-faicon "toggle-on" "Toggles" 1 -0.05))

  ;generate hydra
  
  (pretty-hydra-define hydra-appearance (:title appearance-title
                                         :quit-key "q"
					 ;:pre (hydra-posframe-mode t)
					 ;:post (hydra-posframe-mode 0) ; dirty hack
					 )
  ("Theme"
    (
;     ("o" olivetti-mode "Olivetti" :toggle t)
;     ("t" toggle-window-transparency "Transparency" :toggle t )
      ("c" cycle-themes-mode "Cycle Themes" )
      ("+" text-scale-increase "Zoom In")
      ("-" text-scale-decrease "Zoom Out")
      ("x" toggle-frame-maximized "Maximize Frame" :toggle t )
      ("X" toggle-frame-fullscreen "Fullscreen Frame" :toggle t)
    )
    "Highlighting"
    (
      ("d" rainbow-delimiters-mode "Rainbow Delimiters" :toggle t )
      ("r" rainbow-mode "Show Hex Colours" :toggle t )
      ;    ("n" highlight-numbers-mode "Highlight Code Numbers" :toggle t )
      ("l" display-line-numbers-mode "Show Line Numbers" :toggle t )
      ("_" global-hl-line-mode "Highlight Current Line" :toggle t )
      ;    ("I" rainbow-identifiers-mode "Rainbow Identifiers" :toggle t )
      ("b" beacon-mode "Show Cursor Trailer" :toggle t )
      ("w" whitespace-mode "whitespace" :toggle t)
    )
    "Miscellaneous"
    (
      ("j" visual-line-mode "Wrap Line Window"  :toggle t)
      ("m" visual-fill-column-mode "Wrap Line Column"  :toggle t)
      ;    ("a" adaptive-wrap-prefix-mode "Indent Wrapped Lines" :toggle t )
      ;   ("i" highlight-indent-guides-mode  "Show Indent Guides" :toggle t )
      ("g" fci-mode "Show Fill Column" :toggle t )
      ("<SPC>" nil "Quit" :color blue )
  ))))

  (global-set-key (kbd "C-c h a") 'hydra-appearance/body)

;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1))

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
  (defun my-ivy-alt-done-t ()
    (interactive)
    (ivy-alt-done t))

  (ivy-mode 1))

;; Adds things to Ivy
(use-package ivy-rich
  :hook (ivy . ivy-rich-mode))

;; Counsel. Adds things to Ivy
(use-package counsel
  :config (counsel-mode)
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :after counsel
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;;Buffer management
(setq display-buffer-base-action
  '((display-buffer-reuse-window)
    (display-buffer-reuse-mode-window)
    (display-buffer-same-window)
    (display-buffer-in-previous-window)))

;; Can even have further control with
;; display-buffer-alist, or using extra-parameters

(winner-mode 1)

;;* Helpers
(use-package windmove
  :defer t)

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
_b_move left      _B_left        _V_split-vert-move      _o_del-other      _f_new-frame
_n_move down      _N_down        _H_split-horiz-move     _da_ace-del       _u_winner-undo
_p_move up        _P_up          _v_split-vert           _dw_del-window    _r_winner-redo
_f_move right     _F_right       _h_split-horiz          _df_del-frame
_q_uit
"
  ; Move the focus around
  ("b" windmove-left)
  ("n" windmove-down)
  ("p" windmove-up)
  ("f" windmove-right)
  ; Changes the size of the current window
  ("B" hydra-move-splitter-left)
  ("N" hydra-move-splitter-down)
  ("P" hydra-move-splitter-up)
  ("F" hydra-move-splitter-right)
  ; Split and move (or not)
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
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ; Delete windows
  ("o" delete-other-windows :exit t)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ; Other stuff
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump)))

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
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
  

(use-package org
  :config
  (setq org-ellipsis " ▾")
  
  ;; Coding in blocks
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t)

  :hook (org-mode . my-org-mode-setup)
)

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

;; Automatically tangles this emacs-config config file when we save it
(defun my-org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/emacs-config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my-org-babel-tangle-config)))

(use-package multiple-cursors
  :bind
  (("C-c o <SPC>" . mc/vertical-align-with-space)
   ("C-c o a"     . mc/vertical-align)
   ("C-c o e"     . mc/mark-more-like-this-extended)
   ("C-c o h"     . mc/mark-all-like-this-dwim)
   ("C-c o l"     . mc/edit-lines)
   ("C-c o n"     . mc/mark-next-like-this)
   ("C-c o p"     . mc/mark-previous-like-this)
   ("C-c o C-a"   . mc/edit-beginnings-of-lines)
   ("C-c o C-e"   . mc/edit-ends-of-lines)
   ("C-c o C-s"   . mc/mark-all-in-region)
   ("C-c e"       . mc/edit-lines)
   ("C-c o C-<"   . mc/mark-previous-like-this)
   ("C-c o C->"   . mc/mark-next-like-this)
   ("C-c o C-,"   . mc/mark-all-like-this)))

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
  :bind ("C-x g" . magit-status)
  )

;; rainbow-delimiters. Hightlights with the same colour matching parenthesis
  (use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

  ;; Smartparens is currently bugged
  (use-package smartparens
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
          eval-expression-minibuffer-setup) . enable-paredit-mode))

  (defun paredit-or-smartparens ()
  "Enable paredit or smartparens depending on the major mode"
  (if (member major-mode '(emacs-lisp-mode
                         lisp-mode
                         lisp-interaction-mode))
    (paredit-mode)
  (smartparens-mode)))
;; Bug with strict-mode in cc-mode (Java, C/C++ ...)
;; Bindings are overriden by the cc-mode one, so sp-strict-mode does not
;; work properly (e.g. <DEL> is not bound to sp-backward-delete-char)

  (add-hook 'prog-mode-hook #'paredit-or-smartparens)

;;YASnippet
(use-package yasnippet
  :disabled t
  :diminish
  :init (yas-global-mode t))

;; Company. Auto-completion package
(use-package company
  :diminish

  :init (global-company-mode t)

  :bind (
     :map company-active-map
        ("<tab>" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-n" . nil)
        ("M-p" . nil)
     :map company-search-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))

  :custom
     (company-minimum-prefix-length 3)
     (company-idle-delay 0.1)
     (company-echo-delay 0.1)
     (company-selection-wrap-around t)

  :config
    (setq company-tooltip-align-annotations t
          company-tooltip-flip-when-above t
          company-show-numbers t))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :diminish)

(use-package company-quickhelp
  :after company
  :hook (company-mode . company-quickhelp-mode)
  :diminish
  :custom (company-quickhelp-delay 0.2))

(use-package company-math
  :after company
  :config
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex))

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  (lsp-enable-which-key-integration t)
  ;(setq lsp-signature-render-documentation nil)
  ;(setq lsp-signature-auto-activate nil)
  ;(setq lsp-enable-symbol-highlighting nil)
  (setq lsp-prefer-flymake nil)
  (setq lsp-diagnostics-provider :flycheck) ;:none if none wanted
  :hook
  ((python-mode c-mode c++-mode) . lsp)
)

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
  :after lsp-mode)

(use-package lsp-ivy
  :after (lsp-mode ivy))

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

(use-package python-mode
  :defer t
  :custom
  ;(setq python-shell-interpreter "python3")
  (setq tab-width 4)
  (setq python-indent-offset 4))

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
:init (elmacro-mode t))

;; Make sure that sbcl is available on PATH
(use-package sly
  :hook (lisp-mode . sly-editing-mode)
  :custom (inferior-lisp-program "sbcl") ; Clisp makes SLY crash
  :config
  (add-hook 'sly-mode-hook
            (lambda ()
               (unless (sly-connected-p)
                 (save-excursion (sly))))))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query)
  (add-hook 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode))

(use-package tex
  :ensure auctex
  :config
  (setq TeX-parse-self t                ; Parse documents to provide completion
                                        ; for packages, etc.
        TeX-auto-save t                 ; Automatically save style information
        TeX-electric-sub-and-superscript t ; Automatically insert braces after
                                        ; sub- and superscripts in math mode
        ;; Don't insert magic quotes right away.
        TeX-quote-after-quote t

        TeX-master nil

        ;; Don't ask for confirmation when cleaning
        TeX-clean-confirm nil

        ;; Provide forward and inverse search with SyncTeX
        TeX-source-correlate-mode t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
  
        ;; Produce a PDF by default
        TeX-PDF-mode t)

    (unless (assoc "PDF tools" TeX-view-program-list-builtin)
      (push '("PDF tools" TeX-pdf-tools-sync-view) TeX-view-program-list))

    (setq TeX-view-program-selection '((output-pdf "PDF tools")))
    
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

    ;; Insert math symbols quickly
    (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode))

;; eshell

(setq eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t)

(use-package eshell-did-you-mean
  :commands eshell
  :config (eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))
