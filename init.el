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

(add-to-list 'load-path "~/.emacs.d/extra-packages")
(require 'use-package)
(setq use-package-always-ensure t)

(use-package hydra)

;; Broken for the moment  

;; (use-package posframe)

  ;; ;; Manual load and config of Hydra Posframe
  ;; (load-file "~/.emacs.d/extra-packages/hydra-posframe.el")
  ;; (setq hydra-posframe-parameters
  ;; '((left-fringe . 4) (right-fringe . 4) (top-fringe . 4) (bottom-fringe . 4) (height . 18) (width . 105) (min-height . 17) (max-height . 30) (top . 25)))
  ;; (add-hook 'after-init-hook  'hydra-posframe-mode)

  ;Pretty Hydra
  (use-package pretty-hydra)

  ; Avoid unnecessary warnings
  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-fileicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)

  ;define an icon function with all-the-icons-faicon
  ;to use filecon, etc, define same function with icon set
   (defun with-faicon (icon str &rest height v-adjust)
      (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))
  ;filecon
   (defun with-fileicon (icon str &rest height v-adjust)
      (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

;; Whenever a region is activated, inserting a symbol will first delete the region
(delete-selection-mode 1)

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

;; Global line numbering mode, except in some major modes
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
		doc-view-mode-hook
		undo-tree-visualizer-hook
		pdf-view-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :diminish (undo-tree-mode))

;; Generic UI modes

(use-package beacon)
(use-package rainbow-mode)
(use-package fill-column-indicator)
(use-package visual-fill-column)

(use-package command-log-mode
;; :hook (<your-favourite-mode> . command-log-mode) ; Add here modes in which you want to run the command-log-mode
)

;; Themes
(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))

(use-package cycle-themes
;; :init
;; (setq cycle-themes-theme-list
;;        '(leuven monokai solarized-dark)) ; Your favourite themes list
)

;; First time used: run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; Dimmer. Dims buffers that do not have the focus
(use-package dimmer
  :config
  (dimmer-configure-which-key) ; To fix ! Doesn't work
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-company-box)
  (dimmer-configure-hydra) ; To fix for hydra-posframe
  (dimmer-mode))

;define a title function
(defvar appearance-title (with-faicon "desktop" "Appearance"))
; Other idea:
; (defvar appearance-title (with-faicon "toggle-on" "Toggles" 1 -0.05))

;generate hydra
(pretty-hydra-define hydra-appearance (:title appearance-title
                                       :quit-key "q"
                                       )
("Theme"
   (
;    ("o" olivetti-mode "Olivetti" :toggle t)
;    ("t" toggle-window-transparency "Transparency" :toggle t )
    ("c" cycle-themes "Cycle Themes" )
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
   (("j" visual-line-mode "Wrap Line Window"  :toggle t)
    ("m" visual-fill-column-mode "Wrap Line Column"  :toggle t)
;    ("a" adaptive-wrap-prefix-mode "Indent Wrapped Lines" :toggle t )
;   ("i" highlight-indent-guides-mode  "Show Indent Guides" :toggle t )
    ("g" fci-mode "Show Fill Column" :toggle t )
    ("<SPC>" nil "Quit" :color blue )
)
)
)
(global-set-key (kbd "C-c a") 'hydra-appearance/body)

;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1))

;; Ivy
(use-package ivy
  :diminish
  :init
  (defun my-ivy-alt-done-t ()
    (interactive)
    (ivy-alt-done t))

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
  (ivy-mode 1))

;; Adds things to Ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Counsel. Adds things to Ivy
(use-package counsel
  :init (counsel-mode)
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; Helpful. Extra documentation when calling for help
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(winner-mode 1)

;;* Helpers
(use-package windmove)

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
(kbd "C-c w") ; w for window
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
  :hook (org-mode . my-org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

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

(use-package multiple-cursors) ; TODO: binds

(global-set-key
 (kbd "C-c m")
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
(kbd "C-c r") ; r as rectangle
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
  :diminish projectile
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  ;; (when (file-directory-p "path/to/project/dir")
  ;; (setq projectile-project-search-path '("path/to/project/dir")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer
  )

;; rainbow-delimiters. Hightlights with the same colour matching parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens)

;;YASnippet
(use-package yasnippet
  :diminish
  :hook (lsp-mode . yas-minor-mode))

;; Company. Auto-completion package
(use-package company
  :init (company-mode)
  :hook (prog-mode . company-mode)
  :diminish
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection)
	      ("C-n" . company-select-next)
	      ("C-p" . company-select-previous)
	      ("M-n" . nil)
	      ("M-p" . nil))

  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1)
  (company-selection-wrap-around t))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :diminish)

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :diminish
  :custom (company-quickhelp-delay 1))

;; Auto-complete
(use-package auto-complete
  :config
  (setq ac-use-quick-help t)
  (setq-default ac-sources '(ac-source-yasnippet
			   ac-source-words-in-same-mode-buffers
			   ac-source-dictionary)) ; see auto-complete doc for other sources
  :diminish
)

;; LSP mode. Useful IDE-like features
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-diagnostics-provider :flycheck) ;:none if none wanted
  (setq lsp-prefer-flymake nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable nil)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-enable-symbol-highlighting nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

;; Flycheck
(use-package flycheck
  :init
  (setq flycheck-relevant-error-other-file-show nil)
  (setq flycheck-indication-mode nil)
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
  :config
  (semantic-mode t)
  (global-semanticdb-minor-mode t)
  (global-semantic-idle-scheduler-mode t))

;; Python
;; Change to try Elpy ?

;; Before using LPS, make sure that the server has been installed !
;; pip install --user python-language-server[all]
;; Should be able to use the pyls command

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (setq python-shell-interpreter "python3")
  (setq tab-width 4)
  (setq python-indent-offset 4))

;; Tuareg (for OCaml and ML like languages)
(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t))

;; C/C++
;; See https://github.com/MaskRay/ccls/wiki/lsp-mode
(use-package ccls
  :init
  (setq ccls-executable (executable-find "ccls"))
  :hook ((c-mode c++-mode objc-mode) .
         lsp))

(use-package highlight-defined
:hook (emacs-lisp-mode . highlight-defined-mode))

(use-package elmacro
:init (elmacro-mode t))

;; Might require extra libs to work, see https://github.com/politza/pdf-tools

(use-package pdf-tools
    :config
    (pdf-tools-install)
    (setq TeX-view-program-selection '((output-pdf "pdf-tools")))
    (setq TeX-view-program-list '(("pdf-tools" "TeX-pdf-tools-sync-view")))
    (setq TeX-source-correlate-start-server t))

(use-package auctex
:defer t)

;; Adding support for LaTeX auto-complete
(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
  (require 'ac-math)
  (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

  (setq ac-sources
	(append '(ac-source-math-unicode
		  ac-source-math-latex
		  ac-source-latex-commands)
		ac-sources))
  (auto-complete-mode))

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)

(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)

;; eshell

(use-package eshell-did-you-mean
:init (eshell-did-you-mean-setup))

(use-package eshell-syntax-highlighting
:hook (eshell-mode . eshell-syntax-highlighting-mode))
