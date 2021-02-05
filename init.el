;; An interesting tutorial:
;; http://tuhdo.github.io/emacs-tutor.html
;; __________________________________________

;; Cool packages that might be installed later:
;; peep-dired ; preview files in dired-mode
;; Many more available on github: awesome-emacs
;; __________________________________________


;; Other people configuration files !
;; https://github.com/grettke/every-emacs-initialization-file
;; Beware ! Some of them are 5k+ lines longs
;; __________________________________________


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

(require 'use-package)
(setq use-package-always-ensure t)


;; __________________________________________
;; A set of custom variables, ranging from themes to packages and other options ...

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck company-box jedi yasnippet-snippets yasnippet company function-args ggtags magit ac-math which-key pdf-tools auctex gnu-elpa-keyring-update sage-shell-mode doom-themes  auto-complete ##)))
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;__________________________________________
;; Global "comfort" modes and shortcuts
;; Modes:
(delete-selection-mode 1)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; __________________________________________
;; Packages options and management

;; undo-tree

(use-package undo-tree
  :config
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode)
  :diminish (undo-tree-mode))

;; auto-mode
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))


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
		pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-partial-or-done)
	 ("C-l" . ivy-alt-done)
	 :map ivy-switch-buffer-map
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Dimmer. Dims buffers that do not have the focus
(use-package dimmer
  :config
  (dimmer-configure-which-key) ; To fix ! Doesn't work
  (dimmer-configure-magit)
  (dimmer-configure-org)
  (dimmer-configure-company-box)
  (dimmer-mode))


;; Themes
(use-package doom-themes
  :init (load-theme 'doom-Iosvkem t))


;; rainbow-delimiters. Hightlights with the same colour matching parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;; which-key. Shows all the available key sequences after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 1))

;; Adds things to Ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))


;; Counsel. Adds things to Ivy
(use-package counsel
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

;; Autocompletion: auto-complete, yasnippet and company

;; ;;YASnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; ;; Auto-complete
;; (use-package auto-complete
;;   :config
;;   (setq ac-use-quick-help t)
;;   (setq-default ac-sources '(ac-source-yasnippet
;; 			   ac-source-words-in-same-mode-buffers
;; 			   ac-source-dictionary)) ; see auto-complete doc for other sources
;;   :diminish (auto-complete-mode))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package company
  :init (global-company-mode t)
  :diminish
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :diminish)

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :diminish
  :custom (company-quickhelp-delay 1))


;; ;; Company
;; (use-package company
;;   :init (global-company-mode t)
;;   :config
;;   (setq company-idle-delay 0.1)
;;   (setq company-minimum-prefix-length 3)
;;   (setq company-quickhelp-delay 1)
;;   :diminish
;;   )

;; Use pdf-tools to open PDF files
(pdf-tools-install)

;; ;; Semantic
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


;; Flycheck
(use-package flycheck
  :init
  (setq flycheck-relevant-error-other-file-show nil)
  (setq flycheck-indication-mode nil)
  :config
  :hook (python-mode . flycheck-mode)) ; Temporary to avoid noise ...

;; Magit
(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; uncomment previous line to have magit open itself within the same buffer
  ;; instead of in another buffer  
  )


(use-package projectile
  :diminish projectile-mode
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

(use-package smartparens
  :init
  (smartparens-mode t)
  :diminish
  )

;; org-mode

(use-package org
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Tuareg (for OCaml and ML like languages)
(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t))

;; __________________________________________
;; Language specific configuration

;; Python
;; Change to try Elpy

;; (defun my-python-hooks()
;;     (interactive)
;;     (setq tab-width 4)
;;     (setq python-indent-offset 4)

;;     (add-to-list
;;         'imenu-generic-expression
;;         '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))

;;     ;; pythom mode keybindings
;;     (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
;;     (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
;;     (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
;;     (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
;;     ;; end python mode keybindings

;;     (add-to-list 'company-backends 'company-jedi)
;;     (jedi-mode)
;;     (setq jedi:complete-on-dot t)
;;     (setq jedi:tooltip-method '(pos-tip popup))
;;     (setq jedi:get-in-function-call-delay 0))

;; (add-hook 'python-mode-hook 'my-python-hooks)
;; ;; End Python mode

;;LaTeX

;; Adding support for LaTeX auto-complete
(defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
  (require 'ac-math)
  (add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

  (setq ac-sources
	(append '(ac-source-math-unicode
		  ac-source-math-latex
		  ac-source-latex-commands)
		ac-sources))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  
  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
	    #'TeX-revert-document-buffer)
  
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method '((dvi . source-specials) (pdf . synctex)))
  )

(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
;; End LaTeX mode
