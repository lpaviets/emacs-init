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
    (ccls spinner python-mode org-bullets smartparens counsel-projectile projectile lsp-ivy lsp-treemacs lsp-ui lsp-mode helpful counsel ivy-rich rainbow-delimiters doom-modeline flycheck company-box jedi yasnippet-snippets yasnippet company function-args ggtags magit ac-math which-key pdf-tools auctex gnu-elpa-keyring-update sage-shell-mode doom-themes auto-complete ##))))
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
		pdf-view-mode-hook
		treemacs-mode-hook))
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

;; First time used: run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))


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

;; Autocompletion: auto-complete, yasnippet and company

;;YASnippet
(use-package yasnippet
  :diminish
  :hook (lsp-mode . yas-minor-mode))

;; Auto-complete
(use-package auto-complete
  :config
  (setq ac-use-quick-help t)
  (setq-default ac-sources '(ac-source-yasnippet
			   ac-source-words-in-same-mode-buffers
			   ac-source-dictionary)) ; see auto-complete doc for other sources
  :diminish (auto-complete-mode))

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
  :diminish
  ;; :hook (python-mode . flycheck-mode)
  ) ; Temporary to avoid noise ...

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

(use-package smartparens)

;; org-mode

;; Modifies fonts so that it looks more like a document - not fixed-pitch, etc

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

(setq org-confirm-babel-evaluate nil) ; Take care if executing someone
					; else code

;; (require 'org-tempo) if org-version >= 9.2
;; (add-to-list 'org-structure-template-alist '("sh"  "src sh"))
;; (add-to-list 'org-structure-template-alist '("el"  "src emacs-lisp"))
;; (add-to-list 'org-structure-template-alist '("py"  "src python"))

;; Tuareg (for OCaml and ML like languages)
(use-package tuareg
  :config
  (setq tuareg-indent-align-with-first-arg t)
  (setq tuareg-match-patterns-aligned t))

;; __________________________________________
;; Language specific configuration

;; Python
;; Change to try Elpy ?

;; Before using LPS, make sure that the server has been installed !
;; pip install --user "python-language-server[all]"
;; Should be able to use the pyls command

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (setq python-shell-interpreter "python3")
  (setq tab-width 4)
  (setq python-indent-offset 4))


;; C/C++
;; See https://github.com/MaskRay/ccls/wiki/lsp-mode
(use-package ccls
  :init
  (setq ccls-executable (executable-find "ccls"))
  :hook ((c-mode c++-mode objc-mode) .
         lsp))


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
