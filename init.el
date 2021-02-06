;; An interesting tutorial:
;; http://tuhdo.github.io/emacs-tutor.html
;; __________________________________________

;; Cool packages that might be installed later:
;; which-key ; some interactive documentation when typing commands
;; hydra ; "merge" related comands to a family of short bindings with a common prefix
;; peep-dired ; preview files in dired-mode
;; ggtags ; work with GNU Global
;; ctags ; another smpler but less complete tag generating program
;; ergoemacs ; insane ergonomy changes (esp. keybindings)
;; Many more available on github: awesome-emacs
;; __________________________________________


;; Other people configuration files !
;; https://github.com/grettke/every-emacs-initialization-file
;; Beware ! Some of them are 5k+ lines longs
;; __________________________________________


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\ Your version of Emacs does not support SSL
connections, which is unsafe because it allows man-in-the-middle
attacks.  There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it
again.")))


;; __________________________________________
;; A set of custom variables, ranging from themes to packages and other options ...

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tsdh-dark)))
 '(custom-safe-themes
   (quote
    ("fd944f09d4d0c4d4a3c82bd7b3360f17e3ada8adf29f28199d09308ba01cc092" default)))
 '(fci-rule-color "#5B6268")
 '(jdee-db-active-breakpoint-face-colors (cons "#2b2a27" "#ff5d38"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#2b2a27" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#2b2a27" "#3f444a"))
 '(objed-cursor-color "#ff5d38")
 '(package-selected-packages
   (quote
    (flycheck company-box jedi yasnippet-snippets yasnippet company-c-headers company-irony company function-args ggtags magit ac-math which-key pdf-tools auctex gnu-elpa-keyring-update sage-shell-mode doom-themes realgud realgud-ipdb auto-complete ##)))
 '(vc-annotate-background "#2b2a27")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#a4c551")
    (cons 60 "#b0cc3d")
    (cons 80 "#bcd42a")
    (cons 100 "#c1a623")
    (cons 120 "#c5781c")
    (cons 140 "#cb4b16")
    (cons 160 "#c95a58")
    (cons 180 "#c7699a")
    (cons 200 "#c678dd")
    (cons 220 "#d96fa6")
    (cons 240 "#ec666f")
    (cons 260 "#ff5d38")
    (cons 280 "#cf563c")
    (cons 300 "#9f5041")
    (cons 320 "#6f4a45")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'doom-Iosvkem)



;;__________________________________________
;; Global "comfort" modes and shortcuts
;; Modes:
(delete-selection-mode 1)
(setq inhibit-startup-screen t)
;; Shortcuts

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq undo-tree-visualizer-timestamps t)
(global-undo-tree-mode)

;; __________________________________________
;; Packages options and management

;; auto-mode
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;; Global linum-mode except in some major modes
(require 'linum)
(define-global-minor-mode my-global-linum-mode linum-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'doc-view-mode
			   'undo-tree-visualizer
			   'pdf-view-mode))) ;; add other major modes in which to disable linum-mode
      (linum-mode))))
(my-global-linum-mode t)

;; Autocompletion: auto-complete, yasnippet and company

;; ;;YASnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; Auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-use-quick-help t)
(setq-default ac-sources '(ac-source-yasnippet
			   ac-source-words-in-same-mode-buffers
			   ac-source-dictionary)) ; see auto-complete doc for other sources

;; Company
(require 'company)
;;(require 'company-c-headers)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)
(company-quickhelp-mode t)
(setq company-quickhelp-delay 0)

(add-hook 'after-init-hook 'global-company-mode)

;; Use pdf-tools to open PDF files
(pdf-tools-install)

;; Configure ggtags for C and C++
(require 'cc-mode)
(require 'ggtags)

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
	      (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

;; ;; Semantic
(require 'semantic)
;; (require 'semantic/ia)
;; (require 'semantic/bovine/gcc)

;; (defun my-semantic-hook ()
;;   (imenu-add-to-menubar "TAGS"))
;; (add-hook 'semantic-init-hooks 'my-semantic-hook)

(global-semanticdb-minor-mode t)
(global-semantic-idle-scheduler-mode t)

(semantic-mode t)


;; Flycheck
(setq flycheck-relevant-error-other-file-show nil)
(setq flycheck-indication-mode nil)
(global-flycheck-mode t)

;; __________________________________________
;; Language specific configuration

;; Python

(defun my-python-hooks()
    (interactive)
    (setq tab-width 4)
    (setq python-indent-offset 4)

    (add-to-list
        'imenu-generic-expression
        '("Sections" "^#### \\[ \\(.*\\) \\]$" 1))

    ;; pythom mode keybindings
    (define-key python-mode-map (kbd "M-.") 'jedi:goto-definition)
    (define-key python-mode-map (kbd "M-,") 'jedi:goto-definition-pop-marker)
    (define-key python-mode-map (kbd "M-/") 'jedi:show-doc)
    (define-key python-mode-map (kbd "M-?") 'helm-jedi-related-names)
    ;; end python mode keybindings

    (add-to-list 'company-backends 'company-jedi)
    (jedi-mode)
    (setq jedi:complete-on-dot t)
    (setq jedi:tooltip-method '(pos-tip popup))
    (setq jedi:get-in-function-call-delay 0))

(add-hook 'python-mode-hook 'my-python-hooks)
;; End Python mode


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
