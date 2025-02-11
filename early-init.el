(setq inhibit-startup-message t
      ;; Hack to speedup startup, but have easy mode change !
      initial-scratch-message ";; (lisp-interaction-mode)\n;; (org-mode)\n\n"
      initial-major-mode 'fundamental-mode)
;; Emacs frame startup
;; Maximize the Emacs frame at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(when (version<= "29" emacs-version)
  (add-to-list 'default-frame-alist '(alpha-background . 95))
  (set-frame-parameter nil 'alpha-background 95))

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
