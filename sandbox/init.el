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

;; Disable the annoying startup message and Emacs logo
(setq inhibit-startup-message t)

;; Disable the message on top of the Scratch buffer
(setq initial-scratch-message nil)

;; Maximize the Emacs frame at startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode t)
(global-visual-line-mode 1)

(load-theme 'deeper-blue)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'icomplete)
(setq icomplete-show-matches-on-no-input t
      icomplete-compute-delay 0.05)
(icomplete-vertical-mode 1)

(global-set-key [remap kill-buffer] 'kill-this-buffer)

(require 'windmove)
(windmove-default-keybindings 'shift)
(windmove-swap-states-default-keybindings '(ctrl shift))
(windmove-mode 1)

(ffap-bindings)

(savehist-mode)

(defalias 'yes-or-no-p 'y-or-n-p) ;; Before Emacs 28

(show-paren-mode 1)
(setq show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(electric-pair-mode 1)
(electric-indent-mode 1)

(unless (package-installed-p 'company)
  (package-install 'company))

(global-company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
