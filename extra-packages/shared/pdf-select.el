;;; pdf-select.el --- Select text using keyboard in PDF -*- lexical-binding: t -*-

;; Author: Leo Paviet Salomon, Daniel Nicolai
;; Maintainer: Leo Paviet Salomon
;; Version: 0.1
;; Package-Requires: (pdf-tools)
;; Homepage:
;; Keywords:


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Most of this code is adapted from Daniel Nicolai's pdf-avy-highlight
;;; More info here: https://github.com/dalanicolai/dala-emacs-lisp/blob/master/pdf-avy-highlight.el

(require 'pdf-view)
(require 'pdf-links)

;;; Code:



;; * ================================================================== *
;; * Customizations
;; * ================================================================== *

(defgroup pdf-select nil
  "Keyboard-centric selection in PDF documents"
  :group 'pdf-tools)

(defface pdf-select-read-link
  '((((background dark)) (:background "red" :foreground "yellow"))
    (((background light)) (:background "red" :foreground "yellow")))
  "Face used to determine the colors when reading the selection bounds.
Default to the same values as pdf-links-read-link"
  :group 'pdf-tools-faces)

(defcustom pdf-select-get-chars-function 'pdf-select-get-chars-2
  "Function used by `pdf-select-ask-bounds' to prompt for the
characters at the bound of the selection."
  :group 'pdf-select
  :type 'function)



;; * ================================================================== *
;; * Functions
;; * ================================================================== *

(defun pdf-select-read-char-action (query prompt)
  "Using PROMPT, interactively read a link-action.
BORROWED FROM `pdf-links-read-link-action'. See
`pdf-links-action-perform' for the interface."
  (pdf-util-assert-pdf-window)
  (let* ((links (pdf-info-search-string
                 query
                 (pdf-view-current-page)
                 (current-buffer)))
         (keys (pdf-links-read-link-action--create-keys
                (length links)))
         (key-strings (mapcar (apply-partially 'apply 'string)
                              keys))
         (alist (cl-mapcar 'cons keys links))
         (size (pdf-view-image-size))
         (colors (pdf-util-face-colors
                  'pdf-select-read-link pdf-view-dark-minor-mode))
         (args (list
                :foreground (car colors)
                :background (cdr colors)
                :formats
                `((?c . ,(lambda (_edges) (pop key-strings)))
                  (?P . ,(number-to-string
                          (max 1 (* (cdr size)
                                    pdf-links-convert-pointsize-scale)))))
                :commands pdf-links-read-link-convert-commands
                :apply (pdf-util-scale-relative-to-pixel
                        (mapcar (lambda (l) (car (cdr (assq 'edges l))))
                                links)))))
    (unless links
      (error "No links on this page"))
    (unwind-protect
        (let ((image-data nil))
          (unless image-data
            (setq image-data (apply 'pdf-util-convert-page args ))
            ;; (pdf-cache-put-image
            ;;  (pdf-view-current-page)
            ;;  (car size) image-data 'pdf-links-read-link-action)  ;; Don't want to override actual pdf-links cache
            )
          (pdf-view-display-image
           (create-image image-data (pdf-view-image-type) t))
          (pdf-links-read-link-action--read-chars prompt alist))
      (pdf-view-redisplay))))

(defun pdf-select-get-chars-2 ()
  "Almost exactly copied from `avy-goto-char-2'"
  (let ((c1 (read-char "char 1: " t)))
    (when (memq c1 '(? ?\b))
      (keyboard-quit))
    (let ((c2 (read-char "char 2: " t)))
      (cond ((eq c2 ?)
             (keyboard-quit))
            ((memq c2 '(?\b ?\d))
             (keyboard-escape-quit)))
      (concat (list c1 c2)))))

(defun pdf-select-get-coordinates (end)
  (let* ((query (funcall pdf-select-get-chars-function))
         (coords (list (or
                        (pdf-select-read-char-action query
                                                     "Please specify (SPC scrolls): ")
                        (error "No char selected")))))
    (car (alist-get 'edges (car coords)))))

;; Awful hack: this computation is undone immediately by
;; pdf-sync-backward-search
(defun pdf-select--coordinates-to-xy (coords)
  (let ((size (pdf-view-image-size)))
    (list (* (car coords) (float (car size)))
          (* (cadr coords) (float (cdr size))))))

(defun pdf-select-sync-backward-search ()
  (interactive)
  (if (pdf-view-active-region-p)
      (let ((beg-region (cl-subseq (car (pdf-view-active-region)) 0 2)))
        (apply 'pdf-sync-backward-search (pdf-select--coordinates-to-xy beg-region)))
    (let ((dest (pdf-select-get-coordinates nil)))
      (apply 'pdf-sync-backward-search (pdf-select--coordinates-to-xy dest)))))


;; * ================================================================== *
;; * Minor Mode
;; * ================================================================== *

(defvar pdf-select-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-é") 'pdf-select-ask-bounds)
    (define-key map (kbd "j") 'pdf-select-sync-backward-search)
    map))

;;;###autoload
(defun pdf-select-ask-bounds ()
  (interactive)
  (let* ((start (pdf-select-get-coordinates nil))
         (end (pdf-select-get-coordinates t))
         (edges (append (cl-subseq start 0 2) (cl-subseq end 2 4))))
    (pdf-util-scroll-to-edges edges)
    (setq pdf-view-active-region (append pdf-view-active-region (list edges)))
    (pdf-view--push-mark)
    (pdf-view-display-region)))

;;;###autoload
(define-minor-mode pdf-select-minor-mode
  "Allow selections to be made using the keyboard, using an a
avy-style prompt.
The main entry point is `pdf-select-ask-bounds'.

\\{pdf-select-minor-mode-map}"
  :group 'pdf-select
  (pdf-util-assert-pdf-buffer))

(provide 'pdf-select)

;;; pdf-select.el ends here
