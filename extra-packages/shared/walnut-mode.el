;;; walnut-mode.el --- Major mode for Walnut mode -*- lexical-binding: t -*-

;; Author: Leo Paviet Salomon
;; Maintainer: Leo Paviet Salomon
;; Version: 0.1
;; Package-Requires: ()
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
;;; Major mode for interacting with the Walnut interpreter.
;;; See https://cs.uwaterloo.ca/~shallit/walnut.html
;;; Code:



(defvar walnut-java-path "java"
  "Path to the Java executable used by `run-walnut'")

(defvar walnut-default-directory
  "/home/paviets201/Documents/These/evenements/cirm_mois_thematique/Walnut/bin/"
  "Path to /bin/ subdirectory of Walnut")

(defvar walnut-cli-arguments `("-cp"
                               ,walnut-default-directory
                               "Main.Prover")
  "Commandline arguments to pass to `walnut-cli'.")

(defvar walnut-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-walnut'.")

(defvar walnut-prompt-regexp "^\\(?:\\[[^$]+\\]\\$\\)"
  "Prompt for `run-walnut'.")

(defvar walnut-buffer-name "*Walnut*"
  "Name of the buffer to use for the `run-walnut' comint instance.")

(defconst walnut-keywords
  '("def" "eval" "reg" "morphism" "promote" "image" "macro")
  "List of keywords to highlight in `walnut-font-lock-keywords'.")

(defconst walnut-commands walnut-keywords
  "List of commands to complete in `walnut-mode'")

(defvar walnut-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt walnut-keywords) "\\_>")
     . font-lock-keyword-face)
   ;; msb and lsb commands
   `(,(concat "\\_<\\\\?" (regexp-opt '("lsd_" "msd_")) "[_a-z0-9]+" "\\_>")
     . font-lock-keyword-face)
   '("\\_<[A-Z]+\\_>"
     . font-lock-variable-name-face))
  "Additional expressions to highlight in `walnut-mode'.")

;;; Completion
(defun walnut-capf ()
  (when-let* ((bds (bounds-of-thing-at-point 'symbol))
              (beg (car bds))
              (end (cdr bds)))
    (when (> end beg)
      (list beg end
            walnut-commands
            :annotation-function (lambda (_) "Keywords")))))

;;; The major mode
(defun walnut--initialize ()
  "Helper function to initialize Walnut."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

;;;###autoload
(define-derived-mode walnut-mode comint-mode "Walnut"
  "Major mode for `run-walnut'.

\\<walnut-mode-map>"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp walnut-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  (setq-local comint-dynamic-complete-functions nil)
  (add-hook 'completion-at-point-functions 'walnut-capf nil t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'font-lock-defaults) '(walnut-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) walnut-prompt-regexp)

  (walnut--initialize)
  (electric-pair-local-mode 1)
  (if (fboundp 'rainbow-mode)
      (rainbow-mode -1))
  (if (fboundp 'show-paren-mode)
      (show-paren-mode -1)))

;;;###autoload
(defun run-walnut ()
  "Run an inferior instance of `walnut-cli' inside Emacs."
  (interactive)
  (let* ((default-directory walnut-default-directory)
         (walnut-program walnut-java-path)
         (buffer (get-buffer-create walnut-buffer-name))
         (proc-alive (comint-check-proc buffer))
         (process (get-buffer-process buffer)))
    ;; if the process is dead then re-create the process and reset the
    ;; mode.
    (unless proc-alive
      (with-current-buffer buffer
        (apply 'make-comint-in-buffer "Walnut" buffer
               walnut-program nil walnut-cli-arguments)
        (walnut-mode)))
    ;; Regardless, provided we have a valid buffer, we pop to it.
    (when buffer
      (pop-to-buffer buffer))))

(provide 'walnut)

;;; walnut-mode.el ends here
