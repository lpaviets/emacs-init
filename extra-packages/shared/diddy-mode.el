;;; diddy-mode.el --- Major mode for Diddy mode -*- lexical-binding: t -*-

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
;;; Major mode for Diddy files.
;;; See https://github.com/ilkka-torma/diddy
;;; Code:

(require 'cl-lib)



(defconst diddy-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defvar *diddy--topology-keywords* '("line" "grid" "square" "squaregrid" "king" "kinggrid" "triangle" "trianglegrid" "hex" "hexgrid" "CR"))

(defvar *diddy--commands*
  '((("alphabet" "alph")
     ((:or simple_list mapping)) ("default"))
    ("topology"
     ((:or topology_keyword nested_list)))
    (("dim" "dimension")
     (number) ("onesided"))
    (("nodes" "vertices")
     ((:or simple_list mapping)))
    ("set_weights" (mapping))
    ("save_environment" (label))
    ("load_environment" (label))
    ("run" (label))
    (("sft" "SFT") (label (:or formula pattern_list)))
    (("sofic1d sofic1D") (label label))
    (("compute_forbidden_patterns" "calculate_forbidden_patterns")
     (label) ("radius" "filename"))
    ("load_forbidden_patterns" (label label))
    ("determinize" (label))
    ("minimize" (label))
    (("wang" "Wang")
     (label "tiles" nested_list) ("inverses") ("topology" "use_topology" "custom_topology"))
    ("intersection" (label simple_list))
    ("union" (label simple_list))
    ("product"
     (label simple_list) ("tracks" "env"))
    (("block_map" "blockmap" "CA")
     (label nested_list) ("domain" "codomain"))
    ("compose" (label simple_list))
    ("relation" (label label) ("tracks"))
    ("preimage" (label label label))
    ("fixed_points" (label label))
    ("spacetime_diagram"
     (label label) ("time_axis") ("twosided"))
    (("TFG" "topological_full_group_element") (label nested_list))
    (("show_formula" "print_formula") (label))
    (("show_parsed" "print_parsed") (label))
    (("show_forbidden_patterns" "print_forbidden_patterns") (label))
    (("equal" "equals") (label label) ("method" "expect") ("verbose"))
    (("contains" "contain")
     (label label) ("method" "expect") ("verbose"))
    (("compare_sft_pairs" "compare_SFT_pairs") () ("method"))
    (("compare_sft_pairs_equality" "compare_SFT_pairs_equality") () ("method"))
    (("compute_CA_ball" "calculate_CA_ball") (number simple_list) ("filename"))
    ("minimum_density"
     (label simple_list)
     ("threads" "mode" "chunk_size" "symmetry" "print_freq_pop" "print_freq_cyc" "expect")
     ("verbose" "rotate"))
    ("density_lower_bound"
     (label simple_list simple_list)
     ("radius" "print_freq" "expect")
     ("verbose" "show_rules"))
    ("entropy_upper_bound" (label simple_list) ("radius"))
    ("entropy_lower_bound" (label simple_list simple_list))
    (("TFG_loops" "topological_full_group_element_loops") (label label))
    ("tiler"
     (label)
     ("x_size" "y_size" "node_offsets" "pictures" "gridmoves")
     ("x_periodic" "y_periodic"))
    ("tile_box" (label number))
    ("keep_tiling" (label) ("min" "max"))
    ("start_cache" (number number))
    ("end_cache" ()))
  "A list of Diddy commands and their arguments.
Each element is a list of following form:
 (COMMAND REQ-ARGS &optional OPT-ARGS FLAGS)
 where
 - COMMAND is a string or a list of strings
 - REQ-ARGS is a list of symbols or of other lists of the form (:or SYMBOLS*)
 - OPT-ARGS and FLAGS are lists of strings")

(defvar *diddy--commands-flatten-aliases*
  (flatten-list (mapcar #'car *diddy--commands*)))

(defconst diddy--font-lock-defaults
  `(((,(concat "\\<%" (regexp-opt *diddy--commands-flatten-aliases*) "\\>")
      0 font-lock-builtin-face)
     ("\\<\\(?:let\\|in\\)\\>" 0 font-lock-keyword-face)
     (,(concat "\\<" (regexp-opt *diddy--topology-keywords*) "\\>")
      0 font-lock-type-face))))

(defun diddy-command-to-docstring (name args)
  (cl-destructuring-bind (req &optional opts flags) args
    (with-temp-buffer
      (insert (propertize (upcase name) 'face 'font-lock-builtin-face))
      (insert " ")
      (dolist (req-arg req)
        (if (symbolp req-arg)
            (insert (upcase (symbol-name req-arg)) " ")
          (cl-assert (eq (car req-arg) :or))
          (insert "(")
          (dolist (req-arg-choice (cdr req-arg))
            (insert (upcase (symbol-name req-arg-choice)) " "))
          (delete-backward-char 1)
          (insert ") ")))
      (when opts
        (insert "?[")
        (dolist (opt-arg opts)
          (insert (upcase opt-arg) " "))
        (delete-backward-char 1)
        (insert "]"))
      (when flags
        (insert " @[")
        (dolist (flag flags)
          (insert (upcase flag) " "))
        (delete-backward-char 1)
        (insert "]"))
      (buffer-substring (point-min) (point-max)))))

(defun diddy-current-command ()
  (when (save-excursion
          (re-search-backward "^ *%[a-z_]+" nil t nil))
    (cl-loop with command = (substring (match-string-no-properties 0) 1)
             for (cmd . args) in *diddy--commands*
             thereis (and (if (stringp cmd)
                              (string= cmd command)
                            (assoc-string command cmd))
                          (cons command args)))))

(defun diddy-eldoc-function (&rest _ignore)
  (let ((command (diddy-current-command)))
    (when command
      (diddy-command-to-docstring (car command) (cdr command)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.diddy\\'" . diddy-mode))

;;;###autoload
(define-derived-mode diddy-mode prog-mode "Diddy"
  :syntax-table diddy-syntax-table
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "--[\t -]*")
  (setq font-lock-defaults diddy--font-lock-defaults)
  (setq-local eldoc-documentation-function 'diddy-eldoc-function))

(provide 'diddy-mode)

;;; diddy-mode.el ends here
