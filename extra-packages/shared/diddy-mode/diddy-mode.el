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
(require 'compile)



(defcustom diddy-python "python3.9"
  "Path to the Python interpreter used to run `diddy'")

(defcustom diddy-path "~/Documents/Projects/proj-python/diddy/diddy.py"
  "Path to the actual `diddy' Python file")

(defcustom diddy-confirm-compile compilation-read-command
  "Non-nil means `diddy-run-file' asks for confirmation when sending
the evaluation command.

The command is built using `diddy-python', `diddy-path' and the
current buffer's filename.")

(defcustom diddy-save-before-compile t
  "Non-nil means save the current file automatically before calling
`diddy-run-file'.")

(defun diddy-run-file (file)
  (interactive (list (buffer-file-name)))
  (let* ((compile-command (concat diddy-python " " diddy-path " " file))
         (compile-command (if diddy-confirm-compile
                              (compilation-read-command compile-command)
                            compile-command)))

    (when diddy-save-before-compile
      (save-current-buffer))
    (save-some-buffers (not compilation-ask-about-save)
                       compilation-save-buffers-predicate)
    (setq-default compilation-directory default-directory)
    (compilation-start compile-command 'diddy-compilation-mode
                       (lambda (mode)
                         (format "*%s* <%s>"
                                 mode
                                 (file-name-nondirectory file))))))

;;; Internal
(defvar *diddy--topology-keywords* '("line"
                                     "grid"
                                     "square"
                                     "squaregrid"
                                     "king"
                                     "kinggrid"
                                     "triangle"
                                     "trianglegrid"
                                     "hex"
                                     "hexgrid"
                                     "CR"))

(defvar *diddy--other-keywords* '("let"
                                  "letnum"
                                  "in"
                                  "has"
                                  "abs"
                                  "dist")
  "Other keywords that are not commands (i.e. starting with a %
character) nor topologies. Various functions or syntactic
constructs are in this list.")

(defvar *diddy--quantifiers* '("A" "E" "O"
                               "AC" "EC" "OC")
  "Quantifiers for nodes")

(defvar *diddy--operators* '("="  "~~"  "~"  "@"
                             "!=" "!~~" "!~" "!@"
                             "->" "<->" "|" "&" "!"
                             "==" "/=" "<=" "<" ">=" ">"
                             "+" "*" "#"
                             ":=")
  "Operators: node comparison, node functions, boolean operators ...")

(defvar *diddy--commands*
  ;; Setting up the environment
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

    ;; Defining objects
    (("sft" "SFT") (label (:or formula pattern_list))
     ("onesided")
     ("simplify" "verbose"))
    (("sofic1d sofic1D") (label label))
    ("trace" (label label simple_list nested_list))
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
     (label nested_list) ("domain" "codomain") ("simplify" "verbose"))
    ("compose" (label simple_list))
    ("relation" (label label) ("tracks"))
    ("preimage" (label label label))
    ("fixed_points" (label label))
    (("spacetime_diagram" "spacetime")
     (label label) ("time_axis") ("twosided"))

    ;; TFG
    (("TFG" "topological_full_group_element") (label nested_list))

    ;; Printing objects
    (("show_conf" "print_conf") (label) () ("hide_contents"))
    (("show_formula" "print_formula") (label))
    (("show_parsed" "print_parsed") (label))
    (("show_forbidden_patterns" "print_forbidden_patterns") (label))
    (("show_graph" "print_graph") (label))
    ("show_environment" () ("sft"))
    ("info" (simple_list) () ("verbose"))

    ;; Comparing objects
    ("empty" (label) ("conf_name" "expect") ("verbose"))
    (("equal" "equals") (label label)
     ("method" "expect")
     ("verbose"))
    (("contains" "contain")
     (label label)
     ("method" "expect" "conf_name")
     ("verbose"))
    (("compare_sft_pairs" "compare_SFT_pairs") () ("method"))
    (("compare_sft_pairs_equality" "compare_SFT_pairs_equality") () ("method"))
    (("compute_CA_ball" "calculate_CA_ball") (number simple_list) ("filename"))

    ;; Analyzing dynamical properties
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

    ;; Visualization / finding individual tilings in SFT
    ("tiler"
     (label)
     ("x_size" "y_size" "node_offsets" "pictures" "gridmoves" "topology" "initial" "colors" "hidden")
     ("x_periodic" "y_periodic"))
    ("tile_box" (label number))
    ("keep_tiling" (label) ("min" "max"))

    ;; Technical commands
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

(defconst diddy-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    table))

(defconst diddy--font-lock-defaults
  `(((,(concat "\\<%" (regexp-opt *diddy--commands-flatten-aliases*) "\\>")
      0 font-lock-builtin-face)
     (,(concat "\\<" (regexp-opt *diddy--topology-keywords*) "\\>")
      0 font-lock-type-face)
     (,(concat "\\<" (regexp-opt *diddy--other-keywords*) "\\>")
      0 font-lock-keyword-face)
     (,(regexp-opt (mapcar 'regexp-quote *diddy--operators*))
      0 'font-lock-operator-face)
     (,(concat "\\<"(regexp-opt *diddy--quantifiers*))
      0 'font-lock-operator-face))))

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

(defun diddy-completion-at-point ()
  (save-match-data
    (save-excursion
      (when (looking-back "^[ \t]*\\(%[[:alpha:]]*\\)?")
        (let ((beg (match-beginning 1))
              (end (match-end 1)))
          (when (and beg end)
            (list (1+ beg) end *diddy--commands-flatten-aliases*)))))))

(defun diddy-eldoc-function (&rest _ignore)
  (let ((command (diddy-current-command)))
    (when command
      (diddy-command-to-docstring (car command) (cdr command)))))

(defvar diddy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'diddy-run-file)
    map)
  "Keymap for `diddy-mode'.")

;;; Compilation mode
(define-compilation-mode diddy-compilation-mode "Diddy[C]"
  "Major mode for output of Diddy programs."
  (add-hook 'compilation-mode-font-lock-keywords
            `(,(concat " "
                       (regexp-opt '("CONTAINS"
                                     "EQUAL"
                                     "DIFFERENT"
                                     "DOES NOT CONTAIN"
                                     "EMPTY"
                                     "NONEMPTY"
                                     "DOES"
                                     "DOES NOT")))
              0 compilation-info-face)
            nil t))


;;; Actual Diddy mode

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.diddy\\'" . diddy-mode))

;;;###autoload
(define-derived-mode diddy-mode prog-mode "Diddy"
  "Major mode for editing Diddy files.

\\{diddy-mode-map}"
  :syntax-table diddy-syntax-table
  (setq-local comment-start "-- ")
  (setq-local comment-start-skip "--[\t -]*")
  (setq font-lock-defaults diddy--font-lock-defaults)
  (setq-local eldoc-documentation-function 'diddy-eldoc-function)
  (add-hook 'completion-at-point-functions
            #'diddy-completion-at-point nil 'local))

(provide 'diddy-mode)

;;; diddy-mode.el ends here
