;;; biblio-list.el --- Tabular view of bibtex entries -*- lexical-binding: t -*-

;; Author: Leo Paviet Salomon
;; Maintainer: Leo Paviet Salomon
;; Version: 0.1
;; Package-Requires: (dash)
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

;; Tabular view for bibtex bibliography.
;; TODO:
;; - View bib entry in other window + follow-mode
;; - More "actions" when pressing RET (view entry, open PDF, online DOI/URL ...)
;; - Tag system ? Integrate with bibtex (custom field), or separate ?
;; - Filter: by year, by author (probably hard)
;; - Fixes: robustness to some other workflows/naming schemes/etc
;; - "Performance": cache more things, refresh correctly, watch file changes...

;;; Code:

(require 'bibtex)
(require 'dash)

(defcustom biblio-list-files nil "Bib files used to populate the list.")

(defcustom biblio-list-library-path nil
   "Directory or list of directories in which files are stored.

This is used to show whether or not some article has an
associated file.

A bib entry whose key is KEY is considered to have an associated
file if KEY.EXT is a file in some directory of
`biblio-list-library-path' (directories are not searched
recursively), where EXT is an extension present in
`biblio-list-file-extensions'.")

(defcustom biblio-list-file-extensions '("pdf" "djvu")
  "Valid extensions when looking for files associated to entries.")

(defcustom biblio-list-file-indicator '("✓" . "✗")
  "Indicator to show the presence or absence of a file associated to
an entry.

Cons cell (FILE . NO-FILE).")

(defcustom biblio-list-format [("Author" 50 nil)
                               ("Title"  80 nil)
                               ("Type"   12 t)
                               ("Year"   5 t)
                               ("File"   2 t)]
  "Format of the table . See `tabulated-list-format' for more info.

The columns names must correspond to bibtex fields, up to case,
or to one of:

- FILE: show whether a FILE has been found for this entry.")

(defcustom biblio-list-format-entry-properties
  '(("author" face (:inherit message-header-to :weight normal))
    ("date" face font-lock-variable-name-face)
    ("year" face font-lock-variable-name-face)
    ("type" face font-lock-type-face)
    ("file"  . biblio-list--format-file-checkmark)
    ("tags" face font-lock-type-face))
  "Alist of (FIELD-NAME (PROPERTIES)*).

The properties will be applied to the field FIELD-NAME of an
entry with (apply 'propertize STRING PROPERTIES)

Each entry of the alist can also be of the form (FIELD . FUNC)
where FUNC is either a function name or a function object.
It should take one argument, the STRING to be propertized, and
return a list of properties.

In that case, the entry will be propertized with
(apply 'propertize STRING (funcall FUNC STRING)).")


;; Commands
(defun biblio-list-find-file ()
  "Open file of the entry at point."
  (interactive)
  (let* ((key (tabulated-list-get-id))
         (file (biblio-list--get-file key)))
    (if file
        (find-file file)
      (error "No file found for entry at point: %s" key))))

(defvar-keymap biblio-list-mode-map
  :doc "Local keymap for `biblio-list-mode'"
  :parent tabulated-list-mode-map
  "C-m" 'biblio-list-find-file)

;; Internals
(defvar-local biblio-list--raw-entries nil)
(defvar-local biblio-list--vec-entries nil)

;; Formatting
(defvar biblio-list--nonascii-latex-replacements
  '(("í" . "{\\\\'i}")
    ("æ" . "{\\\\ae}")
    ("ć" . "{\\\\'c}")
    ("é" . "{\\\\'e}")
    ("ä" . "{\\\\\"a}")
    ("è" . "{\\\\`e}")
    ("à" . "{\\\\`a}")
    ("á" . "{\\\\'a}")
    ("ø" . "{\\\\o}")
    ("ë" . "{\\\\\"e}")
    ("ü" . "{\\\\\"u}")
    ("ñ" . "{\\\\~n}")
    ("ņ" . "{\\\\c{n}}")
    ("ñ" . "{\\\\~n}")
    ("å" . "{\\\\aa}")
    ("ö" . "{\\\\\"o}")
    ("Á" . "{\\\\'A}")
    ("í" . "{\\\\'i}")
    ("ó" . "{\\\\'o}")
    ("ó" . "{\\\\'o}")
    ("ú" . "{\\\\'u}")
    ("ú" . "{\\\\'u}")
    ("ý" . "{\\\\'y}")
    ("š" . "{\\\\v{s}}")
    ("č" . "{\\\\v{c}}")
    ("ř" . "{\\\\v{r}}")
    ("š" . "{\\\\v{s}}")
    ("İ" . "{\\\\.I}")
    ("ğ" . "{\\\\u{g}}")
    ("δ" . "$\\\\delta$")
    ("ç" . "{\\\\c{c}}")
    ("ß" . "{\\\\ss}")
    ("≤" . "$\\\\le$")
    ("≥" . "$\\\\ge$")
    ("θ" . "$\\\\theta$")
    ("μ" . "$\\\\mu$")
    ("→" . "$\\\\rightarrow$")
    ("⇌" . "$\\\\leftrightharpoons$")
    ("×" . "$\\\\times$")
    ("°" . "$\\\\deg$")
    ("ş" . "{\\\\c{s}}")
    ("γ" . "$\\\\gamma$")
    ("ɣ" . "$\\\\gamma$")
    ("º" . "degC")
    ("η" . "$\\\\eta$")
    ("µ" . "$\\\\mu$")
    ("α" . "$\\\\alpha$")
    ("β" . "$\\\\beta$")
    ("ɛ" . "$\\\\epsilon$")
    ("Ⅵ" . "\\textrm{VI}")
    ("Ⅲ" . "\\textrm{III}")
    ("Ⅴ" . "\\textrm{V}")
    ("λ" . "$\\\\lambda$")
    ("π" . "$\\\\pi$")
    ("∞" . "$\\\\infty$")
    ("χ" . "$\\\\chi$")
    ("∼" . "\\\\textasciitilde{}")
    ("‑" . "\\\\textemdash{}")
    (" " . " ")
    ("…" . "...")
    ("•" . "\\\\textbullet ")
    ;; I think these are non-ascii spaces. there seems to be more than one.
    (" " . " ")
    (" " . " ")
    (" " . " ")
    ("–" . "-")
    ("−" . "-")
    ("–" . "-")
    ("—" . "-")
    ("‒" . "\\\\textemdash{}")
    ("‘" . "'")
    ("’" . "'")
    ("’" . "'")
    ("“" . "\"")
    ("’" . "'")
    ("”" . "\""))
  "Copied from `org-ref-nonascii-latex-replacements'.")

(defun biblio-list--nonascii-latex-replacement ()
  (dolist (pair biblio-list--nonascii-latex-replacements)
    (let ((in (car pair))
          (out (cdr pair)))
      (when-let ((new-in (and (= 2 (car (aref (syntax-table) (aref in 0))))
                              (s-upcase in))))
        (unless (or (string-equal in new-in)
                    (cl-every (lambda (char)
                                (eq 'ascii (char-charset char)))
                              new-in))  ; approximation ...
          (let ((new-out (with-temp-buffer
                           (insert out)
                           (capitalize-word -1)
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
            (cl-pushnew (cons new-in new-out)
                        biblio-list--nonascii-latex-replacements)))))))

(defun biblio-list--fix-stripped-brackets (s)
  (cl-loop with cur = 0
           minimize cur into min
           for c across s
           when (= c ?{) do (setf cur (1+ cur))
           when (= c ?}) do (setf cur (1- cur))
           finally (return (concat (make-string (- min) ?{)
                                   s
                                   (make-string (- cur min) ?})))))

;; We reverse engineer the "safe ascii encoding" of "special characters"
(defun biblio-list--undo-nonascii (s)
  (replace-regexp-in-string
   "{\\\\.\\({.}\\|.\\)}"
   (lambda (match)
     (or (car (rassoc (regexp-quote match)
                      biblio-list--nonascii-latex-replacements))
         (car (rassoc (regexp-quote (concat
                                     (substring match 0 -2)
                                     "{"
                                     (substring match -2)
                                     "}"))
                      biblio-list--nonascii-latex-replacements))
         (regexp-quote match)))
   (biblio-list--fix-stripped-brackets s)))

(defun biblio-list--clean-string (s)
  (if s
      (->> s
           (biblio-list--undo-nonascii)
           (replace-regexp-in-string "[\"{}]+" "")
           (replace-regexp-in-string "[\n\t ]+" " ")
           (replace-regexp-in-string "  +" " "))
    nil))

(defun biblio-list--format-file-checkmark (checkmark)
  (let ((file (car biblio-list-file-indicator))
        (nofile (cdr biblio-list-file-indicator)))
    (cond
     ((string= checkmark file) '(face (:foreground "green")))
     ((string= checkmark nofile) '(face (:foreground "red")))
     (t nil))))

(defun biblio-list--format-propertize (content field)
  (let ((props (cdr (assoc (downcase field)
                           biblio-list-format-entry-properties))))
    (if (functionp props)
        (apply 'propertize content (funcall props content))
      (apply 'propertize content props))))

(defun biblio-list--format-column (entry column)
  (let* ((field-name (car column))
         (field-content (biblio-list--get-field entry field-name))
         (field-cleaned (if field-content
                            (biblio-list--clean-string field-content)
                          "")))
    (biblio-list--format-propertize field-cleaned field-name)))

;; Parsing
(defun biblio-list--parse-files (&optional files)
  (setq biblio-list--raw-entries nil)
  (let (entries)
    (dolist (file (or files biblio-list-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (bibtex-map-entries (lambda (&rest args)
                              (push (bibtex-parse-entry) entries)))))
    (setq biblio-list--raw-entries (nreverse entries))))

(defun biblio-list--get-file (key)
  (catch 'find-file
    (dolist (dir (ensure-list biblio-list-library-path))
      (let* ((filebase (expand-file-name key dir))
             (files-ext (mapcar (lambda (ext)
                                  (concat filebase "." ext))
                                biblio-list-file-extensions))
             (file-found (cl-find-if 'file-exists-p files-ext)))
        (when file-found
          (throw 'find-file file-found))))))

(defun biblio-list--get-field (entry field)
  (setq field (downcase field))
  (cond
   ((string= field "type")
    (cdr (assoc "=type=" entry)))
   ((string= field "key")
    (cdr (assoc "=key=" entry)))
   ((string= field "file") (let* ((key (cdr (assoc "=key=" entry)))
                                  (file (biblio-list--get-file key)))
                             (if file
                                 (car biblio-list-file-indicator)
                               (cdr biblio-list-file-indicator))))
   (t (cdr (assoc field entry)))))

(defun biblio-list--convert-entries ()
  (setq biblio-list--vec-entries nil)
  (dolist (entry biblio-list--raw-entries)
    (let ((id (biblio-list--get-field entry "key"))
          (contents (cl-map 'vector
                            (lambda (col)
                              (biblio-list--format-column entry col))
                            biblio-list-format)))
      (push (list id contents) biblio-list--vec-entries)
      (setq biblio-list--vec-entries (nreverse biblio-list--vec-entries)))))

(defun biblio-list--extract-fields ()
  (let (fields)
    (dolist (entry biblio-list--entries)
      (dolist (field entry)
        (cl-pushnew (car field) fields :test 'equal)))
    fields))

(defun biblio-list--refresh (&optional ignore-auto no-confirm)
  (biblio-list--parse-files)
  (biblio-list--convert-entries)
  (setq tabulated-list-entries biblio-list--vec-entries))

;;; Entry points

;;;###autoload
(defun biblio-list-bibliography ()
  (interactive)
  (let ((buf (get-buffer-create "*Biblio list*")))
    (with-current-buffer buf
      (setq buffer-file-coding-system 'utf-8)
      ;; TODO: initialize everything
      (biblio-list--refresh)
      (biblio-list-mode)
      (tabulated-list-print 'remember 'update))
    (pop-to-buffer buf)))

;;;###autoload
(define-derived-mode biblio-list-mode tabulated-list-mode "Biblio list"
  "Major mode for browsing a bibliography.

\\<biblio-list-mode-map>
\\{biblio-list-mode-map}"
  :interactive nil
  (setq tabulated-list-format biblio-list-format)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (biblio-list--refresh)
  (tabulated-list-init-header)
  (setq revert-buffer-function 'biblio-list--refresh))

(provide 'biblio-list)

;;; biblio-list.el ends here
