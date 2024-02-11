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
;; - Clean code: in particular, cleaner column->field mapping.
;; - View bib entry in other window + follow-mode
;; - More "actions" when pressing RET (view entry, open PDF, online DOI/URL ...)
;; - Tag system ? Integrate with bibtex (custom field), or separate ?
;; - Filter: by year, by author (probably hard)
;; - Fixes: robustness to some other workflows/naming schemes/etc
;; - "Performance": cache more things, refresh correctly, watch file changes...

;; Explanation:
;; Pipeline is as follows:
;; .bib file     --- (parsing function) --> ((alist of fields) . SOURCE-FILE) -->
;; raw entries   --- (processing)       --> (ID [DESC1 ... DESCN])            -->
;; vec entries   --- (formatting)       --> (ID [DESC1' ... DESCN'])          -->
;; call to personal printing function

;; ID is a list (BIBKEY SOURCE-FILE RAW-ENTRY)
;; In particular, entries in `tabulated-list-entries' keep a reference to the
;; "raw entry": this allows us to filter, mark, etc, on properties that are not
;; kept for display.

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

(defcustom biblio-list-format [("Author" 40 nil)
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

(defcustom biblio-list-preprocess-field
  '(("author" . biblio-list-preprocess-authors))
  "Alist of (FIELD . FUNC) to pre-process the fields of an entry.

If some field has an entry, the function FUNC will be called with
a single argument, the content of the field, before creating the
object to pass to the tabulated list. In particular:

- this operation is performed *before* formatting.

- this function might receive NIL, and take appropriate action if
it wants to replace by a default or placeholder value.")

(defcustom biblio-list-printer 'biblio-list-format-print-entry
  "Function for inserting an entry in the table.

See `tabulated-list-printer' for more info.")

;;; Commands and public functions
(defun biblio-list-find-file (key)
  "Open file associated to an entry KEY.

When called interactively, KEY is the key of the entry at point."
  (interactive (list (biblio-list---key-at-point)))
  (let* ((file (biblio-list--get-file key)))
    (if file
        (find-file file)
      (error "No file found for entry at point: %s" key))))

(defun biblio-list-show-bib-entry (key file)
  "Show bibtex entry corresponding to KEY in FILE.

When called interactively, both parameters are picked from the
entry at point."
  (interactive (list (biblio-list---key-at-point)
                     (biblio-list--get-source-file-at-point)))
  (let ((buf (or (get-file-buffer file)
                 (find-buffer-visiting file))))
    (find-file file)
    (widen)
    (goto-char (point-min))
    (bibtex-search-entry key)
    (unless buf
      (kill-buffer))))

(defvar-keymap biblio-list-mode-map
  :doc "Local keymap for `biblio-list-mode'"
  :parent tabulated-list-mode-map
  "C-m" 'biblio-list-find-file
  "SPC" 'biblio-list-show-bib-entry
  "/ /" 'biblio-list-clear-filter
  "/ d" 'biblio-list-filter-by-date
  "/ a" 'biblio-list-filter-by-any-author
  "/ A" 'biblio-list-filter-by-all-authors
  "/ t" 'biblio-list-filter-by-title)

(defun biblio-list-preprocess-authors (names)
  (when names
    (dolist (pattern bibtex-autokey-name-change-strings)
      (setq names (replace-regexp-in-string (car pattern)
                                            (cdr pattern)
                                            names t)))
    (mapconcat 'capitalize
               (mapcar #'bibtex-autokey-demangle-name
                       (split-string names "[ \t\n]+and[ \t\n]+"))
               ", ")))

(defun biblio-list-format-print-entry (id cols)
  (tabulated-list-print-entry id (biblio-list--format-entry cols)))

;;; Internals
(defvar-local biblio-list--raw-entries nil)
(defvar-local biblio-list--vec-entries nil)

;;; Utils

(defun biblio-list--normalize-field-name (field)
  (setq field (downcase field))
  (cond
   ((string= field "type") "=type=")
   ((string= field "key") "=key=")
   (t field)))

(defun biblio-list--get-field (entry field)
  (setq field (biblio-list--normalize-field-name field))
  (cond
   ((string= field "file")
    (let* ((key (cdr (assoc "=key=" entry)))
           (file (biblio-list--get-file key)))
      (if file
          (car biblio-list-file-indicator)
        (cdr biblio-list-file-indicator))))
   (t (cdr (assoc field entry)))))


;;; Cleaning the entry: removing special characters, etc

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

;;; Parsing
(defun biblio-list---key-at-point ()
  (nth 0 (tabulated-list-get-id)))

(defun biblio-list--get-source-file-at-point ()
  (nth 1 (tabulated-list-get-id)))

(defun biblio-list--parse-files (&optional files)
  "Parses files and fills biblio-list--raw-entries with
(PARSED-ENTRY . SOURCE-FILE)."
  (setq biblio-list--raw-entries nil)
  (let (entries)
    (dolist (file (or files biblio-list-files))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (bibtex-map-entries (lambda (&rest args)
                              (push (cons (bibtex-parse-entry t) file)
                                    entries)))))
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

;;; Processing raw entries
(defun biblio-list--preprocess-field (content field)
  (let* ((func (cdr (assoc field biblio-list-preprocess-field)))
         (processed (if func
                        (funcall func content)
                      content)))
    (biblio-list--clean-string (or processed ""))))

(defun biblio-list--preprocess-entry (entry)
  (let ((new-entry
         (cl-loop for (col . rest) across biblio-list-format
                  for field = (biblio-list--normalize-field-name col)
                  for content = (biblio-list--get-field entry col)
                  for processed = (biblio-list--preprocess-field content field)
                  collect processed)))
    (vconcat new-entry)))

(defun biblio-list--process-entry (entry)
  (let* ((file (cdr entry))
         (entry (car entry))
         (id (list (biblio-list--get-field entry "key") file entry))
         (contents (biblio-list--preprocess-entry entry)))
    (list id contents)))

(defun biblio-list--process-all-entries ()
  (setq biblio-list--vec-entries nil)
  (dolist (entry biblio-list--raw-entries)
    (push (biblio-list--process-entry entry) biblio-list--vec-entries)
    (setq biblio-list--vec-entries (nreverse biblio-list--vec-entries))))

;;; Formatting the entry for display
(defun biblio-list--format-file-checkmark (checkmark)
  (let ((file (car biblio-list-file-indicator))
        (nofile (cdr biblio-list-file-indicator)))
    (cond
     ((string= checkmark file) '(face (:foreground "green")))
     ((string= checkmark nofile) '(face (:foreground "red")))
     (t nil))))

(defun biblio-list--format-column (content column)
  (let ((props (cdr (assoc (downcase column)
                           biblio-list-format-entry-properties))))
    (if (functionp props)
        (apply 'propertize content (funcall props content))
      (apply 'propertize content props))))

(defun biblio-list--format-entry (entry)
  (vconcat (cl-loop for (col . rest) across biblio-list-format
                    for field across entry
                    collect (biblio-list--format-column field col))))

;;; Filtering
(defun biblio-list--filter-by (predicate)
  "Filter \"*Biblio list*\" buffer by PREDICATE.

PREDICATE is a function which will be called with one argument, a
entry, and returns t if that object should be listed in the
Biblio list."
  ;; Update `tabulated-list-entries' so that it contains all
  ;; packages before searching.
  (biblio-list--refresh)
  (let (found-entries)
    (dolist (entry tabulated-list-entries)
      (when (funcall predicate entry)
        (push entry found-entries)))
    (if found-entries
        (progn
          (setq tabulated-list-entries found-entries)
          (biblio-list--redisplay t nil))
      (user-error "No corresponding entries found"))))

(defun biblio-list-filter-by-date (after before)
  (interactive (list (read-number "Articles between date ...: ")
                     (read-number "and date: ")))
  (biblio-list--filter-by
   (lambda (entry)
     (cl-destructuring-bind ((key file raw-entry) vec-entry)
         entry
       (let* ((date (biblio-list--get-field raw-entry "year"))
              (num-date (and date (string-to-number date))))
         (and num-date (<= after num-date before)))))))

(defun biblio-list-clear-filter ()
  (interactive)
  (biblio-list--refresh)
  (biblio-list--redisplay 'remember nil))

;; Not very robust, assumes a specific format ...
;; TODO: pre-process all authors to give completion ?
(defun biblio-list-filter-by-all-authors (authors)
  (interactive (list (mapcar 'capitalize
                             (completing-read-multiple "Authors: " nil))))
  (biblio-list--filter-by
   (lambda (entry)
     (cl-destructuring-bind ((key file raw-entry) vec-entry)
         entry
       (let* ((entry-author (biblio-list--get-field raw-entry "author"))
              (entry-author-clean (biblio-list--preprocess-field entry-author "author"))
              (entry-author-split (split-string entry-author-clean ", ")))
         (cl-loop for req-author in authors
                  always (member req-author entry-author-split)))))))

(defun biblio-list-filter-by-any-author (authors)
  (interactive (list (mapcar 'capitalize
                             (completing-read-multiple "Authors: " nil))))
  (biblio-list--filter-by
   (lambda (entry)
     (cl-destructuring-bind ((key file raw-entry) vec-entry)
         entry
       (let* ((entry-author (biblio-list--get-field raw-entry "author"))
              (entry-author-clean (biblio-list--preprocess-field entry-author "author"))
              (entry-author-split (split-string entry-author-clean ", ")))
         (cl-loop for req-author in authors
                  thereis (member req-author entry-author-split)))))))

(defun biblio-list-filter-by-title (title)
  (interactive (list (read-string "Title: ")))
  (biblio-list--filter-by
   (lambda (entry)
     (cl-destructuring-bind ((key file raw-entry) vec-entry)
         entry
       (let* ((entry-title (or (biblio-list--get-field raw-entry "title")
                               (biblio-list--get-field raw-entry "booktitle")))
              (entry-clean (biblio-list--preprocess-field entry-title "title"))
              (case-fold-search t))
         (string-match title entry-clean))))))

;;; Tabulated-list-mode functions
(defun biblio-list--refresh (&optional ignore-auto no-confirm)
  (biblio-list--parse-files)
  (biblio-list--process-all-entries)
  (setq tabulated-list-entries biblio-list--vec-entries))

(defun biblio-list--redisplay (&optional remember-pos update)
  (tabulated-list-init-header)
  (tabulated-list-print remember-pos update))

;;; Entry points

;;;###autoload
(defun biblio-list-bibliography ()
  (interactive)
  (let ((buf (get-buffer-create "*Biblio list*")))
    (with-current-buffer buf
      (setq buffer-file-coding-system 'utf-8)
      ;; TODO: initialize everything
      (biblio-list-mode)
      (biblio-list--refresh)
      (biblio-list--redisplay 'remember 'update))
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
  (setq tabulated-list-printer biblio-list-printer)
  (biblio-list--refresh)
  (biblio-list--redisplay t nil)
  (setq revert-buffer-function 'biblio-list--refresh))

(provide 'biblio-list)

;;; biblio-list.el ends here
