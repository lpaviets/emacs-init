;; TeX-LaTeX-sentinel: add a somewhat simple way to hijack the process
;; to decide which command to run next
;;
;; TODO: May be this should really be dealt with in TeX-command-default ?

(defcustom TeX-LaTeX-extras-sentinels nil
  "List of extra tests to do when calling TeX-LaTeX-sentinel.

This is a list of functions of two arguments, a PROCESS and a
NAME.")

(defun TeX-LaTeX-default-sentinels (process name)
  (cond ((TeX-TeX-sentinel-check process name))
        ((and (save-excursion
                (re-search-forward
                 "^Package biblatex Warning: Please (re)run Biber on the file"
                 nil t))
              (with-current-buffer TeX-command-buffer
                (and (LaTeX-bibliography-list)
                     (TeX-check-files (TeX-master-file "bbl")
                                      (TeX-style-list)
                                      (append TeX-file-extensions
                                              BibTeX-file-extensions
                                              TeX-Biber-file-extensions)))))
         (message "%s%s" "You should run Biber to get citations right, "
                  (TeX-current-pages))
         (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                  TeX-command-Biber)))
        ((and (save-excursion
                (re-search-forward
                 "^\\(?:LaTeX\\|Package natbib\\) Warning: Citation" nil t))
              (with-current-buffer TeX-command-buffer
                (and (LaTeX-bibliography-list)
                     (TeX-check-files (TeX-master-file "bbl")
                                      (TeX-style-list)
                                      (append TeX-file-extensions
                                              BibTeX-file-extensions
                                              TeX-Biber-file-extensions)))))
         (message "%s%s" "You should run BibTeX to get citations right, "
                  (TeX-current-pages))
         (setq TeX-command-next (with-current-buffer TeX-command-buffer
                                  TeX-command-BibTeX)))
        ((re-search-forward "Package biblatex Warning: Please rerun LaTeX" nil t)
         (message "%s%s" "You should run LaTeX again, " (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^(biblatex)\\W+Page breaks have changed" nil t)
         (message "%s%s" "You should run LaTeX again - page breaks have changed, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^\\(?:LaTeX Warning: Label(s)\\|\
Package natbib Warning: Citation(s)\\)" nil t)
         (message "%s%s" "You should run LaTeX again to get references right, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward
          "^\\(?:(rerunfilecheck)\\|Package hyperref Warning:\\)\\W+\
Rerun to get outlines right" nil t)
         (message "%s%s" "You should run LaTeX again to get outlines right, "
                  (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^LaTeX Warning: Reference" nil t)
         (message "%s%s%s" name ": there were unresolved references, "
                  (TeX-current-pages))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        ((re-search-forward "^\\(?:LaTeX Warning: Citation\\|\
Package natbib Warning:.*undefined citations\\)" nil t)
         (message "%s%s%s" name ": there were unresolved citations, "
                  (TeX-current-pages))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        ((re-search-forward "^No file .*\\.\\(toc\\|lof\\|lot\\)\\.$" nil t)
         (message "%s" (concat "You should run LaTeX again to get "
                               (upcase (match-string-no-properties 1))
                               " right"))
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "Package longtable Warning: Table widths have \
changed\\. Rerun LaTeX\\." nil t)
         (message
          "%s" "You should run LaTeX again to get table formatting right")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^hf-TikZ Warning: Mark '.*' changed\\. \
Rerun to get mark in right position\\." nil t)
         (message
          "%s" "You should run LaTeX again to get TikZ marks in right position")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^Package Changebar Warning: \
Changebar info has changed." nil t)
         (message
          "%s" "You should run LaTeX again to get the change bars right")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward "^\\* xsim warning: \"rerun\"" nil t)
         (message
          "%s" "You should run LaTeX again to synchronize exercise properties")
         (setq TeX-command-next TeX-command-default))
        ((re-search-forward
          TeX-LaTeX-sentinel-banner-regexp nil t)
         (let* ((warnings (and TeX-debug-warnings
                               (TeX-LaTeX-sentinel-has-warnings)))
                (bad-boxes (and TeX-debug-bad-boxes
                                (TeX-LaTeX-sentinel-has-bad-boxes)))
                (add-info (when (or warnings bad-boxes)
                            (concat " (with "
                                    (when warnings "warnings")
                                    (when (and warnings bad-boxes) " and ")
                                    (when bad-boxes "bad boxes")
                                    ")"))))
           (message "%s" (concat name ": successfully formatted "
                                 (TeX-current-pages) add-info)))
         (let (dvi2pdf)
           (if (with-current-buffer TeX-command-buffer
                 (and TeX-PDF-mode (setq dvi2pdf (TeX-PDF-from-DVI))))
               (setq TeX-command-next dvi2pdf)
             (setq TeX-command-next TeX-command-Show))))
        (t
         (message "%s%s%s" name ": problems after " (TeX-current-pages))
         (setq TeX-command-next TeX-command-default))))

(defun-override lps/TeX-LaTeX-sentinel (process name)
  "Cleanup TeX output buffer after running LaTeX.

Parse the output buffer to collect errors and warnings if the
variable `TeX-parse-all-errors' is non-nil.

Open the error overview if
`TeX-error-overview-open-after-TeX-run' is non-nil and there are
errors or warnings to show.

This first runs `TeX-LaTeX-default-sentinels', setting
`TeX-command-next' as needed. It then runs, in order, all the
functions of `TeX-LaTeX-extras-sentinels with PROCESS and NAME as
arguments, until one of those returns a non-NIL value."
  (if TeX-parse-all-errors
      (TeX-parse-all-errors))
  (if (and (with-current-buffer TeX-command-buffer
             TeX-error-overview-open-after-TeX-run)
           (TeX-error-overview-make-entries
            (TeX-master-directory) (TeX-active-buffer)))
      (TeX-error-overview))

  ;; Default stuff
  (TeX-LaTeX-default-sentinels process name)

  ;; Now run custom checks
  (catch 'done
    (dolist (sent TeX-LaTeX-extras-sentinels)
      (when (funcall sent process name)
        (throw 'done nil))))

  ;; Check whether the idx file changed.
  (let (idx-file)
    (and (file-exists-p
          (setq idx-file
                (with-current-buffer TeX-command-buffer
                  (expand-file-name (TeX-active-master "idx")))))
         ;; imakeidx package automatically runs makeindex, thus, we need to be
         ;; sure .ind file isn't newer than .idx.
         (TeX-check-files (with-current-buffer TeX-command-buffer
                            (expand-file-name (TeX-active-master "ind")))
                          (with-current-buffer TeX-command-buffer
                            (list (file-name-nondirectory (TeX-active-master))))
                          '("idx"))
         (with-temp-buffer
           (insert-file-contents-literally idx-file)
           (not (equal
                 ;; Compare old md5 hash of the idx file with the new one.
                 (cdr (assoc idx-file LaTeX-idx-md5-alist))
                 (md5 (current-buffer)))))
         (push (cons idx-file t) LaTeX-idx-changed-alist)))

  (unless (TeX-error-report-has-errors-p)
    (run-hook-with-args 'TeX-after-compilation-finished-functions
                        (with-current-buffer TeX-command-buffer
                          (expand-file-name
                           (TeX-active-master (TeX-output-extension)))))))

;; TODO: The glossaries package *always* regenerate the .gsl files & co. May be
;; there is a way to avoid some re-compilation if this was actually not needed.
(defvar-local TeX-glossary-finished nil)

(defun lps/TeX-glossaries (process name)
  (with-current-buffer TeX-command-buffer
    (when (and (member "glossaries" (TeX-style-list))
               (string-equal (print TeX-command-next)
                             TeX-command-default) ; Something else needs to be run otherwise
               (not TeX-glossary-finished)
               (TeX-check-files (TeX-master-file "gls")
                                (TeX-style-list)
                                TeX-file-extensions))
      (message "Running Glossaries command on %s" (TeX-master-file))
      (TeX-command "Glossaries" 'TeX-master-file)
      (setq-local TeX-glossary-finished t)
      (setq TeX-command-next TeX-command-default)))
  nil)

(TeX-add-style-hook
 "glossaries"
 (lambda ()
   (add-to-list 'TeX-LaTeX-extras-sentinels 'lps/TeX-glossaries)
   (add-hook 'TeX-after-compilation-finished-functions (lambda () (setq TeX-glossary-finished nil)) nil t)))
