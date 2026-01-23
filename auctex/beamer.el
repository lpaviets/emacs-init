(defvar beamer-mode-map
  (define-keymap
    "C-M-x"   'lps/LaTeX-beamer-compile-frame
    "C-c M-r" 'lps/LaTeX-beamer-change-all-pauses
    "C-x n f" 'lps/LaTeX-beamer-narrow-to-frame
    "<remap> <mark-paragraph>" 'lps/LaTeX-beamer-mark-frame))

(define-minor-mode beamer-mode
  "A minor mode for editing LaTeX document using the beamer class.
It defines the following commands:

\\{beamer-mode-map}"
  :keymap beamer-mode-map
  (if beamer-mode
      (progn
        (advice-add 'TeX-fold-item :override 'lps/TeX-fold-item-beamer)
        (lps/LaTeX-beamer-frame-as-section)
        (lps/LaTeX-beamer-fold-all-frames))
    (progn
      (advice-remove 'TeX-fold-item 'lps/TeX-fold-item-beamer)
      (lps/LaTeX-beamer-remove-frame-as-section)
      (TeX-fold-clearout-buffer))))

;; Folding
(defvar lps/TeX-beamer-fold-frame-overlay-regexp
  (concat (regexp-quote TeX-esc)
          "begin[ \t]*{"
          "[A-Za-z*]+}.*?[ \t\n]*?"
          (regexp-quote TeX-esc)
          "frametitle[ \t]*{"
          "\\(.+?\\)}\n"))

(defcustom lps/TeX-beamer-frame-title-max-length 12
  "Maximal length of a frametitle displayed when the frame is folded.

For example, a frame with a \\frametitle{My super duper title} with a
value of 12 would be abbreviated as \[frame:My super du…\] when folded.")

(defun lps/TeX-fold-frame (type)
  "Hide the frame at point.

Return non-nil if a frame was found and folded, nil otherwise."
  (when (and (eq type 'env)
             (memq major-mode '(latex-mode LaTeX-mode))
             (string= (LaTeX-current-environment) "frame"))
    (when-let ((item-start (condition-case nil
                               (save-excursion
                                 (LaTeX-find-matching-begin) (point))
                             (error nil)))
               (item-end (TeX-fold-item-end item-start type))
               (item-name (or (save-excursion
                                (goto-char item-start)
                                (re-search-forward
                                 lps/TeX-beamer-fold-frame-overlay-regexp
                                 item-end t 1)
                                (match-string-no-properties 1))
                              "frame"))
               (display-string-spec (if (string= item-name "frame")
                                        "[frame]"
                                      (concat "[frame:"
                                              (truncate-string-to-width item-name
                                                                        lps/TeX-beamer-frame-title-max-length
                                                                        0 nil "…")
                                              "]")))
               (ov (TeX-fold-make-overlay item-start item-end type
                                          display-string-spec)))
      (TeX-fold-hide-item ov))))

(defun lps/TeX-fold-item-beamer (type)
  (or (lps/TeX-fold-frame type)
      ;; Copy of TeX-fold-env code:
      ;; Can't directly call it, as this is the function we advice,
      ;; as it is used as the entry point in most other functions
      ;; rather than the wrapper TeX-fold-<env/macro> ...
      (if (and (eq type 'env)
               (eq major-mode 'plain-tex-mode))
          (message
           "Folding of environments is not supported in current mode")
        (let ((item-start (cond ((and (eq type 'env)
                                      (eq major-mode 'context-mode))
                                 (save-excursion
                                   (ConTeXt-find-matching-start) (point)))
                                ((and (eq type 'env)
                                      (eq major-mode 'texinfo-mode))
                                 (save-excursion
                                   (Texinfo-find-env-start) (point)))
                                ((eq type 'env)
                                 (condition-case nil
                                     (save-excursion
                                       (LaTeX-find-matching-begin) (point))
                                   (error nil)))
                                (t
                                 (TeX-find-macro-start)))))
          (when item-start
            (let* ((item-name (save-excursion
                                (goto-char item-start)
                                (looking-at
                                 (cond ((and (eq type 'env)
                                             (eq major-mode 'context-mode))
                                        (concat (regexp-quote TeX-esc)
                                                "start\\([A-Za-z]+\\)"))
                                       ((and (eq type 'env)
                                             (eq major-mode 'texinfo-mode))
                                        (concat (regexp-quote TeX-esc)
                                                "\\([A-Za-z]+\\)"))
                                       ((eq type 'env)
                                        (concat (regexp-quote TeX-esc)
                                                "begin[ \t]*{"
                                                "\\([A-Za-z*]+\\)}"))
                                       (t
                                        (concat (regexp-quote TeX-esc)
                                                "\\([A-Za-z@*]+\\)"))))
                                (match-string-no-properties 1)))
                   (fold-list (cond ((eq type 'env) TeX-fold-env-spec-list-internal)
                                    ((eq type 'math)
                                     TeX-fold-math-spec-list-internal)
                                    (t TeX-fold-macro-spec-list-internal)))
                   fold-item
                   (display-string-spec
                    (or (catch 'found
                          (while fold-list
                            (setq fold-item (car fold-list))
                            (setq fold-list (cdr fold-list))
                            (when (member item-name (cadr fold-item))
                              (throw 'found (car fold-item)))))
                        ;; Item is not specified.
                        (if TeX-fold-unspec-use-name
                            (concat "[" item-name "]")
                          (if (eq type 'env)
                              TeX-fold-unspec-env-display-string
                            TeX-fold-unspec-macro-display-string))))
                   (item-end (TeX-fold-item-end item-start type))
                   (ov (TeX-fold-make-overlay item-start item-end type
                                              display-string-spec)))
              (TeX-fold-hide-item ov)))))))

;; Mark
(defun lps/LaTeX-beamer-mark-frame ()
  (interactive)
  (unless (member "beamer" TeX-active-styles)
    (error "Not in a beamer document"))
  (beginning-of-line)
  (while (not (looking-at-p "\\\\begin *{frame}"))
    (LaTeX-find-matching-begin))
  (forward-char)
  (LaTeX-mark-environment))

;; Adapted from:
;; https://mbork.pl/2016-07-04_Compiling_a_single_Beamer_frame_in_AUCTeX
(defun lps/LaTeX-beamer-compile-frame ()
  "Compile the current frame"
  (interactive)
  (let* ((sections (lps/LaTeX-beamer-all-sections))
         (add-sections (lambda ()
                         (save-excursion
                           (goto-char (point-min))
                           (re-search-forward "\\\\begin{document}" nil t)
                           (insert "\n"
                                   "% SECTIONS BEFORE POINT\n"
                                   (mapconcat 'identity (car sections) "\n"))
                           (goto-char (point-max))
                           (re-search-backward "\\\\end{document}" nil t)
                           (insert "\n"
                                   "% SECTIONS AFTER POINT\n"
                                   (mapconcat 'identity (cdr sections) "\n")))))
         (TeX-region-hook (cons add-sections TeX-region-hook)))
    (save-mark-and-excursion
      (lps/LaTeX-beamer-mark-frame)
      (TeX-command-run-all-region))))

;; TODO: finish integrating this
;;
;; Difficulty: inserting sections that appear *before* point above the "current"
;; text, and those appearing *after*, after.
;;
;;Moreover, empty sections are not appearing, and all sections are empty, as we
;;want to compile a single frame.
(defun lps/LaTeX-beamer-all-sections ()
  ;; Taken from `reftex-section-info'. Somewhat buggy, not an expert in RefTeX
  ;; code, so use at your own risk.
  ;; Inlines a bunch of stuff found in called functions too.
  (let (sections-before
        sections-after
        (start-point (point)))
    (save-excursion
      (goto-char (point-min))
      (save-match-data
        (while (re-search-forward reftex-section-regexp nil t)
          (let* ((macro (reftex-match-string 0))
                 (level-exp (cdr (assoc (match-string 2) reftex-section-levels-all)))
                 (text1 (buffer-substring-no-properties
                         (point)
                         (min (+ (point) 150)
                              (point-max)
                              (condition-case nil
                                  ;; Unneeded fanciness.
                                  (let ((forward-sexp-function nil))
                                    (up-list 1)
                                    (1- (point)))
                                (error (point-max))))))
                 ;; Literal can be too short since text1 too short. No big problem.
                 (text (reftex-nicify-text text1)))
            (when (<= 1 level-exp)
              (let ((section-text (concat macro text "}")))
                (if (<= (point) start-point)
                    (push section-text sections-before)
                  (push section-text sections-after))))))))
    (cons (nreverse sections-before) (nreverse sections-after))))

(defun lps/LaTeX-beamer-narrow-to-frame ()
  (interactive)
  (save-mark-and-excursion
    (lps/LaTeX-beamer-mark-frame)
    (narrow-to-region (region-beginning) (region-end))))

(defun lps/LaTeX-beamer-frame-as-section ()
  (require 'reftex)
  (unless (assoc-string "frametitle" reftex-section-levels)
    (setq-local reftex-section-levels
                (append reftex-section-levels
                        '(("frametitle" . -4)
                          ("framesubtitle" . -5)))))
  (reftex-reset-mode))

(defun lps/LaTeX-beamer-remove-frame-as-section ()
  (require 'reftex)
  (when (assoc-string "frametitle" reftex-section-levels)
    (setq-local reftex-section-levels
                (cl-remove-if
                 (lambda (item)
                   (let ((name (car item)))
                     (assoc-string name '("frametitle" "framesubtitle"))))
                 reftex-section-levels)))
  (reftex-reset-mode))

(defun lps/LaTeX-beamer-fold-all-frames ()
  (interactive)
  (lps/TeX-fold-all-of-env "frame"))

(defvar lps/LaTeX-beamer-pause-macros '("pause["
                                        "only<"
                                        "onslide<"
                                        "alt<"
                                        "item<")
  "List of LaTeX macros that specifying pauses or overlays in a beamer
frame, and whose syntax rougly follows the one used by \\onslide<...>

Time specifications in those macros will be modified by the
`lps/LaTeX-beamer-change-all-pauses' function.

The character introducting the parameter list, usually [, < or {, has
to be added to the end, e.g. if you want to recognize the overlays
attached to items in an itemize environment, add \"item<\" to this
variable")

(defun lps/LaTeX-beamer-change-all-pauses (n &optional from-here)
  "Increase by N all the pauses and overlays timesteps specified by
the macros of `lps/LaTeX-beamer-pause-macros' contained in the
current frame.

If FROM-HERE is non-nil, only change the ones after the point.

If the region is active, ignore FROM-HERE and only act on the region
instead."
  (interactive "*nChange by steps: \nP")
  (save-mark-and-excursion
    (save-restriction
      (let ((beg (cond
                  (from-here (point))
                  ((region-active-p) (region-beginning))
                  (t nil))))
        (unless (region-active-p)
          (lps/LaTeX-beamer-mark-frame)
          (setq beg (or beg (region-beginning))))
        (narrow-to-region beg (region-end))
        (goto-char (point-min))
        (while (re-search-forward
                (concat "\\\\" (regexp-opt lps/LaTeX-beamer-pause-macros))
                nil t)
          (let ((point (point))
                (end (re-search-forward "[[:space:]]*]\\|>\\|}" nil t)))
            (when (and end (not (texmathp)))
              (goto-char point)
              (while (re-search-forward "[[:digit:]]+" end t)
                (let ((num (string-to-number (match-string 0))))
                  (replace-match (number-to-string (+ num n))))))))))))

(TeX-add-style-hook "beamer" 'beamer-mode TeX-dialect)
