;;; personal-shared-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "pdf-select" "pdf-select.el" (0 0 0 0))
;;; Generated autoloads from pdf-select.el

(autoload 'pdf-select-ask-bounds "pdf-select" nil t nil)

(autoload 'pdf-select-minor-mode "pdf-select" "\
Allow selections to be made using the keyboard, using an a
avy-style prompt.
The main entry point is `pdf-select-ask-bounds'.

This is a minor mode.  If called interactively, toggle the
`Pdf-Select minor mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `pdf-select-minor-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{pdf-select-minor-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "pdf-select" '("pdf-select-"))

;;;***

(provide 'personal-shared-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; personal-shared-autoloads.el ends here
