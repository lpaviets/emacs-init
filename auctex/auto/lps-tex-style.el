;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "lps-tex-style"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("tikz" "") ("tikz-cd" "") ("amsmath" "") ("amssymb" "") ("amsthm" "") ("amsfonts" "") ("stmaryrd" "") ("mathtools" "") ("thm-restate" "") ("hyperref" "") ("cleveref" "") ("url" "") ("subcaption" "") ("relsize" "") ("ifthen" "") ("graphicx" "") ("xcolor" "dvipsnames") ("tcolorbox" "") ("listings" "") ("lps-tex-style-colours" "") ("lps-tex-style-commands" "") ("lps-tex-style-mathenv" "") ("lps-tex-style-beamer" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "fontenc"
    "inputenc"
    "tikz"
    "tikz-cd"
    "amsmath"
    "amssymb"
    "amsthm"
    "amsfonts"
    "stmaryrd"
    "mathtools"
    "thm-restate"
    "hyperref"
    "cleveref"
    "url"
    "subcaption"
    "relsize"
    "ifthen"
    "graphicx"
    "xcolor"
    "tcolorbox"
    "listings"
    "lps-tex-style-colours"
    "lps-tex-style-commands"
    "lps-tex-style-mathenv"
    "lps-tex-style-beamer")
   (LaTeX-add-environments
    '("proof" LaTeX-env-args ["argument"] 0)
    '("subproof" LaTeX-env-args ["argument"] 0)
    '("function*" LaTeX-env-args ["argument"] 0)
    '("procedure*" LaTeX-env-args ["argument"] 0)
    '("function" LaTeX-env-args ["argument"] 0)
    '("procedure" LaTeX-env-args ["argument"] 0)
    '("\\algocf@envname*" LaTeX-env-args ["argument"] 0)
    '("\\algocf@envname" LaTeX-env-args ["argument"] 0)
    '("algocf@algorithm" LaTeX-env-args ["argument"] 0)
    '("mylem" LaTeX-env-args ["argument"] 0)
    '("calculs" LaTeX-env-args ["argument"] 0)
    '("calculs:rcl" LaTeX-env-args ["argument"] 3))
   (LaTeX-add-amsthm-newtheorems
    "proposition"
    "corollary"
    "lemma"
    "theorem"
    "definition"
    "example"
    "examples"
    "conjecture"
    "remark"
    "notation"
    "observation"
    "claim"
    "question")
   (LaTeX-add-tcolorbox-tcbuselibraries
    "skins"
    "theorems"
    "breakable"
    "hooks"
    "magazine"))
 :latex)

