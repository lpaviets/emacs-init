(TeX-add-style-hook
 "lps-tex-style"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "fontenc"
    "inputenc"
    "tikz"
    "tikz-cd"
    "amsmath"
    "amssymb"
    "amsthm"
    "mathtools"
    "thm-restate"
    "hyperref"
    "cleveref"
    "url"
    "xcolor"
    "subcaption"
    "stmaryrd"
    "relsize")
   (TeX-add-symbols
    '("tileRU" ["argument"] 0)
    '("tileLU" ["argument"] 0)
    '("tileUR" ["argument"] 0)
    '("tileUL" ["argument"] 0)
    '("tileUP" ["argument"] 0)
    '("shiftlang" ["argument"] 1)
    '("zeropoint" ["argument"] 0)
    '("tfg" 1)
    '("homom" 2)
    '("homot" 1)
    '("pfg" 1)
    '("sqshift" 1)
    '("wire" 1)
    '("equivB" 1)
    '("word" 1)
    '("sqcover" 1)
    '("cover" 1)
    '("decproblem" 1)
    '("compclass" 1)
    '("lift" 1)
    '("card" 1)
    '("norm" 1)
    '("interval" 1)
    '("vect" 1)
    '("cyl" 1)
    '("defn" 1)
    "TODO"
    "acts"
    "subpattern"
    "argmax"
    "argmin")
   (LaTeX-add-environments
    '("subproof" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-amsthm-newtheorems
    "conjecture"
    "proposition"
    "corollary"
    "lemma"
    "theorem"
    "remark"
    "definition"
    "example"
    "notation"
    "observation"
    "claim"))
 :latex)
