;; -*- lexical-binding: t; -*-

(TeX-add-style-hook
 "lps-tex-style"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("fontenc" "T1") ("inputenc" "utf8") ("tikz" "") ("tikz-cd" "") ("amsmath" "") ("amssymb" "") ("amsthm" "") ("amsfonts" "") ("stmaryrd" "") ("mathtools" "") ("thm-restate" "") ("hyperref" "") ("cleveref" "") ("url" "") ("subcaption" "") ("relsize" "") ("ifthen" "") ("graphicx" "") ("xcolor" "dvipsnames") ("tcolorbox" "") ("listings" "")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
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
    "listings")
   (TeX-add-symbols
    '("randomclipline" ["argument"] 4)
    '("randomclip" ["argument"] 4)
    '("tileRU" ["argument"] 0)
    '("tileLU" ["argument"] 0)
    '("tileUR" ["argument"] 0)
    '("tileUL" ["argument"] 0)
    '("tileUP" ["argument"] 0)
    '("WangTile" ["argument"] 6)
    '("WangTileNeutral" ["argument"] 6)
    '("shiftlang" ["argument"] 1)
    '("zeropoint" ["argument"] 0)
    '("machinenohalt" ["argument"] 1)
    '("machinehalt" ["argument"] 1)
    '("WangMono" 3)
    '("tfg" 1)
    '("commutator" 2)
    '("altgroup" 1)
    '("symgroup" 1)
    '("derivedsg" 1)
    '("grouppres" 2)
    '("homom" 2)
    '("homot" 1)
    '("pfg" 1)
    '("sqshift" 1)
    '("wire" 1)
    '("equivB" 1)
    '("word" 1)
    '("sqcover" 1)
    '("cover" 1)
    '("liftfree" 1)
    '("prelift" 1)
    '("lift" 1)
    '("bigballn" 1)
    '("balln" 1)
    '("decproblem" 1)
    '("compclass" 1)
    '("restr" 2)
    '("indicator" 1)
    '("id" 1)
    '("card" 1)
    '("norm" 1)
    '("intervalint" 1)
    '("intervaloo" 1)
    '("intervaloc" 1)
    '("intervalco" 1)
    '("intervalcc" 1)
    '("interval" 1)
    '("domain" 1)
    '("weight" 2)
    '("vect" 1)
    '("cylinder" 1)
    '("defn" 1)
    '("TODO" 1)
    "acts"
    "semidirect"
    "isomorphic"
    "surjection"
    "injection"
    "subsetf"
    "subpattern"
    "subgroup"
    "normalsg"
    "pgfrandomresetseed"
    "argmax"
    "argmin")
   (LaTeX-add-environments
    '("subproof" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-xcolor-definecolors
    "TLavander"
    "TAmethyst"
    "TBlue"
    "TCyan"
    "TGray"
    "TTeal"
    "TViolet"
    "TYellow"
    "TOrange"
    "TLime"
    "TRed"
    "TBlack"
    "TNeutral"
    "gridborder"
    "ColTheorem"
    "ColLemma"
    "ColDefinition"
    "ColCorollary"
    "ColProposition"
    "ColProblem"
    "ColQuestion")
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
   (LaTeX-add-tcolorbox-newtcolorboxes
    '("theorembox" "3" "[" "")
    '("lemmabox" "3" "[" "")
    '("definitionbox" "3" "[" "")
    '("corollarybox" "3" "[" "")
    '("propositionbox" "3" "[" "")
    '("questionbox" "2" "[" "")
    '("decisionproblembox" "1" "" "")
    '("theorempersobox" "3" "[" ""))
   (LaTeX-add-tcolorbox-tcbuselibraries
    "skins"
    "theorems"
    "breakable"
    "hooks"
    "magazine"))
 :latex)

