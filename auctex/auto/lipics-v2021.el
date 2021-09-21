(TeX-add-style-hook
 "lipics-v2021"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "twoside" "notitlepage" "fleqn")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("eucal" "mathscr") ("amsmath" "tbtags" "fleqn") ("threeparttable" "online") ("lineno" "left" "mathlines") ("hyperref" "pdfa" "unicode") ("caption" "labelsep=space" "singlelinecheck=false" "font={up,small}" "labelfont={sf,bf}" "listof=false") ("rotating" "figuresright") ("cleveref" "capitalise" "noabbrev")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "href")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "glyphtounicode"
    "article"
    "art10"
    "microtype"
    "inputenc"
    "lmodern"
    "fontawesome5"
    "fontenc"
    "textcomp"
    "eucal"
    "amssymb"
    "soul"
    "color"
    "xcolor"
    "babel"
    "amsmath"
    "enumerate"
    "graphicx"
    "array"
    "multirow"
    "tabularx"
    "threeparttable"
    "listings"
    "lineno"
    "hyperxmp"
    "totpages"
    "hyperref"
    "colorprofiles"
    "caption"
    "rotating"
    "subcaption"
    "xstring"
    "comment"
    "amsthm"
    "thm-restate"
    "cleveref"
    "aliascnt")
   (TeX-add-symbols
    '("flag" ["argument"] 1)
    '("supplementdetails" ["argument"] 2)
    '("relatedversiondetails" ["argument"] 2)
    '("ccsdesc" ["argument"] 1)
    '("affil" ["argument"] 1)
    '("addtosupplementmacro" 2)
    '("addtorelatedversionmacro" 2)
    '("Copyright" 1)
    '("patchBothAmsMathEnvironmentsForLineno" 1)
    '("patchAmsMathEnvironmentForLineno" 1)
    "proofsubparagraph"
    "claimqedhere"
    "lipicsEnd"
    "fs"
    "algorithmautorefname"
    "crefrangeconjunction"
    "creflastconjunction"
    "lemmaautorefname"
    "corollaryautorefname"
    "propositionautorefname"
    "exerciseautorefname"
    "definitionautorefname"
    "conjectureautorefname"
    "observationautorefname"
    "exampleautorefname"
    "noteautorefname"
    "remarkautorefname"
    "claimautorefname"
    "subtitle"
    "subtitleseperator"
    "EventLogoHeight"
    "copyrightline"
    "numberwithinsect"
    "usecleveref"
    "useautoref"
    "authoranonymous"
    "usethmrestate"
    "authorcolumns"
    "compactauthor"
    "pdfa"
    "sectionmark"
    "subsectionmark"
    "maketitle"
    "title"
    "date"
    "thefootnote"
    "topmattervskip"
    "orcidsymbol"
    "mailsymbol"
    "homesymbol"
    "complement"
    "makelabel"
    "TPTtagStyle"
    "tablenotes"
    "usehyperxmp"
    "C"
    "G"
    "F"
    "U"
    "thanks"
    "footnotemark"
    "cs"
    "normalfont"
    "scshape"
    "textrightarrow"
    "mathsf"
    "titlerunning"
    "authorrunning"
    "EventLongTitle"
    "EventShortTitle"
    "EventEditors"
    "EventNoEds"
    "EventLogo"
    "EventAcronym"
    "EventYear"
    "EventDate"
    "EventLocation"
    "SeriesVolume"
    "ArticleNo"
    "DOIPrefix"
    "and"
    "authorcolumnsMin"
    "author"
    "affiliation"
    "footnote"
    "hideLIPIcs"
    "keywords"
    "keywordsHeading"
    "ccsdescEnd"
    "subjclass"
    "subjclassHeading"
    "doiHeading"
    "category"
    "categoryHeading"
    "relatedversion"
    "relatedversionHeading"
    "supplement"
    "supplementHeading"
    "funding"
    "fundingHeading"
    "acknowledgements"
    "acknowledgementsHeading"
    "chapterautorefname"
    "sectionautorefname"
    "subsectionautorefname"
    "subsubsectionautorefname"
    "paragraphautorefname"
    "subparagraphautorefname")
   (LaTeX-add-environments
    "bracketenumerate"
    "alphaenumerate"
    "romanenumerate"
    "itemize"
    "enumerate"
    '("subproof" LaTeX-env-args ["argument"] 0)
    '("claimproof" LaTeX-env-args ["argument"] 0))
   (LaTeX-add-pagestyles
    "plain"
    "headings")
   (LaTeX-add-counters
    "TotPages"
    "currentauthor")
   (LaTeX-add-xcolor-definecolors
    "darkgray"
    "lipicsGray"
    "lipicsBulletGray"
    "lipicsLineGray"
    "lipicsLightGray"
    "lipicsYellow")
   (LaTeX-add-caption-DeclareCaptions
    '("\\DeclareCaptionLabelFormat{boxed}" "LabelFormat" "boxed"))
   (LaTeX-add-comment-incl-excls
    '("CCSXML" "exclude"))
   (LaTeX-add-amsthm-newtheorems
    "theorem"
    "lemma"
    "corollary"
    "proposition"
    "exercise"
    "definition"
    "conjecture"
    "observation"
    "example"
    "note"
    "remark"
    "claim")
   (LaTeX-add-amsthm-newtheoremstyles
    "claimstyle"))
 :latex)

