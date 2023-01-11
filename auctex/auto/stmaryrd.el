;;; stmaryrd.el --- AUCTeX style for `stmaryrd.sty'  -*- lexical-binding: t; -*-

;; Author: Léo Paviet Salomon
;; Keywords: tex

;; This file is not part of AUCTeX.

;;; Commentary:

;; This file adds support for `stmaryrd.sty'.

;; The commands were just taken out of the \DeclareMathSymbol from
;; stmaryrd.sty in the order specified there, i.e., \mathord, \mathbin,
;; \mathrel

;;; Code:

(require 'tex)
(require 'latex)

(defvar LaTeX-stmaryrd-symbols
  '("shortleftarrow"
    "shortrightarrow"
    "shortuparrow"
    "shortdownarrow"
    "Yup"
    "Ydown"
    "Yleft"
    "Yright"
    "varcurlyvee"
    "varcurlywedge"
    "minuso"
    "baro"
    "sslash"
    "bbslash"
    "moo"
    "varotimes"
    "varoast"
    "varobar"
    "varodot"
    "varoslash"
    "varobslash"
    "varocircle"
    "varoplus"
    "varominus"
    "boxast"
    "boxbar"
    "boxdot"
    "boxslash"
    "boxbslash"
    "boxcircle"
    "boxbox"
    "boxempty"
    "lightning"
    "merge"
    "vartimes"
    "fatsemi"
    "sswarrow"
    "ssearrow"
    "curlywedgeuparrow"
    "curlywedgedownarrow"
    "fatslash"
    "fatbslash"
    "lbag"
    "rbag"
    "varbigcirc"
    "leftrightarroweq"
    "curlyveedownarrow"
    "curlyveeuparrow"
    "nnwarrow"
    "nnearrow"
    "leftslice"
    "rightslice"
    "varolessthan"
    "varogreaterthan"
    "varovee"
    "varowedge"
    "talloblong"
    "interleave"
    "oast"
    "ocircle"
    "obar"
    "obslash"
    "olessthan"
    "ogreaterthan"
    "ovee"
    "owedge"
    "oblong"
    "inplus"
    "niplus"
    "nplus"
    "subsetplus"
    "supsetplus"
    "subsetpluseq"
    "supsetpluseq"
    "Lbag"
    "Rbag"
    "llparenthesis"
    "rrparenthesis"
    "binampersand"
    "bindnasrepma"
    "trianglelefteqslant"
    "trianglerighteqslant"
    "ntrianglelefteqslant"
    "ntrianglerighteqslant"
    "llfloor"
    "rrfloor"
    "llceil"
    "rrceil"
    "arrownot"
    "Arrownot"
    "Mapstochar"
    "mapsfromchar"
    "Mapsfromchar"
    "leftrightarrowtriangle"
    "leftarrowtriangle"
    "rightarrowtriangle"
    "bigtriangledown"
    "bigtriangleup"
    "bigcurlyvee"
    "bigcurlywedge"
    "bigsqcap"
    "bigbox"
    "bigparallel"
    "biginterleave"
    "bignplus"
    "llbracket"
    "rrbracket"
    "varcopyright"
    "longarrownot"
    "Longarrownot"
    "Mapsto"
    "mapsfrom"
    "Mapsfrom"
    "Longmapsto"
    "longmapsfrom"
    "Longmapsfrom"))

(TeX-add-style-hook
 "stmaryrd"
 (lambda ()
   (mapc 'TeX-add-symbols LaTeX-stmaryrd-symbols))
 TeX-dialect)

(defvar LaTeX-stmaryrd-package-options
  (append (list "heavyvircles"
                "only")
          LaTeX-stmaryrd-symbols))

;;; stmaryrd.el ends here.
