(TeX-add-style-hook
 "Thesis"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1") ("color" "usenames" "dvipsnames") ("tcolorbox" "most")))
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "chapters/abstract"
    "chapters/intro"
    "chapters/methodology"
    "chapters/dataset"
    "chapters/results"
    "chapters/conclusion"
    "chapters/appendix_data"
    "chapters/appendix_figures"
    "chapters/appendix_tables"
    "chapters/declaration"
    "article"
    "art12"
    "arxiv"
    "inputenc"
    "fontenc"
    "hyperref"
    "url"
    "booktabs"
    "amsfonts"
    "nicefrac"
    "microtype"
    "amsmath"
    "mathrsfs"
    "amssymb"
    "eucal"
    "color"
    "standalone"
    "multirow"
    "caption"
    "dblfloatfix"
    "graphicx"
    "subcaption"
    "tcolorbox")
   (LaTeX-add-bibliographies
    "biblio"))
 :latex)

