(TeX-add-style-hook
 "prova"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("inputenc" "utf8") ("fontenc" "T1")))
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
    "chapters/dataset"
    "chapters/methodology"
    "chapters/appendix_data"
    "article"
    "art10"
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
    "lipsum"
    "standalone")
   (LaTeX-add-labels
    "sec:headings"
    "sec:others"
    "fig:fig1"
    "tab:table")
   (LaTeX-add-bibliographies
    "biblio"))
 :latex)

