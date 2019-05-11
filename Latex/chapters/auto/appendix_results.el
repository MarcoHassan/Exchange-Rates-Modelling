(TeX-add-style-hook
 "appendix_results"
 (lambda ()
   (LaTeX-add-labels
    "table:MAEunivariate"
    "tab:Cointegration"
    "table:MAEmultivariate"
    "table:GTS-OLS fit"))
 :latex)

