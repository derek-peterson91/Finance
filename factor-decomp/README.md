# Factor Risk Decomposition (R)

End-to-end workflow in R for rolling factor betas, portfolio risk decomposition, and daily P&L attribution.

## Subfolder structure
- R/: analysis functions
- data_raw/: raw downloads (factors, prices)
- data_clean/: merged/processed files
- reports/: knitted Rmd/HTML outputs
- app/: Shiny dashboard (to-do)

## Reproducibility
Uses renv. To restore:
```r
renv::restore()
