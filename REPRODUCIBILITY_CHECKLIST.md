# Reproducibility Checklist — Group 9 (NGCDF County Allocation Analysis)

Use this checklist before submitting or re-submitting, and tick each item only after actually verifying it — not from memory of a previous run.

## Environment
- [ ] R version recorded (`sessionInfo()` output is included at the end of the rendered report — check it matches what you actually used)
- [ ] All required packages installed: `tidyverse`, `janitor`, `readr`, `shiny`, `shinydashboard`, `plotly`, `DT`, `scales`, `ineq`, `bslib`
- [ ] `renv.lock` present and `renv::restore()` runs without error (see below if not yet set up)

## Data
- [ ] `NEW_NGCDF_DATA_28-JAN_2026.csv` is present in the **same folder** as `GROUP_9_ new Report.qmd` and `Group_9_Application.R`
- [ ] No `setwd()` or absolute/machine-specific path remains anywhere in either file
- [ ] `DATA_DICTIONARY.md` matches the actual columns in the CSV (re-check if the data file is ever updated)

## Clean-environment run
- [ ] Close R/RStudio completely, reopen, and render `GROUP_9_ new Report.qmd` from a **fresh session** (no leftover objects in the Global Environment)
- [ ] Confirm the rendered PDF/HTML opens without errors
- [ ] Spot-check at least 3 numbers/tables in the rendered output against the raw CSV by hand
- [ ] Run `shiny::runApp("Group_9_Application.R")` from a fresh R session and confirm the app launches without console errors
- [ ] Click through both group tabs (Rift Valley / Central) and confirm every chart, KPI, and table renders (not blank/error)

## Documentation
- [ ] `README.md` present and its "How to Run" steps actually work, tested by someone who didn't write the code
- [ ] `DATA_DICTIONARY.md` present
- [ ] `LICENSE` present
- [ ] `references.bib` present and every in-text citation (`@ngcdfact2015`, `@ngcdf2025`, `@iebc2022`, `@knbs2025`) resolves in the rendered output (no "?" citation markers)
- [ ] Author names appear consistently in the report, the app, and the README (currently: Stephen Nzambu Ndundu, Susan Leah Wangari)

## Before final submission
- [ ] Zip the folder and unzip it somewhere else (or send to a classmate) to confirm nothing was missing
- [ ] Open the unzipped copy fresh and re-render/re-run per the two checks above
- [ ] Delete any `.Rhistory`, `.RData`, or personal `.Rproj.user` files before zipping (these can carry machine-specific state)

## Note on `renv.lock`
If you haven't set up `renv` for this project yet:
```r
install.packages("renv")
renv::init()      # run once, from the project folder
renv::snapshot()  # after confirming the report/app both run
```
This creates `renv.lock`, pinning exact package versions so a grader's `renv::restore()` reproduces your environment exactly.
