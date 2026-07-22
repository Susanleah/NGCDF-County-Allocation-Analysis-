# NGCDF County Allocation Analysis — Group 9

**Course:** SDS 6103, Statistical Computing (2025/2026)
**Authors:** Stephen Nzambu Ndundu, Susan Leah Wangari
**Reviewer feedback addressed:** Dr. Tim Kamanu, PhD — review dated 20/07/2026

This project analyses National Government Constituencies Development Fund
(NGCDF) allocations (2014–2026, with linear projections to 2030) across two
groups of Kenyan counties:

- **Rift Valley group:** Trans Nzoia, Nandi, Uasin Gishu
- **Central group:** Murang'a, Kiambu, Kirinyaga

The analysis covers total allocations, per-voter allocations, CPI-adjusted
(real) allocations, intra-county equity (Gini coefficient), constituency
performance tiers, national ranking, and neighbouring-county comparisons.

## Repository contents

| File | Purpose |
|---|---|
| `GROUP_9_ new Report.qmd` | Full written Quarto report (rendered to PDF) |
| `GROUP 9 New Report .pdf` | Rendered output of the report |
| `Group_9_Application.R` | Interactive Shiny dashboard covering the same analysis |
| `NEW_NGCDF_DATA_28-JAN_2026.csv` | Source dataset (constituency-level NGCDF allocations) |
| `references.bib` | Bibliography for citations used in the report |
| `DATA_DICTIONARY.md` | Column-by-column definitions, units, and known data issues |
| `REPRODUCIBILITY_CHECKLIST.md` | Checklist to verify a clean-environment re-run before submission |
| `LICENSE` | Code licence (MIT) + data attribution note |
| `README.md` | This file |

## How to run

**Requirements:** R (≥ 4.2 recommended), plus these packages:
```r
install.packages(c(
  "tidyverse", "janitor", "readr",
  "shiny", "shinydashboard", "plotly", "DT", "scales", "ineq", "bslib"
))
```
(If a `renv.lock` file has been added per `REPRODUCIBILITY_CHECKLIST.md`, run
`renv::restore()` instead to get exact pinned versions.)

**Important:** keep `NEW_NGCDF_DATA_28-JAN_2026.csv` in the **same folder**
as both `GROUP_9_ new Report.qmd` and `Group_9_Application.R`. Neither file
uses `setwd()` or an absolute path — both rely on the data file sitting
alongside them.

**To render the report:**
```r
quarto::quarto_render("GROUP_9_ new Report.qmd")
```
or click "Render" in RStudio/Positron.

**To launch the interactive dashboard:**
```r
shiny::runApp("Group_9_Application.R")
```

## Data source

Constituency-level NGCDF allocation figures (2014–2026) compiled from NGCDF
Board disclosures, cross-referenced with IEBC constituency codes and voter
registration figures, and deflated using KNBS Consumer Price Index data.
Full citation details are in `references.bib`; column-level detail and known
data quality issues (e.g. a small number of missing allocation-year values)
are documented in `DATA_DICTIONARY.md`.

## Limitations

- 2027–2030 figures are **linear projections**, not official allocations, and
  should not be read as government commitments.
- Nominal KES figures are not directly comparable across years without the
  CPI adjustment applied in the report/app; raw year-over-year comparisons
  will overstate real funding growth.
- The Gini coefficient here measures *intra-county* distribution across
  constituencies, not inequality of end-beneficiary outcomes — it says
  nothing about how equitably funds were spent once allocated.
- Associations shown (e.g. between allocation and voter counts) are
  descriptive, not causal.

## Licence

Code is MIT-licensed (see `LICENSE`). The underlying government data is not
original work of the authors — see the data licence note in `LICENSE` and
`DATA_DICTIONARY.md` for attribution requirements.
