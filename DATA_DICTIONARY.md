# Data Dictionary — `NEW_NGCDF_DATA_28-JAN_2026.csv`

**Project:** NGCDF County Allocation Analysis (Group 9, SDS 6103)
**Authors:** Stephen Nzambu Ndundu, Susan Leah Wangari
**File format:** CSV, UTF-8, comma-delimited, 23 columns
**Grain:** one row per parliamentary constituency (290 constituencies)
**Source:** National Government Constituencies Development Fund (NGCDF) Board — constituency allocation records, cross-referenced with IEBC constituency/voter codes. See `references.bib` for full citations.
**Accessed:** 28 January 2026

## Columns

| Column name (raw) | Type | Description | Units / format | Notes |
|---|---|---|---|---|
| `County` | text | County name (47 counties) | — | Cleaned to canonical spelling in code (e.g. "Murang'a" variants collapsed) |
| `Constituency` | text | Parliamentary constituency name | — | Primary analysis grain |
| `Sub-County` | text | Administrative sub-county name | — | Not used in current analysis; retained for future geographic joins |
| `IEBC_CN` | text | IEBC constituency name code (raw, uppercase, may include trailing whitespace) | — | Superseded by `ConstNAME` for display |
| `IEBC-CC` | integer | IEBC county code | numeric ID | Official IEBC county numbering |
| `IEBC-CN` | text | IEBC county name (raw) | — | Duplicate of `County`, kept for traceability to source |
| `IEBC-CONSC` | integer | IEBC constituency code within county | numeric ID | Order of constituency within its county per IEBC |
| `VOTERS` | integer | Registered voters in the constituency | count of people | From IEBC 2022 general election voter register (`@iebc2022`); used as the denominator for per-voter allocation metrics |
| `ColCODE` | integer | Internal column/constituency code used for joins | numeric ID | Analysis-internal; not an official government code |
| `ConstNAME` | text | Cleaned/display-ready constituency name | — | Used in place of `IEBC_CN` for labels and tables |
| `2014`–`2026` | numeric | Annual NGCDF allocation to the constituency | Kenyan Shillings (KES), nominal (not inflation-adjusted) | See below for CPI adjustment; **some cells are blank** (see Known Data Quality Issues) |

## Derived / computed fields (created in code, not in the raw CSV)

| Field | Created in | Description |
|---|---|---|
| `voters` | `load_data()` | Cleaned copy of `VOTERS`, coerced to integer |
| `county`, `constituency` | `load_data()` | Lowercased/cleaned copies of `County`, `Constituency` after `janitor::clean_names()` |
| CPI-adjusted allocation | `CPI_DF` join (2014 = 100 base) | Nominal KES values deflated to real 2014 KES using the Consumer Price Index series (`@knbs2025`) |
| Gini coefficient | `build_group_data()` | Intra-county inequality of constituency-level allocations, computed per county per year |
| Tier (High/Mid/Low) | `make_tiers()` | Intra-county tertile classification of constituencies by 2025 allocation |
| Per-voter allocation | analysis code | Allocation ÷ `voters`, KES per registered voter |
| 2027–2030 forecast | linear projection | Extrapolated from the 2014–2026 trend per constituency; **a projection, not an official allocation** |

## Known data quality issues

- **Missing values:** a small number of constituency-year cells (e.g. 2023–2024 for some constituencies) are blank in the source file. These are coerced to `NA` in code (`suppressWarnings(as.numeric(.x))`) rather than silently dropped or zero-filled. Any analysis or chart built on these years should visibly flag the gap rather than imply zero funding.
- **Duplicate/near-duplicate identifier columns:** `IEBC_CN`/`IEBC-CN`/`County` and `ConstNAME`/`Constituency` carry overlapping information from source-system merges; `county`/`constituency` (cleaned) are the columns actually used downstream.
- **County name variants:** "Murang'a" appears with inconsistent apostrophe/spacing in the raw file and is standardised in code via a regex match.
- **Nominal currency only:** raw allocation figures are nominal KES; comparisons across years should use the CPI-adjusted series, not raw figures, to avoid mistaking inflation for real funding growth.

## Access / provenance

Data was compiled from publicly available NGCDF Board constituency allocation disclosures and IEBC voter registration statistics. No personally identifiable information is present — all records are at the constituency (aggregate) level. See `references.bib` for the four sources cited (`ngcdfact2015`, `ngcdf2025`, `iebc2022`, `knbs2025`).
