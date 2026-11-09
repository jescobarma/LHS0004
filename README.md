# Neonatal Mortality in the Colombian DHS 2015

**Project ID:** LHS0004  
**Lead:** --  
**Collaborators:** --  
**Status:** ongoing

---

## Description
Determinants of neonatal mortality using DHS 2015 data and the Finite Population Bayesian Bootstrap.

---

## Scripts Overview
| Code | Task | Software | Created | Status |
|------|------|-----------|----------|---------|
| LHS000401 | Analytic file derivation | R | 2025-10-15 | completed |
| LHS000402 | Bayesian Finite Population Bootstrap – Synthetic population generation | IVEware | 2025-10-16 | completed |
| LHS000403 | Multiple Imputation using the MICE package, one SLURM array task per synthetic population subset | R | 2025-10-17 | completed |
| LHS000404 | Standard and Firth logistic regression of neonatal mortality across bootstrap × Polya × imputation datasets | R | 2025-10-17 | completed |
| LHS000405 | Combine standard and Firth beta estimates using Zhou (2014) MI method | R | 2025-10-17 | completed |
| LHS000406 | Generates a descriptive table for manuscript | R | 2025-10-20 | completed |
| LHS000407 | Convergence diagnostics of the FPBB MI imputations (supplement figure) | R | 2026-10-22 | completed |
| LHS000408 | Logistic regression comparing FPBB MI, complete-case analysis, and conventional MI (forest plot) | R | 2026-10-21 | completed |
| LHS000409 | Appendix table comparing complete-case, FPBB MI, and conventional MI odds ratios and CI widths | R | 2026-10-22 | completed |
| LHS000410 | Delta-adjustment (MNAR) sensitivity analysis for birth weight under FPBB MI | R | 2026-10-23 | completed |
| LHS000411 | Delta-adjustment (MNAR) sensitivity analysis for antenatal care quality and substance use under FPBB MI | R | 2026-10-23 | completed |
| LHS000412 | Combine delta-adjusted estimates using Zhou (2014) MI method (sensitivity table and figure) | R | 2026-10-23 | completed |
| LHS000490 | Formats for categorical variables | R | 2025-10-20 | completed |

---


## How to Run LHS000401.R 


**Requirements:**
R ≥ 4.3.0 with packages `haven` and `dplyr`.


**Directory structure expected:**
```
project/
├── scripts/
│ ├── LHS000401/LHS000401.R
│ ├── LHS000402/LHS000402.xml
│ ├── LHS000403/LHS000403.R, LHS000403.sh
│ ├── LHS000404/LHS000404.R
│ ├── LHS000405/LHS000405.R
│ ├── LHS000406/LHS000406.R
│ ├── LHS000407/LHS000407.R
│ ├── LHS000408/LHS000408.R
│ ├── LHS000409/LHS000409.R
│ ├── LHS000410/LHS000410.R
│ ├── LHS000411/LHS000411.R
│ ├── LHS000412/LHS000412.R
│ └── LHS000490/LHS000490.R
├── data/
│ ├── raw/COKR72FL.SAS7BDAT (downloaded DHS file)
│ └── derived/
├── outputs/ (tables: .csv, .tex)
└── imgs/ (figures: .pdf)
```


**To run from RStudio:**
```r
setwd("scripts/LHS000401")
source("LHS000401.R")
```


**To run from terminal:**
```
cd scripts/LHS000401
Rscript LHS000401.R
```


The script outputs:
```
data/derived/LHS000401.rda
```

