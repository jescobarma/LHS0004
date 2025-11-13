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
| LHS000403 | Multiple Imputation using the MICE package | R | 2025-10-17 | completed |
| LHS000404 | Logistic regression of neonatal mortality across bootstrap × Polya × imputation datasets | R | 2025-10-17 | completed |
| LHS000405 | Combine beta estimates using Zhou et al. (2016) MI method | R | 2025-10-17 | completed |
| LHS000406 | Generates a descriptive table for manuscript | R | 2025-10-20 | completed |
| LHS000490 | Formats for categorical variables | R | 2025-10-20 | completed |

---


## How to Run LHS000401.R 


**Requirements:**
R ≥ 4.3.0 with packages `haven` and `dplyr`.


**Directory structure expected:**
```
project/
├── code/
│ ├── LHS000401/LHS000401.R
│ └── LHS000490/LHS000490.R
├── data/
│ ├── raw/COKR72FL.SAS7BDAT
│ └── derived/
```


**To run from RStudio:**
```r
setwd("code/LHS000401")
source("LHS000401.R")
```


**To run from terminal:**
```
cd code/LHS000401
Rscript LHS000401.R
```


The script outputs:
```
data/derived/LHS000401.rda
```



---
*Automatically generated from project.yaml*
