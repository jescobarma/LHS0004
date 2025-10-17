#=============================================================================
#
#  Script name: LHS000405.R
#  
#  Author: Alvaro Quijano
#
#  Date: 2025-10-17
#  
#  Description: Combine beta estimates from LHS000404 to produce final estimates
#               using Zhou et al (2016) method and generate a formatted
#               LaTeX-style regression table.
#
#   Input: data/derived/LHS000404_&i.rda (i = 1-250)
#
#   Output: results/LHS000405.pdf 
#
#=============================================================================

# Load libraries
library(dplyr)
library(purrr)
library(kableExtra)
library(tibble)
library(glue)

# Load and combine all beta estimates
files <- list.files("data/derived/LHS000404", pattern = "LHS000404_\\d+\\.rda$", full.names = TRUE)

betas_all <- purrr::map_dfr(files, function(f) {
  load(f)   # loads 'final_betas'
  final_betas
})


# Combine estimates using Zhou et al. (2016) method
# Reference: Zhou, H., Elliott, M. R., & Raghunathan, T. E. (2016). Synthetic multiple-imputation procedure for multistage complex samples. Journal of Official Statistics, 32(1), 231-256.

csamples <- 50  # number of bootstrap replicates over PSUs

MI_estimates <- betas_all %>%
  group_by(term) %>%
  summarise(
    Q_mi = mean(estimate),
    V_L  = var(estimate),
    L    = n(),
    .groups = "drop"
  ) %>%
  mutate(
    V_mi = (1 + 1 / L) * V_L,
    se   = sqrt(V_mi),
    t    = qt(0.975, df = pmax(L - 1, 1)),
    LL   = Q_mi - se * t,
    UL   = Q_mi + se * t,
    OR    = ifelse(sign(LL) == sign(UL), exp(Q_mi), NA_real_),
    OR_LL = ifelse(!is.na(OR), exp(LL), NA_real_),
    OR_UL = ifelse(!is.na(OR), exp(UL), NA_real_)
  ) %>%
  rename(Parm = term, Estimate = Q_mi) %>%
  select(Parm, Estimate, se, LL, UL, OR, OR_LL, OR_UL)

# --- Step 5. Write file ---
rtf_path <- "LHS000405.csv"
write.csv(MI_estimates, file = rtf_path)
