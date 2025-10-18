#=============================================================================
#
#  Script name: LHS000405.R
#  
#  Author: Alvaro Quijano
#
#  Date: 2025-10-17
#  
#  Description: Combine beta estimates from LHS000404 to produce 
#               final estimates using Zhou et al (2016) method 
#
#   Input: data/derived/LHS000404_&i.rda (i = 1-250)
#
#   Output: results/LHS000405.pdf 
#
#=============================================================================

# Load libraries
library(dplyr)
library(purrr)
library(tidyr)

# Load and combine all beta estimates
files <- list.files("data/derived/LHS000404", pattern = "LHS000404_\\d+\\.rda$", full.names = TRUE)

# Load all beta estimates into a single data frame
betas_all <- purrr::map_dfr(files, function(f) {
  load(f)   # loads 'final_betas'
  final_betas
})

# Combine estimates using Zhou et al. (2016) method
# Reference: Zhou, H., Elliott, M. R., & Raghunathan, T. E. (2016). Synthetic multiple-imputation 
# procedure for multistage complex samples. Journal of Official Statistics, 32(1), 231-256.

# Set number of bootstrap replicates over PSUs
csamples <- max(betas_all$indexL)

# Estimate combined MI estimates
MI_estimates <- betas_all %>%
  group_by(term) %>%
  summarise(
    Q_mi = round(mean(estimate),3),
    V_L  = var(estimate),
    .groups = "drop"
  ) %>%
  mutate(
    V_mi = round((1 + (1 / csamples) ) * V_L,3),
    se   = round(sqrt(V_mi),3),
    t    = qt(0.975, df = csamples - 1),
    LL   = round(Q_mi - se * t,3),
    UL   = round(Q_mi + se * t,3),
    OR    = round(exp(Q_mi),3),
    OR_LL = round(exp(LL),3),
    OR_UL = round(exp(UL),3)
  ) %>%
  rename(Parm = term, Estimate = Q_mi) %>%
  select(Parm, Estimate, se, LL, UL, OR, OR_LL, OR_UL)

# --- Write file ---
path <- "outputs/LHS000405.csv"
write.csv(MI_estimates, file = path)
