#=============================================================================
#
#  Script name: LHS000405.R
#
#  Date: 2026-10-19
#
#  Description: Table 2. Combine beta estimates from LHS000404 (standard
#               logistic and Firth logistic) using Zhou et al (2014) method
#               and compare them side by side as a formatted table of odds
#               ratios (95% CI) and betas (SE).
#
#   Input: data/derived/LHS000404/LHS000404_&i.rda (i = 1-500)
#          (each file contains betas_logistic and betas_firth)
#
#   Output: outputs/LHS000405.csv
#           data/derived/LHS000405.rda (combined Firth estimates)
#
#=============================================================================

# Load required packages.
library(dplyr)
library(purrr)
library(tidyr)

# Collect the 500 per-subset beta files written by LHS000404.
files <- list.files("data/derived/LHS000404", pattern = "LHS000404_\\d+\\.rda$", full.names = TRUE)

# Read one named beta object (betas_logistic or betas_firth) from every file and stack them.
load_betas <- function(object_name) {
  purrr::map_dfr(files, function(f) {
    e <- new.env(parent = emptyenv())
    load(f, envir = e)
    e[[object_name]]
  })
}

# Complete-data degrees of freedom: n_PSU - n_strata from the 2015 Colombia DHS design
df_com_design <- 3919L - 63L

# Reference: Zhou, H., Elliott, M. R., & Raghunathan, T. E. (2014). Synthetic multiple-imputation
combine_betas <- function(betas_all, df_com = df_com_design) {
  # Number of Polya (L) draws, which sets the between-draw variance inflation.
  L <- dplyr::n_distinct(betas_all$indexL)

  # Within each L draw, average the estimate over the A x B replicates to get the draw-level point estimate.
  per_l <- betas_all %>%
    group_by(term, indexL) %>%
    summarise(Q_l = mean(estimate), .groups = "drop")

  # Pool across draws
  per_l %>%
    group_by(term) %>%
    summarise(Q_mi = mean(Q_l), V_L = var(Q_l), .groups = "drop") %>%
    mutate(se = sqrt((1 + 1 / L) * V_L),
           t  = qt(0.975, df = min(df_com, L - 1)),
           LL = Q_mi - t * se, UL = Q_mi + t * se) %>%
    select(term, Estimate = Q_mi, se, LL, UL) %>%
    mutate(OR = exp(Estimate), OR_LL = exp(LL), OR_UL = exp(UL))
}

# Stack the betas from both methods across all subsets.
betas_ml    <- load_betas("betas_logistic")
betas_firth <- load_betas("betas_firth")

# Combine each method's betas via Zhou et al (2014) and save the pooled estimates.
logistic_fpbb <- combine_betas(betas_ml)
firth_fpbb    <- combine_betas(betas_firth)
save(logistic_fpbb, firth_fpbb, file = "data/derived/LHS000405.rda")

# Helper to render an odds ratio with its 95% CI as "OR (LL, UL)".
format_or <- function(or, ll, ul) sprintf("%.2f (%.2f, %.2f)", or, ll, ul)

# Build the side-by-side comparison table
table_final <- inner_join(logistic_fpbb, firth_fpbb,
                          by = "term", suffix = c("_logit", "_firth")) %>%
  transmute(
    Parameter = term,
    `Logistic OR (95% CI)` = format_or(OR_logit, OR_LL_logit, OR_UL_logit),
    `Firth OR (95% CI)`    = format_or(OR_firth, OR_LL_firth, OR_UL_firth),
    `Logistic beta (SE)`   = sprintf("%.2f (%.2f)", Estimate_logit, se_logit),
    `Firth beta (SE)`      = sprintf("%.2f (%.2f)", Estimate_firth, se_firth),
    `Delta beta (Firth - Logistic)` = sprintf("%.3f", Estimate_firth - Estimate_logit)
  )

# Write the table to CSV and echo it to the log.
write.csv(table_final, file = "outputs/LHS000405.csv", row.names = FALSE)
print(as.data.frame(table_final), row.names = FALSE)
