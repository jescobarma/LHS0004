#=============================================================================
#
#  Script name: LHS000406.R
#
#  Date: 2025-10-17
#
#  Description: Descriptive Table 1 — unweighted frequencies with
#               survey-weighted row percentages by neonatal death; missing
#               values shown as "Missing". Mother's education collapsed to
#               "No education/Primary" vs "Secondary or higher".
#
#  History:
#    2026-10-25  Collapsed maternal education to two categories
#               ("No education/Primary" vs "Secondary or higher")
#
#   Usage: Rscript scripts/LHS000406/LHS000406.R
#
#   Input: data/derived/LHS000401.rda
#
#   Output: outputs/LHS000406.csv
#
#=============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(survey)
})

# Load variable formats shared across the pipeline.
source("scripts/LHS000490/LHS000490.R")

# Load the analysis dataset, apply labels, and collapse education to two categories.
load("data/derived/LHS000401.rda")
LHS000401 <- apply_formats(LHS000401, formats_LHS0004) %>%
  mutate(yob = factor(yob),
         meduc_c2 = factor(if_else(meduc_c3 == "Secondary or higher",
                                   "Secondary or higher", "No education/Primary"),
                           levels = c("No education/Primary", "Secondary or higher")))

# Variables to tabulate; meduc_c2 replaces the 3-category meduc_c3.
variables <- c("yob", "child_sex", "urban", "bord_spacing_c5",
               "underwt_birth", "preterm_birth", "mage_birth_c3", "meduc_c2",
               "ethnicity_c3", "any_substance_use", "wealth_index_c3", "anc_quality_score_c2")

# NAs become an explicit "Missing" level so they keep their rows in the design.
LHS000401 <- LHS000401 %>%
  mutate(across(all_of(variables), ~ forcats::fct_na_value_to_level(.x, level = "Missing")))

# Build the complex survey design using PSU, strata, and sampling weights.
design <- svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                    data = LHS000401, nest = TRUE)

# For one variable: unweighted counts and survey-weighted row percentages by neonatal death.
describe <- function(variable) {
  counts <- table(LHS000401[[variable]], LHS000401$neonatal)
  counts <- counts[rowSums(counts) > 0, , drop = FALSE]
  row_pct <- 100 * prop.table(svytable(as.formula(paste0("~", variable, " + neonatal")), design),
                              margin = 1)[rownames(counts), , drop = FALSE]
  bind_rows(
    tibble(Characteristic = variable, Neonatal_No = "", Neonatal_Yes = ""),
    tibble(Characteristic = paste0("  ", rownames(counts)),
           Neonatal_No  = sprintf("%d (%.2f%%)", counts[, "No"],  row_pct[, "No"]),
           Neonatal_Yes = sprintf("%d (%.2f%%)", counts[, "Yes"], row_pct[, "Yes"])))
}

# Compute overall totals to append as the last row.
total_counts <- table(LHS000401$neonatal)
total_pct <- 100 * prop.table(svytable(~neonatal, design))

# Assemble the full table across all variables
table_final <- bind_rows(
  map(variables, describe),
  tibble(Characteristic = "Total",
         Neonatal_No  = sprintf("%d (%.2f%%)", total_counts["No"],  total_pct["No"]),
         Neonatal_Yes = sprintf("%d (%.2f%%)", total_counts["Yes"], total_pct["Yes"])))

# Write the final table to a CSV file
write.csv(table_final, file = "outputs/LHS000406.csv", row.names = FALSE)
