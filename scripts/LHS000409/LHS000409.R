#=============================================================================
#
#  Script name: LHS000409.R
#
#  Date: 2026-10-22
#
#  Description: Appendix table comparing the three ways of handling the
#               missing data in LHS000408 (mother's education collapsed):
#               odds ratios (95% CI) from complete-case analysis, FPBB MI and
#               conventional MI (design ignored in the imputation model), and
#               the width of the complete-case and conventional MI intervals
#               relative to FPBB MI on the log-OR scale (each method's 95% CI
#               width, log(upper) - log(lower), and the complete-case and
#               conventional MI widths divided by the FPBB MI width).
#
#   Usage: Rscript scripts/LHS000409/LHS000409.R
#
#   Input: data/derived/LHS000401.rda
#          data/derived/LHS000408.rda (logit_fpbb, complete_case, conventional_mi)
#
#   Output: outputs/LHS000409.csv
#
#=============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
})

load("data/derived/LHS000401.rda")
load("data/derived/LHS000408.rda")   # logit_fpbb, complete_case, conventional_mi

# Display order of LHS000418 without the intercept; term is NA for reference rows
layout <- tribble(
  ~variable,                        ~level,                       ~level_tex,                       ~term,
  "Year of birth",                  "2010",                       "2010",                           NA,
  "Year of birth",                  "2011",                       "2011",                           "yob2011",
  "Year of birth",                  "2012",                       "2012",                           "yob2012",
  "Year of birth",                  "2013",                       "2013",                           "yob2013",
  "Year of birth",                  "2014",                       "2014",                           "yob2014",
  "Year of birth",                  "2015",                       "2015",                           "yob2015",
  "Child sex",                      "Male",                       "Male",                           NA,
  "Child sex",                      "Female",                     "Female",                         "child_sexFemale",
  "Birth weight",                   "Normal",                     "Normal",                         NA,
  "Birth weight",                   "Low",                        "Low",                            "underwt_birthYes",
  "Preterm birth",                  "No",                         "No",                             NA,
  "Preterm birth",                  "Yes",                        "Yes",                            "preterm_birthYes",
  "Birth order and spacing",        "Firstborn",                  "Firstborn",                      NA,
  "Birth order and spacing",        "2-3 & <36 months",           "2--3 \\& $<36$ months",          "bord_spacing_c52–3 (<36 months apart)",
  "Birth order and spacing",        "2-3 & >=36 months",          "2--3 \\& $\\ge 36$ months",      "bord_spacing_c52–3 (≥36 months apart)",
  "Birth order and spacing",        "4+ & <36 months",            "4+ \\& $<36$ months",            "bord_spacing_c5>3 (<36 months apart)",
  "Birth order and spacing",        "4+ & >=36 months",           "4+ \\& $\\ge 36$ months",        "bord_spacing_c5>3 (≥36 months apart)",
  "Mother's age at birth",          "20-34 years",                "20--34 years",                   NA,
  "Mother's age at birth",          "<=19 years",                 "$\\le 19$ years",                "mage_birth_c3≤19 years",
  "Mother's age at birth",          "35+ years",                  "35+ years",                      "mage_birth_c3≥35 years",
  "Maternal education",             "No education/Primary",       "No education/Primary",           NA,
  "Maternal education",             "Secondary or higher",        "Secondary or higher",            "meduc_c3Secondary or higher",
  "Maternal ethnicity",             "Non-Indigenous/Non-Afrodescendant", "Non-Indigenous/Non-Afrodescendant", NA,
  "Maternal ethnicity",             "Afrodescendant",             "Afrodescendant",                 "ethnicity_c3Afro-descendant",
  "Maternal ethnicity",             "Indigenous",                 "Indigenous",                     "ethnicity_c3Indigenous",
  "Antenatal care quality",         "Low/Moderate",               "Low/Moderate",                   NA,
  "Antenatal care quality",         "High",                       "High",                           "anc_quality_score_c2High",
  "Substance use during pregnancy", "No",                         "No",                             NA,
  "Substance use during pregnancy", "Yes",                        "Yes",                            "any_substance_useYes",
  "Wealth index",                   "Wealthier/Wealthiest",       "Wealthier/Wealthiest",           NA,
  "Wealth index",                   "Middle",                     "Middle",                         "wealth_index_c3Middle",
  "Wealth index",                   "Poorer/Poorest",             "Poorer/Poorest",                 "wealth_index_c3Poor",
  "Place of residence",             "Rural",                      "Rural",                          NA,
  "Place of residence",             "Urban",                      "Urban",                          "urbanUrban"
)

stopifnot(setequal(c("(Intercept)", na.omit(layout$term)), logit_fpbb$term))

# log-OR interval width is 2 t se, so the ratio to FPBB MI compares interval lengths on the plot scale
summarise_fit <- function(est) {
  est %>% transmute(term, t = qt(0.975, df), se = sqrt(variance),
                    OR = exp(estimate), OR_LL = exp(estimate - t * se), OR_UL = exp(estimate + t * se),
                    width = 2 * t * se) %>%
    select(-t, -se)
}

data_columns <- c(`Year of birth` = "yob", `Child sex` = "child_sex", `Birth weight` = "underwt_birth",
                  `Preterm birth` = "preterm_birth", `Birth order and spacing` = "bord_spacing_c5",
                  `Mother's age at birth` = "mage_birth_c3", `Maternal education` = "meduc_c3",
                  `Maternal ethnicity` = "ethnicity_c3", `Antenatal care quality` = "anc_quality_score_c2",
                  `Substance use during pregnancy` = "any_substance_use", `Wealth index` = "wealth_index_c3",
                  `Place of residence` = "urban")
stopifnot(setequal(names(data_columns), layout$variable))
pct_missing <- 100 * colMeans(is.na(LHS000401[data_columns]))

estimates <- layout %>%
  mutate(pct_missing = unname(pct_missing[match(variable, names(data_columns))])) %>%
  left_join(summarise_fit(complete_case) %>% rename_with(~ paste0(.x, "_cc"), -term), by = "term") %>%
  left_join(summarise_fit(logit_fpbb) %>% rename_with(~ paste0(.x, "_fpbb"), -term), by = "term") %>%
  left_join(summarise_fit(conventional_mi) %>% rename_with(~ paste0(.x, "_cmi"), -term), by = "term") %>%
  mutate(ratio_cc = width_cc / width_fpbb, ratio_cmi = width_cmi / width_fpbb)

table_csv <- estimates %>%
  transmute(Variable = variable, Level = level, pct_missing = round(pct_missing, 1),
            across(starts_with("OR"), ~ round(.x, 3)),
            across(starts_with("width"), ~ round(.x, 3)),
            across(starts_with("ratio"), ~ round(.x, 2)))

write.csv(table_csv, file = "outputs/LHS000409.csv", row.names = FALSE, na = "")

print(as.data.frame(table_csv), row.names = FALSE)
