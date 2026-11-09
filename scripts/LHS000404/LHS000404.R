#=============================================================================
#
#  Script name: LHS000404.R
#
#  Date: 2025-10-17
#
#  Description: Fits standard logistic and Firth (MPL_Jeffreys) logistic
#               regression on each of the 2,500 FPBB-imputed datasets for
#               one LHS000403 subset, and saves both sets of beta estimates.
#               Estimates are combined across all subsets by LHS000408.
#
#  History:
#    2026-10-25  Added Firth (bias-reduced) logistic regression (brglm2,
#               type = "MPL_Jeffreys") alongside the standard logistic fit;
#               collapsed maternal education to two categories
#               ("No education/Primary" vs "Secondary or higher").
#
#   Input: data/derived/LHS000403/LHS000403_&i.rda (i = 1-500)
#
#   Output: data/derived/LHS000404/LHS000404_&i.rda  (betas_logistic, betas_firth)
#
#=============================================================================

# Read the subset index passed by the SLURM array wrapper.
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

# Load required packages.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(mice)
  library(brglm2) # Firth's logistic regression (type = "MPL_Jeffreys")
})

# Load variable formats shared across the pipeline.
source("scripts/LHS000490/LHS000490.R")

# Load the mice imputation object for this subset.
load(file = sprintf("data/derived/LHS000403/LHS000403_%d.rda", task_id))

# Extract all 5 completed datasets and attach the three-level index (A = imputation, L = Polya draw, B = PSU bootstrap).
completed_data <- mice::complete(imp, "long") %>%
  mutate(indexA = .imp,
         indexL = floor((IMPL_ - 1) / 5) + 1,
         indexB = IMPL_ - (indexL - 1) * 5,
         Index  = paste0(indexA, "_", indexL, "_", indexB)) %>%
  select(-c(IMPL_, OBS_, .id, .imp))

# Split into a named list of 5 data frames, one per completed dataset.
imputed_datasets <- split(completed_data, completed_data$Index)

# remove objcts to free up memory
rm(completed_data, imp)

# Define the neonatal mortality model formula; the same for both methods.
model_formula <- neonatal ~ yob + urban + child_sex +
  bord_spacing_c5 + preterm_birth + mage_birth_c3 + meduc_c3 +
  wealth_index_c3 + underwt_birth + ethnicity_c3 + any_substance_use +
  anc_quality_score_c2

# Set reference levels: 2010 for year of birth; collapse education to two categories with "No education/Primary" as reference.
prepare <- function(data) {
  data %>% mutate(
    yob      = relevel(as.factor(yob), ref = "2010"),
    meduc_c3 = factor(if_else(meduc_c3 == "Secondary or higher",
                              "Secondary or higher", "No education/Primary"),
                      levels = c("No education/Primary", "Secondary or higher"))
  )
}

# Fit standard logistic and Firth logistic on each completed dataset; stop if any Firth fit fails to converge.
fits <- imputed_datasets %>%
    imap(~ {
    data_clean <- prepare(.x)

    fit_logistic <- glm(model_formula, data = data_clean,
                        family = binomial(link = "logit"))

    fit_firth <- glm(model_formula, data = data_clean,
                     family = binomial(link = "logit"),
                     method = "brglmFit", type = "MPL_Jeffreys")
    if (!fit_firth$converged || anyNA(coef(fit_firth)))
      stop("Firth fit did not converge for Index ", .y)

    list(
      logistic = tibble(term = names(coef(fit_logistic)),
                        estimate = unname(coef(fit_logistic)),
                        Index = .y),
      firth    = tibble(term = names(coef(fit_firth)),
                        estimate = unname(coef(fit_firth)),
                        converged = fit_firth$converged,
                        Index = .y)
    )
  })

# Parse composite Index into the three integer components.
separate_index <- function(df) {
  df %>% tidyr::separate(Index, into = c("indexA", "indexL", "indexB"),
                         sep = "_", convert = TRUE)
}

# Collect coefficients from both methods
betas_logistic <- map_dfr(fits, "logistic") %>% separate_index()
betas_firth    <- map_dfr(fits, "firth")    %>% separate_index()

# Save both sets of estimates to a single file.
save(betas_logistic, betas_firth,
     file = sprintf("data/derived/LHS000404/LHS000404_%d.rda", task_id))
