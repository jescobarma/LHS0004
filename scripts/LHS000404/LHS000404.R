#=============================================================================
#
#  Script name: LHS000404.R
#
#  Date: 2025-10-17
#  
#  Description: Script to analyze LHS000403 imputed datasets and produce
#               beta estimates of the model with neonatal mortality as response.
#
#   Input: data/derived/LHS000403_&i.rda (i = 1-250)
#
#   Output: results/LHS000404_&i.rda 
#
#=============================================================================

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

# Load required packages
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(broom)
  library(mice)
})

# load source functions
source("scripts/LHS000490/LHS000490.R")

# Load imputed synthetic datasets
load(file = sprintf("data/derived/LHS000403/LHS000403_%d.rda", task_id))

# Extracts all the imputed dataset
completed_data <- mice::complete(imp, "long") %>%  
                    mutate(indexA = .imp,
                           indexL = floor((IMPL_ - 1) / 5) + 1,
                           indexB = IMPL_ - (indexL - 1) * 5) %>%
                    mutate(Index = paste0(indexA, "_", indexL, "_", indexB)) %>%
                    select(-c(IMPL_,OBS_,.id,.imp))

# Split datasets by .imp variable
imputed_datasets <- split(completed_data, completed_data$Index)

# remove dataframes to free up memory
rm(completed_data,imp)

# Estimate the model for each Boostraped X Polya's draw X imputed dataset
betas <- imputed_datasets %>%
  imap_dfr(~ {
    # Relevel year of birth only
    data_clean <- .x %>%
      mutate(yob = relevel(as.factor(yob), ref = "2010"),
             meduc_c3 = relevel(as.factor(meduc_c3), ref = "No education"))
    
    # Fit the logistic regression model
    model <- glm(
      neonatal ~ yob + urban + child_sex + 
        bord_spacing_c5 + preterm_birth + mage_birth_c3 + meduc_c3 +
        wealth_index_c3 + underwt_birth + ethnicity_c3 + any_substance_use + 
        anc_quality_score_c2,
      data = data_clean,
      family = binomial(link = "logit")
    )
    
    # Extract coefficients
    broom::tidy(model) %>%
      mutate(Index = .y)
  }) %>%
  tidyr::separate(Index, into = c("indexA", "indexL", "indexB"), sep = "_", convert = TRUE)

# Process dataset to contain only relevant variables
final_betas <- betas %>% as_tibble() %>% 
  select(term, estimate, indexA, indexL, indexB)

# Save beta estimates
save(final_betas, file = sprintf("data/derived/LHS000404/LHS000404_%d.rda", task_id))
