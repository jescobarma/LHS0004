#=============================================================================
#
#  Script name: LHS000404.R
#  
#  Author: Alvaro Quijano
#
#  Date: 2025-10-17
#  
#  Description: Script to analyze LHS000403 imputed datasets and produce
#               beta estimates of the model with neonatal mortality as response.
#
#   Input: data/derived/LHS000403_&i.rda (i = 1-50)
#
#   Output: results/LHS000404.rda 
#
#=============================================================================

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

suppressPackageStartupMessages({
    # load libraries
    library(tidyverse)
    library(dplyr)
    library(mice)
})

# load source functions
source("scripts/LHS000490/LHS000490.R")

# Load imputed synthetic datasets
load(file = "data/derived/LHS000403_1.rda")

# Extracts all the imputed dataset
completed_data <- mice::complete(imp, "long")

# Apply formats to the completed data
formats <- list(
  urban = fmt_urban,
  child_sex = fmt_child_sex,
  neonatal = fmt_binary,
  infant = fmt_binary,
  birth_order_spacing_c5 = fmt_birth_order_spacing_c5,
  preterm_birth = fmt_binary,
  mage_birth_c3 = fmt_mage_birth_c3,
  meduc_c3 = fmt_meduc_c3,
  ethnicity_c3 = fmt_ethnicity_c3,
  wealth_index_c3 = fmt_wealth_index_c3
)
completed_data  <- apply_formats(completed_data, formats)

# Split datasets by .imp variable
imputed_datasets <- split(completed_data, completed_data$.imp)

# remove dataframes to free up memory
rm(completed_data,imp)

# Estimate the model for each imputed dataset
coef_list  <- map(imputed_datasets, function(data) {
  model <- glm(neonatal ~ factor(yob) + urban + child_sex + 
                birth_order_spacing_c5 +  preterm_birth + mage_birth_c3 + meduc_c3 +
                wealth_index_c3 + underwt_birth + ethnicity_c3 + any_substance_use + 
                anc_quality_score_c2, 
                data=data %>% filter(yob < 2016), 
                family=binomial(link="logit"))
    return(coef(summary(model)))                
})

# Combine results into a single data frame
betas <- do.call(rbind, lapply(seq_along(coef_list), function(i) {
  cbind(indexL = i, coef_list[[i]])
}))

# Process dataset to contain only relevant variables
final_betas <- betas %>% as_tibble() %>% mutate(Parm = rownames(betas)) %>%
  select(Parm, indexL, Estimate)

# Save beta estimates
save(final_betas, file = file = sprintf("data/derived/LHS000404_%d.rda", task_id))
