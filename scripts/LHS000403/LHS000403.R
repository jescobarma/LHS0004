##########################################################
#
#   Program: LHS000403.R
#
#   Description: Multiple imputation by subset (IMPL_)
#
#   Author: Álvaro Quijano
#
#   Version: R 4.5.1
#
#  ----------------------------------------------------------------
#
#   Input:  data/derived/LHS000402.rda (FPBB synthetic population)
# 
#   Output: data/derived/LHS000403.rda (Imputed datasets)
#
###########################################################

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

suppressPackageStartupMessages({
  library(mice)
  library(dplyr)
})

# load data from step #2
load("data/derived/LHS000402.rda")

# type conversions
ds <- ds %>%
  mutate(
    birth_order_spacing_c5 = ordered(birth_order_spacing_c5),
    underwt_birth = factor(underwt_birth, levels = c(0,1), labels = c("No","Yes")),
    any_substance_use = factor(any_substance_use, levels = c(0,1), labels = c("No","Yes")),
    anc_quality_score_c2 = factor(anc_quality_score_c2, levels = c(0,1), labels = c("Low","High"))
  )

# Establish an imputation method for each variable 
init_method <- make.method(ds)
init_method["birth_order_spacing_c5"] <- "polr"
init_method[c("underwt_birth","any_substance_use","anc_quality_score_c2")] <- "logreg"

# Exclude irrelevant predictors from the model
pred_matrix <- make.predictorMatrix(ds)
pred_matrix[, c("OBS_","IMPL_","psu","weight","region","strata")] <- 0

# Split data by IMPL_ (implicated dataset)
split_data <- split(ds, ds$IMPL_)
data_subset <- split_data[[task_id]]
rm(ds, split_data); gc()

# Print a message to indicate the start of the imputation.
message("Imputing subset ", task_id)

# Run multiple imputation in each subset
imp <- mice(
  data = data_subset,
  m = 5,
  method = init_method,
  predictorMatrix = pred_matrix,
  maxit = 10,
  printFlag = FALSE,
  seed = 123
)

# Save files and print a message of completion
save(imp, file = sprintf("data/derived/LHS000403_%d.rda", task_id), compress = "xz")
message("Subset ", task_id, " imputation completed.")
