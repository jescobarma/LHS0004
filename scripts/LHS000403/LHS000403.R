#============================================================================
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
#   Output: data/derived/LHS000403_&i.rda (Imputed datasets)
#
#============================================================================

# Retrieve command-line arguments
args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])

suppressPackageStartupMessages({
  library(mice)
  library(dplyr)
})

# load data from step #2 and source formats
load("data/derived/LHS000402.rda")
source("scripts/LHS000490/LHS000490.R")
colnames(ds)
# --- Subset only the relevant IMPL_ for this job ---
ds <- ds %>% filter(IMPL_ == task_id)

# Apply formats to the dataset
ds <- apply_formats(ds, formats_LHS0004)

# --- Define imputation methods ---
init_method <- make.method(ds)
init_method["bord_spacing_c5"] <- "polyreg"
init_method[c("underwt_birth","any_substance_use","anc_quality_score_c2")] <- "logreg"

# --- Define predictor matrix and exclude design variables ---
pred_matrix <- make.predictorMatrix(ds)
pred_matrix[, c("OBS_","IMPL_","psu","weight","strata","infant","region")] <- 0

# --- Start imputation ---
message("Imputing subset ", task_id)

imp <- mice(
  data = ds,
  m = 5,                    # number of imputations
  method = init_method,
  predictorMatrix = pred_matrix,
  maxit = 5,                # iterations
  printFlag = FALSE,
  seed = 123
)

# --- Save output ---
outfile <- sprintf("data/derived/LHS000403/LHS000403_%d.rda", task_id)
save(imp, file = outfile, compress = "xz")

message("Subset ", task_id, " imputation completed and saved to ", outfile)

# --- Clean up ---
rm(ds, imp, init_method, pred_matrix)