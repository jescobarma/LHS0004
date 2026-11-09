#============================================================================
#
#   Program: LHS000410.R
#
#   Description: Delta-adjustment (MNAR) sensitivity analysis of the MAR
#                assumption for birth weight under FPBB MI: one IMPL_ subset is imputed
#                exactly as in LHS000403, except that missing underwt_birth is
#                drawn with delta added to the logit of P(low birth weight).
#                The 5 completed datasets are then fitted with standard
#                logistic regression (mother's education collapsed to
#                "No education/Primary" vs "Secondary or higher", as in
#                LHS000408), and only the coefficients are combined later
#                (LHS000412).
#
#   Usage: Rscript LHS000410.R <subset id> <delta> [maxit, default 50]
#
#   Input:  data/derived/LHS000402_subsets/LHS000402_&i.rda (FPBB synthetic population subset)
#
#   Output: data/derived/LHS000410/delta<delta>/LHS000410_&i.rda
#           (imp: mice object; betas: logistic coefficients)
#
#============================================================================

args <- commandArgs(trailingOnly = TRUE)
task_id <- as.integer(args[1])
delta   <- as.numeric(args[2])
maxit   <- if (length(args) >= 3) as.integer(args[3]) else 50

suppressPackageStartupMessages({
  library(mice)
  library(dplyr)
  library(purrr)
  library(tibble)
})

load(sprintf("data/derived/LHS000402_subsets/LHS000402_%d.rda", task_id))
source("scripts/LHS000490/LHS000490.R")
ds <- apply_formats(ds, formats_LHS0004)

# mice's logreg (Bayesian logistic draw) with delta added to the linear predictor of the
# missing cases; mice finds it by name and passes delta through 'blots'
mice.impute.logreg_delta <- function(y, ry, x, wy = NULL, delta = 0, ...) {
  if (is.null(wy)) wy <- !ry
  x <- cbind(1, as.matrix(x))
  fit <- suppressWarnings(glm.fit(x[ry, , drop = FALSE], as.integer(y[ry]) - 1L,
                                  family = binomial(link = "logit")))
  covariance <- summary.glm(fit)$cov.unscaled
  beta_star <- coef(fit) + t(chol((covariance + t(covariance)) / 2)) %*% rnorm(ncol(x))
  p <- plogis(x[wy, , drop = FALSE] %*% beta_star + delta)
  factor(levels(y)[1 + (runif(length(p)) <= p)], levels = levels(y))
}

# --- Imputation as in LHS000403, with the delta shift on birth weight ---
init_method <- make.method(ds)
init_method["bord_spacing_c5"] <- "polyreg"
init_method[c("any_substance_use", "anc_quality_score_c2")] <- "logreg"
init_method["underwt_birth"] <- "logreg_delta"

pred_matrix <- make.predictorMatrix(ds)
pred_matrix[, c("OBS_", "IMPL_", "psu", "weight", "strata", "region")] <- 0

imp <- mice(ds, m = 5, method = init_method, predictorMatrix = pred_matrix,
            blots = list(underwt_birth = list(delta = delta)),
            maxit = maxit, printFlag = FALSE, seed = 123)

# --- Logistic fits on each completed dataset, as in LHS000404 (logistic arm) ---
model_formula <- neonatal ~ yob + urban + child_sex + bord_spacing_c5 + preterm_birth +
  mage_birth_c3 + meduc_c3 + wealth_index_c3 + underwt_birth + ethnicity_c3 +
  any_substance_use + anc_quality_score_c2

prepare <- function(data) {
  data %>% mutate(yob = relevel(as.factor(yob), ref = "2010"),
                  meduc_c3 = factor(if_else(meduc_c3 == "Secondary or higher",
                                            "Secondary or higher", "No education/Primary"),
                                    levels = c("No education/Primary", "Secondary or higher")))
}

betas <- map_dfr(seq_len(imp$m), function(a) {
  data <- prepare(complete(imp, a))
  fit <- glm(model_formula, data = data, family = binomial(link = "logit"))
  if (!fit$converged) stop("fit did not converge")
  enframe(coef(fit), name = "term", value = "estimate") %>%
    mutate(delta = delta, indexA = a,
           indexL = floor((task_id - 1) / 5) + 1,
           indexB = task_id - (floor((task_id - 1) / 5)) * 5)
})

outfile <- sprintf("data/derived/LHS000410/delta%s/LHS000410_%d.rda", format(delta), task_id)
dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
save(imp, betas, file = outfile, compress = "xz")
