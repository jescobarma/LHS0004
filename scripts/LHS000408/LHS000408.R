#=============================================================================
#
#  Script name: LHS000408.R
#
#  Date: 2026-10-21
#
#  Description: Logistic regression of neonatal mortality with mother's
#               education collapsed to "No education/Primary" (the reference)
#               vs "Secondary or higher", compared across three ways of
#               handling the missing data:
#                 FPBB MI          - unweighted glm on the LHS000403 imputations,
#                                    combined as in LHS000405 (Zhou et al 2016);
#                 complete-case    - svyglm on the original DHS data;
#                 conventional MI  - mice on the original DHS data (design
#                                    ignored in the imputation model), svyglm
#                                    on each imputation, combined with Rubin's
#                                    rules (Barnard-Rubin degrees of freedom).
#               svyglm uses Taylor linearisation for the design-based variances.
#               Education is collapsed after imputation, so LHS000403 is reused;
#               the FPBB fits run in parallel on forked cores. This script draws
#               the three-method forest plot itself.
#
#   Usage: Rscript LHS000408.R [n imputations, default 5] [n FPBB files, default 500]
#                              [n cores, default 1]
#
#   Input: data/derived/LHS000401.rda
#          data/derived/LHS000403/LHS000403_&i.rda (i = 1-500)
#
#   Output: imgs/LHS000408_forest.pdf
#           data/derived/LHS000408.rda (logit_fpbb, complete_case, conventional_mi)
#
#=============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_imputations <- if (length(args) >= 1) as.integer(args[1]) else 5
n_files       <- if (length(args) >= 2) as.integer(args[2]) else 500
n_cores       <- if (length(args) >= 3) as.integer(args[3]) else 1

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(mice)
  library(survey)
  library(parallel)
})

source("scripts/LHS000490/LHS000490.R")

# LHS000401.rda is stored with numeric codes; label it as LHS000403 does for the FPBB data
load("data/derived/LHS000401.rda")
LHS000401 <- apply_formats(LHS000401, formats_LHS0004)

model_formula <- neonatal ~ yob + urban + child_sex + bord_spacing_c5 + preterm_birth +
  mage_birth_c3 + meduc_c3 + wealth_index_c3 + underwt_birth + ethnicity_c3 +
  any_substance_use + anc_quality_score_c2

prepare <- function(data) {
  data %>% mutate(yob = relevel(as.factor(yob), ref = "2010"),
                  meduc_c3 = factor(if_else(meduc_c3 == "Secondary or higher",
                                            "Secondary or higher", "No education/Primary"),
                                    levels = c("No education/Primary", "Secondary or higher")))
}

# mclapply returns worker errors as values (NULL if a worker is killed), so they are raised here
par_map <- function(x, f) {
  out <- mclapply(x, f, mc.cores = n_cores)
  failed <- map_lgl(out, ~ is.null(.x) || inherits(.x, "try-error"))
  if (any(failed)) stop("parallel task ", which(failed)[1], " failed: ", out[[which(failed)[1]]])
  out
}

svy_logistic <- function(design) {
  fit <- svyglm(model_formula, design = design, family = quasibinomial(link = "logit"))
  if (!fit$converged) stop("svyglm fit did not converge")
  tibble(term = names(coef(fit)), estimate = unname(coef(fit)), variance = diag(vcov(fit)))
}

odds_ratios <- function(est) {
  est %>% mutate(se = sqrt(variance), t = qt(0.975, df),
                 OR = exp(estimate), OR_LL = exp(estimate - t * se), OR_UL = exp(estimate + t * se))
}

# --- FPBB MI: unweighted glm on the synthetic populations (LHS000404) ---
fpbb_betas <- bind_rows(par_map(seq_len(n_files), function(i) {
  load(sprintf("data/derived/LHS000403/LHS000403_%d.rda", i))
  complete(imp, "long") %>%
    mutate(indexA = .imp,
           indexL = floor((IMPL_ - 1) / 5) + 1,
           indexB = IMPL_ - (indexL - 1) * 5) %>%
    group_by(indexA, indexL, indexB) %>%
    group_modify(~ {
      fit <- glm(model_formula, data = prepare(.x), family = binomial(link = "logit"))
      if (!fit$converged || anyNA(coef(fit))) stop("glm fit failed")
      enframe(coef(fit), name = "term", value = "estimate")
    }) %>%
    ungroup()
}))

# Combined as in LHS000405. Reference: Zhou, H. et al (2014).
# Synthetic multiple-imputation procedure for multistage complex samples. JOS, 32(1), 231-256.

n_L <- n_distinct(fpbb_betas$indexL)
fpbb_per_l <- fpbb_betas %>%
  group_by(term, indexL) %>%
  summarise(Q_l = mean(estimate), n_fits = n(), .groups = "drop")
stopifnot(all(fpbb_per_l$n_fits == 25))   # 5 B x 5 A per L

logit_fpbb <- fpbb_per_l %>%
  group_by(term) %>%
  summarise(estimate = mean(Q_l), variance = (1 + 1 / n_L) * var(Q_l), .groups = "drop") %>%
  mutate(df = n_L - 1)

# --- Complete-case analysis ---
design <- svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                    data = prepare(LHS000401), nest = TRUE)
df_design <- degf(design)

is_complete <- complete.cases(LHS000401[all.vars(model_formula)])
complete_case <- svy_logistic(subset(design, is_complete)) %>%
  mutate(df = df_design)

# --- Conventional MI, design ignored in the imputation model ---
predictors <- make.predictorMatrix(LHS000401)
predictors[, c("region", "strata", "psu", "weight")] <- 0
methods <- make.method(LHS000401)
methods["bord_spacing_c5"] <- "polyreg"
methods[c("underwt_birth", "any_substance_use", "anc_quality_score_c2")] <- "logreg"

imp <- mice(LHS000401, m = n_imputations, method = methods, predictorMatrix = predictors,
            maxit = 50, printFlag = FALSE, seed = 123)

conventional_mi <- map_dfr(seq_len(n_imputations), function(k) {
  svy_logistic(svydesign(ids = ~psu, strata = ~strata, weights = ~weight,
                         data = prepare(complete(imp, k)), nest = TRUE))
}) %>%
  group_by(term) %>%
  summarise(between = var(estimate), estimate = mean(estimate), within = mean(variance),
            .groups = "drop") %>%
  mutate(variance = within + (1 + 1 / n_imputations) * between,
         lambda   = (1 + 1 / n_imputations) * between / variance,
         df_old   = (n_imputations - 1) / lambda^2,
         df_obs   = (df_design + 1) / (df_design + 3) * df_design * (1 - lambda),
         df       = df_old * df_obs / (df_old + df_obs)) %>%
  select(term, estimate, variance, df)

cat("Complete cases:", sum(is_complete), "of", nrow(LHS000401), "\n")

# Pooled estimates for the LHS000409 table, so it does not refit
save(logit_fpbb, complete_case, conventional_mi, file = "data/derived/LHS000408.rda")

method_labels <- c("FPBB MI", "Complete-case", "Conventional MI")

comparison <- bind_rows(
  `FPBB MI`         = odds_ratios(logit_fpbb) %>% select(term, OR, OR_LL, OR_UL),
  `Complete-case`   = odds_ratios(complete_case) %>% select(term, OR, OR_LL, OR_UL),
  `Conventional MI` = odds_ratios(conventional_mi) %>% select(term, OR, OR_LL, OR_UL),
  .id = "method") %>%
  mutate(method = factor(method, levels = method_labels))

# --- Forest plot ---
layout <- tribble(
  ~group,         ~variable,                        ~level,                           ~term,
  "Birth cohort", "Year of birth",                  "2010",                           NA,
  "Birth cohort", "Year of birth",                  "2011",                           "yob2011",
  "Birth cohort", "Year of birth",                  "2012",                           "yob2012",
  "Birth cohort", "Year of birth",                  "2013",                           "yob2013",
  "Birth cohort", "Year of birth",                  "2014",                           "yob2014",
  "Birth cohort", "Year of birth",                  "2015",                           "yob2015",
  "Child",        "Child sex",                      "Male",                           NA,
  "Child",        "Child sex",                      "Female",                         "child_sexFemale",
  "Child",        "Birth weight",                   "Normal",                         NA,
  "Child",        "Birth weight",                   "Low",                            "underwt_birthYes",
  "Child",        "Preterm birth",                  "No",                             NA,
  "Child",        "Preterm birth",                  "Yes",                            "preterm_birthYes",
  "Child",        "Birth order and\nspacing",      "Firstborn",                      NA,
  "Child",        "Birth order and\nspacing",      "2–3, <36 months",                "bord_spacing_c52–3 (<36 months apart)",
  "Child",        "Birth order and\nspacing",      "2–3, ≥36 months",                "bord_spacing_c52–3 (≥36 months apart)",
  "Child",        "Birth order and\nspacing",      "4+, <36 months",                 "bord_spacing_c5>3 (<36 months apart)",
  "Child",        "Birth order and\nspacing",      "4+, ≥36 months",                 "bord_spacing_c5>3 (≥36 months apart)",
  "Mother",       "Mother's age at birth",          "20–34 years",                    NA,
  "Mother",       "Mother's age at birth",          "≤19 years",                      "mage_birth_c3≤19 years",
  "Mother",       "Mother's age at birth",          "≥35 years",                      "mage_birth_c3≥35 years",
  "Mother",       "Maternal education",             "No education/Primary",           NA,
  "Mother",       "Maternal education",             "Secondary or higher",            "meduc_c3Secondary or higher",
  "Mother",       "Maternal ethnicity",             "Non-Indigenous/\nNon-Afrodescendant", NA,
  "Mother",       "Maternal ethnicity",             "Afrodescendant",                 "ethnicity_c3Afro-descendant",
  "Mother",       "Maternal ethnicity",             "Indigenous",                     "ethnicity_c3Indigenous",
  "Mother",       "Antenatal care quality",         "Low/Moderate",                   NA,
  "Mother",       "Antenatal care quality",         "High",                           "anc_quality_score_c2High",
  "Mother",       "Substance use during\npregnancy", "No",                             NA,
  "Mother",       "Substance use during\npregnancy", "Yes",                            "any_substance_useYes",
  "Household",    "Wealth index",                   "Wealthier/Wealthiest",           NA,
  "Household",    "Wealth index",                   "Middle",                         "wealth_index_c3Middle",
  "Household",    "Wealth index",                   "Poorer/Poorest",                 "wealth_index_c3Poor",
  "Household",    "Place of residence",             "Rural",                          NA,
  "Household",    "Place of residence",             "Urban",                          "urbanUrban"
)
stopifnot(setequal(c("(Intercept)", na.omit(layout$term)), unique(comparison$term)))

# A heading row per group and per variable, then the levels; y runs down each column.
# Two columns (birth cohort and child | mother and household); the column offset keeps
# y unique so each panel's free y scale picks up only its own labels. Plotmath has no
# line breaks, so a label with "\n" continues on an extra row.
column_offset <- 100
rows <- layout %>%
  mutate(group = factor(group, levels = unique(group)),
         variable = factor(variable, levels = unique(variable))) %>%
  group_by(group, variable) %>%
  group_modify(~ bind_rows(tibble(level = NA_character_, term = NA_character_), .x)) %>%
  group_by(group) %>%
  group_modify(~ bind_rows(tibble(variable = NA, level = NA_character_, term = NA_character_), .x)) %>%
  ungroup() %>%
  mutate(style = case_when(is.na(variable) ~ "group", is.na(level) ~ "variable",
                           is.na(term) ~ "reference", TRUE ~ "level"),
         text = case_when(style == "group"     ~ toupper(group),
                          style == "variable"  ~ as.character(variable),
                          style == "reference" ~ paste(level, "(ref.)"),
                          TRUE                 ~ level)) %>%
  separate_longer_delim(text, "\n") %>%
  mutate(column = if_else(group %in% c("Birth cohort", "Child"), 1L, 2L)) %>%
  group_by(column) %>%
  mutate(y = -row_number() - (column - 1) * column_offset) %>%
  ungroup() %>%
  mutate(label = case_when(style == "group"     ~ sprintf('bold("%s")', text),
                           style == "variable"  ~ sprintf('bold("  %s")', text),
                           style == "reference" ~ sprintf('italic("      %s")', text),
                           TRUE                 ~ sprintf('"      %s"', text)),
         label = gsub("≥", '" >= "', gsub("≤", '" <= "', label)))

group_bands <- rows %>%
  filter(is.na(variable)) %>%
  transmute(column, ymin = y - 0.5, ymax = y + 0.5)

variable_bands <- rows %>%
  filter(!is.na(variable)) %>%
  group_by(column, variable) %>%
  summarise(ymin = min(y) - 0.5, ymax = max(y) + 0.5, .groups = "drop") %>%
  filter(as.integer(variable) %% 2 == 0)

reference_points <- rows %>%
  filter(style == "reference", grepl("(ref.)", text, fixed = TRUE)) %>%
  select(column, y)

n_column_rows <- max(count(rows, column)$n)
column_span <- tibble(column = rep(1:2, each = 2),
                      y = -(column - 1) * column_offset + rep(c(-0.5, -n_column_rows - 0.5), 2))

figure_methods <- c(`Complete-case` = "Complete-case analysis", `FPBB MI` = "FPBB-MI",
                    `Conventional MI` = "Conventional MI")
method_offset <- c(0.24, 0, -0.24)

estimates <- comparison %>%
  inner_join(rows %>% select(term, column, y), by = "term") %>%
  mutate(method = factor(figure_methods[as.character(method)], levels = figure_methods),
         y = y + method_offset[as.integer(method)])

or_breaks <- c(0.05, 0.2, 1, 5, 20, 100)
x_limits <- range(estimates$OR_LL, estimates$OR_UL) * c(0.8, 1.25)

# Produce the plot (grey background for the group and variable bands, a dashed vertical line at OR = 1)
forest_plot <- ggplot(estimates, aes(y = y)) +
  geom_rect(data = variable_bands, aes(xmin = x_limits[1], xmax = x_limits[2], ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey95") +
  geom_rect(data = group_bands, aes(xmin = x_limits[1], xmax = x_limits[2], ymin = ymin, ymax = ymax),
            inherit.aes = FALSE, fill = "grey82") +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.35, colour = "grey35") +
  geom_linerange(aes(xmin = OR_LL, xmax = OR_UL, colour = method), linewidth = 0.45) +
  geom_point(aes(x = OR, colour = method, shape = method), size = 1.4) +
  geom_point(data = reference_points, aes(x = 1, y = y), inherit.aes = FALSE, size = 1.4) +
  geom_blank(data = column_span) +
  facet_wrap(~ column, ncol = 2, scales = "free_y") +
  scale_x_log10(breaks = or_breaks, labels = as.character(or_breaks),
                limits = x_limits, expand = expansion(0)) +
  scale_y_continuous(breaks = rows$y, labels = parse(text = rows$label), expand = expansion(0)) +
  scale_colour_manual(values = c("grey45", "#0B4F8A", "#D55E00")) +
  scale_shape_manual(values = c(15, 16, 17)) +
  labs(x = "Odds ratio (95% confidence interval, log scale)", y = NULL, colour = NULL, shape = NULL) +
  theme_classic(base_size = 9, base_family = "serif") +
  theme(axis.text.y = element_text(hjust = 0, colour = "black", size = 9.5),
        axis.text.x = element_text(colour = "black", size = 7),
        strip.text = element_blank(),
        strip.background = element_blank(),
        panel.spacing.x = unit(10, "pt"),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_line(linewidth = 0.35),
        panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.35),
        legend.position = "top",
        legend.text = element_text(size = 12.75),
        legend.key.width = unit(14, "pt"),
        legend.margin = margin(0, 0, 0, 0),
        plot.margin = margin(4, 8, 4, 2))

# Save the plot to pdf file
ggsave("imgs/LHS000408_forest.pdf", forest_plot, width = 8.45, height = 5.9, device = cairo_pdf)
