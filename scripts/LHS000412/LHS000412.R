#=============================================================================
#
#  Script name: LHS000412.R
#
#  Date: 2026-10-23
#
#  Description: Combine the delta-adjusted FPBB logistic fits for the three
#               shifted variables (birth weight, LHS000410; antenatal care
#               quality and substance use, LHS000411) using Zhou et al (2016),
#               adding delta = 0 (MAR) from LHS000405. Reports the largest
#               relative change of each OR from its MAR value across delta,
#               and plots each shifted variable's own OR (95% CI) against delta.
#
#   Usage: Rscript LHS000412.R [n subsets per delta, default 500]
#
#   Input: data/derived/LHS000410/delta&d/LHS000410_&i.rda
#          data/derived/LHS000411/<variable>/delta&d/LHS000411_&i.rda
#          (d = -1, -0.5, 0.5, 1; i = 1-500)
#          data/derived/LHS000405.rda (logistic_fpbb, MAR estimates)
#
#   Output: outputs/LHS000412.csv
#           outputs/LHS000412_change.csv
#           imgs/LHS000412_delta.pdf
#           data/derived/LHS000412.rda
#
#=============================================================================

args <- commandArgs(trailingOnly = TRUE)
n_files <- if (length(args) >= 1) as.integer(args[1]) else 500

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
})

deltas <- c(-1, -0.5, 0.5, 1)
variables <- c("underwt_birth", "anc_quality_score_c2", "any_substance_use")
shifted_terms <- c(underwt_birth = "underwt_birthYes", anc_quality_score_c2 = "anc_quality_score_c2High",
                   any_substance_use = "any_substance_useYes")
shifted_labels <- c(underwt_birth = "Low birth weight", anc_quality_score_c2 = "High antenatal care quality",
                    any_substance_use = "Substance use during pregnancy")

delta_files <- function(variable, delta) {
  if (variable == "underwt_birth") {
    sprintf("data/derived/LHS000410/delta%s/LHS000410_%d.rda", format(delta), seq_len(n_files))
  } else {
    sprintf("data/derived/LHS000411/%s/delta%s/LHS000411_%d.rda", variable, format(delta), seq_len(n_files))
  }
}

betas_all <- expand_grid(variable = variables, delta = deltas) %>%
  pmap_dfr(function(variable, delta) {
    files <- delta_files(variable, delta)
    stopifnot(all(file.exists(files)))
    map_dfr(files, function(f) {
      load(f)   # loads 'imp' and 'betas'
      betas %>% mutate(variable = variable)
    })
  })

# Reference: Zhou, H., Elliott, M. R., & Raghunathan, T. E. (2016). Synthetic multiple-imputation
# procedure for multistage complex samples. Journal of Official Statistics, 32(1), 231-256.
per_l <- betas_all %>%
  group_by(variable, delta, term, indexL) %>%
  summarise(Q_l = mean(estimate), n_fits = n(), .groups = "drop")

# Degrees of freedom min(design df = 3919 PSUs - 63 strata, L - 1), as in LHS000405
df_com_design <- 3919L - 63L
df_zhou <- min(df_com_design, n_distinct(per_l$indexL) - 1)

fpbb_delta <- per_l %>%
  group_by(variable, delta, term) %>%
  summarise(estimate = mean(Q_l), variance = (1 + 1 / n_distinct(indexL)) * var(Q_l),
            df = df_zhou, .groups = "drop")

load("data/derived/LHS000405.rda")   # logistic_fpbb (delta = 0, MAR estimates)

# Align logistic_fpbb to fpbb_delta column names before binding (Estimate -> estimate,
# se^2 -> variance), with the same df as the delta fits (L = 100 in LHS000405 too).
mar_fpbb <- expand_grid(variable = variables, logistic_fpbb) %>%
  rename(estimate = Estimate) %>%
  mutate(delta = 0, variance = se^2, df = df_zhou) %>%
  select(variable, delta, term, estimate, variance, df)

estimates <- bind_rows(fpbb_delta, mar_fpbb) %>%
  mutate(se = sqrt(variance), t = qt(0.975, df),
         OR = exp(estimate), OR_LL = exp(estimate - t * se), OR_UL = exp(estimate + t * se)) %>%
  select(variable, delta, term, OR, OR_LL, OR_UL)

# Largest relative change of each OR from its MAR value, over delta in [-1, 1]
change <- estimates %>%
  filter(term != "(Intercept)") %>%
  group_by(variable, term) %>%
  summarise(max_rel_change = max(abs(OR / OR[delta == 0] - 1)), .groups = "drop") %>%
  arrange(variable, desc(max_rel_change))

save(estimates, change, file = "data/derived/LHS000412.rda")

# --- Table: one row per (shifted variable, parameter), one column per delta ---
table_final <- estimates %>%
  arrange(delta) %>%
  mutate(value = sprintf("%.2f (%.2f, %.2f)", OR, OR_LL, OR_UL),
         delta = sprintf("delta = %.1f OR (95%% CI)", delta),
         variable = factor(variable, levels = variables)) %>%
  select(Shifted = variable, Parameter = term, delta, value) %>%
  pivot_wider(names_from = delta, values_from = value) %>%
  arrange(Shifted)

write.csv(table_final, file = "outputs/LHS000412.csv", row.names = FALSE)
write.csv(change, file = "outputs/LHS000412_change.csv", row.names = FALSE)
print(as.data.frame(change), row.names = FALSE)

# --- Plot: each shifted variable's own OR (95% CI) against delta, one panel per shifted variable ---
own_estimates <- estimates %>%
  filter(term == shifted_terms[variable]) %>%
  mutate(shifted = factor(shifted_labels[variable], levels = shifted_labels))
stopifnot(nrow(own_estimates) == 15)

or_breaks <- c(0.5, 1, 2, 5, 10, 20)

delta_plot <- ggplot(own_estimates, aes(x = delta, y = OR)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.35, colour = "grey35") +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.35, colour = "grey35") +
  geom_ribbon(aes(ymin = OR_LL, ymax = OR_UL), fill = "#0B4F8A", alpha = 0.15) +
  geom_line(colour = "#0B4F8A", linewidth = 0.45) +
  geom_pointrange(aes(ymin = OR_LL, ymax = OR_UL), colour = "#0B4F8A", size = 0.25, linewidth = 0.45) +
  facet_wrap(~ shifted, nrow = 1) +
  scale_x_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), labels = c("−1", "−0.5", "0", "0.5", "1")) +
  scale_y_log10(breaks = or_breaks, labels = as.character(or_breaks)) +
  labs(x = expression(delta), y = "Odds ratio (95% CI, log scale)") +
  theme_classic(base_size = 9, base_family = "serif") +
  theme(axis.text = element_text(colour = "black", size = 8),
        axis.title = element_text(size = 10),
        strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing.x = unit(12, "pt"),
        panel.border = element_rect(fill = NA, colour = "grey35", linewidth = 0.35),
        plot.margin = margin(4, 8, 4, 2))

ggsave("imgs/LHS000412_delta.pdf", delta_plot, width = 8.45, height = 3.2, device = cairo_pdf)
