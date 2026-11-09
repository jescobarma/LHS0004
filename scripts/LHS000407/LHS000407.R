#=============================================================================
#
#  Script name: LHS000407.R
#
#  Date: 2026-10-22
#
#  Description: Convergence diagnostics of the FPBB MI imputations (supplement):
#               trace plots, Gelman-Rubin R-hat, and drift of chain means.
#
#   Usage: Rscript scripts/LHS000407/LHS000407.R [n FPBB files, default 500]
#
#   Input: data/derived/LHS000403/LHS000403_&i.rda (i = 1-500)
#
#   Output: imgs/LHS000407_convergence.pdf
#
#=============================================================================

# Read the number of FPBB files to diagnose (default 500).
args <- commandArgs(trailingOnly = TRUE)
n_files <- if (length(args) >= 1) as.integer(args[1]) else 500

# Load required packages.
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(coda)
})

# Imputed variables in the LHS000418 table order, with display labels; post burn-in window is iterations 11-50.
variable_labels <- c(underwt_birth = "Low birth weight (<2,500 g)", bord_spacing_c5 = "Birth order and spacing",
                     anc_quality_score_c2 = "Antenatal care quality",
                     any_substance_use = "Substance use during pregnancy")
window <- 11:50

# Pull the per-iteration chain means/variances (and the imputation method) for the imputed variables
chains <- function(i) {
  load(sprintf("data/derived/LHS000403/LHS000403_%d.rda", i))
  list(mean = imp$chainMean[names(variable_labels), , , drop = FALSE],
       var = imp$chainVar[names(variable_labels), , , drop = FALSE],
       method = imp$method[names(variable_labels)])
}

# For one population, compute per-variable Gelman-Rubin R-hat (coda) and standardised drift of the chain means over the window.
diagnose <- function(i) {
  ch <- chains(i)
  map_dfr(names(variable_labels), function(v) {
    means <- ch$mean[v, window, ]   # iterations x chains
    mcmc_list <- as.mcmc.list(lapply(seq_len(ncol(means)), function(j) as.mcmc(means[, j])))
    tibble(population = i, variable = v,
           rhat_classic = coda::gelman.diag(mcmc_list, autoburnin = FALSE)$psrf[1, 1],
           std_drift = mean(apply(means, 2, function(y) coef(lm(y ~ window))[2])) /
             sqrt(mean(ch$var[v, window, ])))
  })
}

# Run the diagnostics across all synthetic populations.
diagnostics <- map_dfr(seq_len(n_files), diagnose)

# --- Figure ---
# Panel A traces come from population 1; shift binary (logreg) means from the 1/2 coding to a 0/1 proportion.
example <- chains(1)
binary <- names(which(example$method == "logreg"))
example$mean[binary, , ] <- example$mean[binary, , ] - 1

# Reshape chain means and SDs
traces <- bind_rows(
  as.data.frame.table(example$mean, responseName = "value") %>% mutate(statistic = "Mean"),
  as.data.frame.table(sqrt(example$var), responseName = "value") %>% mutate(statistic = "Standard deviation")) %>%
  setNames(c("variable", "iteration", "chain", "value", "statistic")) %>%
  mutate(iteration = as.integer(iteration),
         chain = factor(as.integer(chain)),
         variable = factor(variable_labels[as.character(variable)], levels = variable_labels),
         statistic = case_when(statistic == "Standard deviation" ~ "standard deviation",
                               variable == variable_labels[["bord_spacing_c5"]] ~ "mean category code (1–5)",
                               TRUE ~ "proportion"),
         panel = factor(paste0(sub(" (during|\\()", "\n\\1", variable), ":\n", statistic)))
trace_panels <- traces %>% distinct(variable, statistic, panel) %>%
  arrange(statistic == "standard deviation", variable) %>% pull(panel) %>% as.character()
traces <- traces %>% mutate(panel = factor(panel, levels = trace_panels))

# Widen each panel's y range 1.5x around its midpoint so free scales do not magnify Monte Carlo noise.
trace_limits <- traces %>%
  group_by(panel) %>%
  summarise(centre = (max(value) + min(value)) / 2, half_width = 1.5 * (max(value) - min(value)) / 2) %>%
  reframe(value = c(centre - half_width, centre + half_width), .by = panel)

chain_colours <- c("#0B4F8A", "#D55E00", "#009E73", "#CC79A7", "grey35")

# Panel A: per-chain trace plots, one facet per variable x statistic
trace_plot <- ggplot(traces, aes(iteration, value, colour = chain)) +
  geom_rect(data = tibble(burn_in = "Burn-in"),
            aes(xmin = -Inf, xmax = min(window) - 0.5, ymin = -Inf, ymax = Inf, fill = burn_in),
            inherit.aes = FALSE) +
  geom_line(linewidth = 0.4) +
  geom_blank(data = trace_limits, aes(y = value), inherit.aes = FALSE) +
  facet_wrap(~ panel, nrow = 2, scales = "free_y") +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 50), expand = expansion(add = 0.5)) +
  scale_colour_manual(values = chain_colours) +
  scale_fill_manual(values = "grey93") +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2)) +
  labs(x = "Iteration", y = NULL, colour = "Chain", fill = NULL,
       title = "A. Trace plots of the imputed values, synthetic population 1") +
  theme_bw(base_size = 9, base_family = "serif") +
  theme(strip.background = element_rect(fill = "grey90", colour = NA),
        strip.text = element_text(size = 7, margin = margin(3, 2, 3, 2)),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        plot.title = element_text(size = 9.5, face = "bold"))

# Panel B: distribution of the Gelman-Rubin R-hat across populations per variable
gelman_plot <- diagnostics %>%
  mutate(variable = factor(variable_labels[variable], levels = rev(variable_labels))) %>%
  ggplot(aes(rhat_classic, variable)) +
  geom_vline(xintercept = 1.1, linetype = "dashed", linewidth = 0.35, colour = "grey35") +
  geom_boxplot(outlier.shape = NA, width = 0.5, linewidth = 0.35, fill = "grey95") +
  geom_point(position = position_jitter(height = 0.15, seed = 2016), size = 0.5, alpha = 0.35,
             colour = "#0B4F8A") +
  labs(x = expression(hat(R) ~ "(iterations 11–50)"), y = NULL,
       title = "B. Gelman–Rubin statistic") +
  theme_bw(base_size = 9, base_family = "serif") +
  theme(axis.text.y = element_text(colour = "black", size = 8.5),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 9.5, face = "bold"))

# Panel C: distribution of the standardised drift across populations per variable
drift_plot <- diagnostics %>%
  mutate(variable = factor(variable_labels[variable], levels = rev(variable_labels))) %>%
  ggplot(aes(std_drift, variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.35, colour = "grey35") +
  geom_boxplot(outlier.shape = NA, width = 0.5, linewidth = 0.35, fill = "grey95") +
  geom_point(position = position_jitter(height = 0.15, seed = 2016), size = 0.5, alpha = 0.35,
             colour = "#0B4F8A") +
  scale_x_continuous(breaks = seq(-0.005, 0.005, by = 0.0025), labels = function(x) sprintf("%g", x)) +
  coord_cartesian(xlim = c(-0.005, 0.005)) +
  labs(x = "Standardised drift (iterations 11–50)", y = NULL,
       title = "C. Standardised drift") +
  theme_bw(base_size = 9, base_family = "serif") +
  theme(axis.text.y = element_text(colour = "black", size = 8.5),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 9.5, face = "bold"))

# Stack the trace panel over the two diagnostic panels and save the figure.
convergence_plot <- trace_plot / (gelman_plot + drift_plot) + plot_layout(heights = c(2, 1))

ggsave("imgs/LHS000407_convergence.pdf", convergence_plot, width = 8.45, height = 7, device = cairo_pdf)
