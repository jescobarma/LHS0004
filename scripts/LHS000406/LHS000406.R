#============================================================================
#
#   Program: LHS000406.R
#
#   Description: Generates descriptive survey-weighted tables for neonatal outcomes
#                using data from LHS000401.
#
#   Version: R 4.5.1
#
#  ----------------------------------------------------------------
#
#   Input:  data/derived/LHS000401.rda
# 
#   Output: Descriptive table (printed to console or exported manually)
#
#============================================================================

# --- Load required packages ---
suppressPackageStartupMessages({
  library(survey)
  library(dplyr)
  library(knitr)
  library(kableExtra)
})

# --- Load derived dataset ---
load("../../data/derived/LHS000401.rda")

# --- Define survey design ---
survey_design <- svydesign(
  data = LHS000401,
  strata = ~strata,
  weights = ~weight,
  id = ~psu,
  nest = TRUE
)

#============================================================================
#   Function: generate_neonatal_table
#   Purpose:  Create a frequency and percentage table (No/Yes) for each variable
#============================================================================
generate_neonatal_table <- function(variable, design) {
  
  # Prepare data subset
  df <- LHS000401 %>%
    select(all_of(c(variable, "neonatal"))) %>%
    mutate(
      !!sym(variable) := ifelse(is.na(.data[[variable]]), "Missing", as.character(.data[[variable]]))
    )
  
  # Ensure "Missing" appears last
  levels_var <- unique(df[[variable]])
  if ("Missing" %in% levels_var) 
    levels_var <- c(setdiff(levels_var, "Missing"), "Missing")
  
  df[[variable]] <- factor(df[[variable]], levels = levels_var)
  
  # Update design with variable
  design_var <- update(design)
  design_var$variables[[variable]] <- df[[variable]]
  
  # Create weighted tables
  weighted_tab <- svytable(as.formula(paste0("~", variable, "+neonatal")), design_var)
  weighted_prop <- prop.table(weighted_tab, margin = 1)
  
  # Create unweighted counts for display
  count_tab <- table(df[[variable]], df$neonatal, useNA = "ifany")
  
  # Add missing columns if needed
  for (cat in c("No", "Yes")) {
    if (!(cat %in% colnames(count_tab))) {
      count_tab <- cbind(count_tab, setNames(rep(0, nrow(count_tab)), cat))
    }
  }
  
  # Build output table
  output_df <- data.frame(
    Category = rownames(count_tab),
    Neonatal_No  = paste0(count_tab[, "No"], " (", sprintf("%.2f", 100 * weighted_prop[, "No"]), "%)"),
    Neonatal_Yes = paste0(count_tab[, "Yes"], " (", sprintf("%.2f", 100 * weighted_prop[, "Yes"]), "%)")
  )
  
  # Add title row
  title_row <- data.frame(Category = paste0("**", variable, "**"),
                          Neonatal_No = "", Neonatal_Yes = "")
  
  output_df <- rbind(title_row, output_df)
  return(output_df)
}

#============================================================================
#   Variables to be analyzed
#============================================================================
variables <- c(
  "child_sex", "region", "urban", "bord_spacing_c5",
  "underwt_birth", "preterm_birth", "mage_birth_c3", "meduc_c3",
  "ethnicity_c3", "any_substance_use", "wealth_index_c3", "anc_quality_score_c2"
)

#============================================================================
#   Generate and combine all tables
#============================================================================
neonatal_tables <- lapply(variables, function(v) {
  tryCatch(
    generate_neonatal_table(v, survey_design),
    error = function(e) {
      message("Error with variable: ", v, " -> ", e$message)
      return(NULL)
    }
  )
})

neonatal_final <- do.call(rbind, neonatal_tables)

#============================================================================
#   Add global totals
#============================================================================
neonatal_total <- svytable(~neonatal, survey_design)
total_row <- data.frame(
  Category = "**Total**",
  Neonatal_No  = paste0(sprintf("%.2f", 100 * prop.table(neonatal_total)["No"]), "%"),
  Neonatal_Yes = paste0(sprintf("%.2f", 100 * prop.table(neonatal_total)["Yes"]), "%")
)

neonatal_final <- rbind(neonatal_final, total_row)

#============================================================================
#   Display final table
#============================================================================
neonatal_final %>%
  kbl(
    align = "lcc",
    booktabs = TRUE,
    escape = FALSE,
    caption = "Descriptive Table of Neonatal Outcomes (Row Percentages)"
  ) %>%
  kable_styling(full_width = FALSE, position = "center")

#============================================================================
#   End of Program
#============================================================================

