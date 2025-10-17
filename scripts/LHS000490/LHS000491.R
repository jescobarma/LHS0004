# ============================================================================
# formats_LHS000401.R
# ============================================================================
# This script defines reusable labeling functions ("formats") for key
# categorical variables in the DHS-based project:
#    LHS0004 - Determinants of Neonatal Mortality in Colombia
#
# Each format function mimics a SAS PROC FORMAT VALUE statement:
# it takes a coded variable and returns a labeled factor.
# ============================================================================

# ----------------------------------------------------------------------------
# Urban/Rural classification (urban)
# ----------------------------------------------------------------------------
fmt_urban <- function(x) {
  factor(x,
         levels = c(0, 1),
         labels = c("Rural", "Urban"))
}

# ----------------------------------------------------------------------------
# Region
# ----------------------------------------------------------------------------

fmt_region <- function(x) {
  factor(x,
         levels = 1:6,
         labels = c("Atlantica", "Oriental", "Central", "Pacifica", "Bogota", "Orinoquia/Amazonia"))
}


# ----------------------------------------------------------------------------
# Child’s sex (child_sex)
# ----------------------------------------------------------------------------
fmt_child_sex <- function(x) {
  factor(x,
         levels = c(0, 1),
         labels = c("Male", "Female"))
}

# ----------------------------------------------------------------------------
# Neonatal or Infant Death (binary)
# ----------------------------------------------------------------------------
fmt_binary <- function(x) {
  factor(x,
         levels = c(0, 1),
         labels = c("No", "Yes"))
}

# ----------------------------------------------------------------------------
# Birth order and spacing (5 categories)
# ----------------------------------------------------------------------------
fmt_birth_order_spacing_c5 <- function(x) {
  factor(x,
         levels = 0:4,
         labels = c(
           "First birth",
           "2–3 (<36 months apart)",
           "2–3 (≥36 months apart)",
           ">3 (<36 months apart)",
           ">3 (≥36 months apart)"
         ))
}

# ----------------------------------------------------------------------------
# Mother’s age at birth (3 categories)
# ----------------------------------------------------------------------------
fmt_mage_birth_c3 <- function(x) {
  factor(x,
         levels = 1:3,
         labels = c("≤19 years", "20–34 years", "≥35 years"))
}

# ----------------------------------------------------------------------------
# Mother’s education (3 categories)
# ----------------------------------------------------------------------------
fmt_meduc_c3 <- function(x) {
  factor(x,
         levels = 1:3,
         labels = c("No education", "Primary", "Secondary or higher"))
}

# ----------------------------------------------------------------------------
# Ethnicity (3 categories)
# ----------------------------------------------------------------------------
fmt_ethnicity_c3 <- function(x) {
  factor(x,
         levels = 1:3,
         labels = c("Indigenous", "Afro-descendant", "Other"))
}

# ----------------------------------------------------------------------------
# Wealth index (3 categories)
# ----------------------------------------------------------------------------
fmt_wealth_index_c3 <- function(x) {
  factor(x,
         levels = 1:3,
         labels = c("Poor", "Middle", "Rich"))
}

# ----------------------------------------------------------------------------
# Antenatal care quality score (2 categories)
# ----------------------------------------------------------------------------
fmt_anc_quality_score_c2 <- function(x) {
  factor(x,
         levels = c(0, 1),
         labels = c("Low quality", "High quality"))
}

# ----------------------------------------------------------------------------
# Format dictionary
# ----------------------------------------------------------------------------
# This list maps variable names in your derived dataset to the corresponding
# format functions. It can be used with the apply_formats() helper below.
# ----------------------------------------------------------------------------
formats_LHS000401 <- list(
  urban = fmt_urban,
  region = fmt_region,
  child_sex = fmt_child_sex,
  neonatal = fmt_binary,
  infant = fmt_binary,
  birth_order_spacing_c5 = fmt_birth_order_spacing_c5,
  underwt_birth = fmt_binary,
  preterm_birth = fmt_binary,
  mage_birth_c3 = fmt_mage_birth_c3,
  meduc_c3 = fmt_meduc_c3,
  ethnicity_c3 = fmt_ethnicity_c3,
  any_substance_use = fmt_binary,
  wealth_index_c3 = fmt_wealth_index_c3,
  anc_quality_score_c2 = fmt_anc_quality_score_c2
)

# ----------------------------------------------------------------------------
# Apply all formats automatically
# ----------------------------------------------------------------------------
# Example: LHS000401 <- apply_formats(LHS000401, formats)
# ----------------------------------------------------------------------------
apply_formats <- function(data, format_list) {
  for (v in names(format_list)) {
    if (v %in% names(data)) {
      data[[v]] <- format_list[[v]](data[[v]])
    }
  }
  data
}
