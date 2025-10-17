# =======================================================
# formats_LHS000490.R
# =======================================================
# Labeling functions ("formats") for key categorical variables in the DHS project:
#   LHS0004 – Determinants of Neonatal Mortality in Colombia
#
# Each function mimics a SAS PROC FORMAT VALUE statement:
# it takes a coded variable and returns a labeled factor.
# =======================================================

# Urban/Rural classification (urban)
fmt_urban <- function(x, ref = "Rural") {
  f <- factor(x,
              levels = c(0, 1),
              labels = c("Rural", "Urban"))
  relevel(f, ref = ref)
}

# Region
fmt_region <- function(x, ref = "Bogota") {
  f <- factor(x,
              levels = 1:6,
              labels = c("Atlantica", "Oriental", "Central",
                         "Pacifica", "Bogota", "Orinoquia/Amazonia"))
  relevel(f, ref = ref)
}
# Child’s sex (child_sex)
fmt_child_sex <- function(x, ref = "Male") {
  f <- factor(x,
              levels = c(0, 1),
              labels = c("Male", "Female"))
  relevel(f, ref = ref)
}

# Y/N formats
fmt_binary <- function(x, ref = "No") {
  f <- factor(x,
              levels = c(0, 1),
              labels = c("No", "Yes"))
  relevel(f, ref = ref)
}

# Birth order and spacing (5 categories)
fmt_bord_spacing_c5 <- function(x, ref = "First birth") {
  f <- factor(x,
              levels = 0:4,
              labels = c(
                "First birth",
                "2–3 (<36 months apart)",
                "2–3 (≥36 months apart)",
                ">3 (<36 months apart)",
                ">3 (≥36 months apart)"
              ))
  relevel(f, ref = ref)
}

# Mother’s age at birth (3 categories)
fmt_mage_birth_c3 <- function(x, ref = "20–34 years") {
  f <- factor(x,
              levels = 1:3,
              labels = c("≤19 years", "20–34 years", "≥35 years"))
  relevel(f, ref = ref)
}

# Mother’s education (3 categories)
fmt_meduc_c3 <- function(x, ref = "Secondary or higher") {
  f <- factor(x,
              levels = 1:3,
              labels = c("No education", "Primary", "Secondary or higher"))
  relevel(f, ref = ref)
}

# Ethnicity (3 categories)
fmt_ethnicity_c3 <- function(x, ref = "Other") {
  f <- factor(x,
              levels = 1:3,
              labels = c("Indigenous", "Afro-descendant", "Other"))
  relevel(f, ref = ref)
}

# Wealth index (3 categories)
fmt_wealth_index_c3 <- function(x, ref = "Middle") {
  f <- factor(x,
              levels = 1:3,
              labels = c("Poor", "Middle", "Rich"))
  relevel(f, ref = ref)
}

# Antenatal care quality score (2 categories)
fmt_anc_quality_score_c2 <- function(x, ref = "Low/Moderate") {
  f <- factor(x,
              levels = c(0, 1),
              labels = c("Low/Moderate", "High"))
  relevel(f, ref = ref)
}

# Format dictionary
# Maps variable names to their corresponding format functions for use with apply_formats().
formats_LHS0004 <- list(
  urban = fmt_urban,
  region = fmt_region,
  child_sex = fmt_child_sex,
  neonatal = fmt_binary,
  infant = fmt_binary,
  bord_spacing_c5 = fmt_bord_spacing_c5,
  underwt_birth = fmt_binary,
  preterm_birth = fmt_binary,
  mage_birth_c3 = fmt_mage_birth_c3,
  meduc_c3 = fmt_meduc_c3,
  ethnicity_c3 = fmt_ethnicity_c3,
  any_substance_use = fmt_binary,
  wealth_index_c3 = fmt_wealth_index_c3,
  anc_quality_score_c2 = fmt_anc_quality_score_c2
)

# Apply all formats automatically
# Example: LHS000401 <- apply_formats(LHS000401, formats)
apply_formats <- function(data, format_list) {
  for (v in names(format_list)) {
    if (v %in% names(data)) {
      data[[v]] <- format_list[[v]](data[[v]])
    }
  }
  data
}
