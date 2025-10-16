################################################################################
# Program name: LHS000401.R
#
# Description: 
#   This R script processes the 2015 Colombia DHS Children’s Recode (KR file)
#   and generates a derived dataset used for the analysis of child health outcomes.
#   The script reproduces the same data management procedures as the DHS program’s 
#   SAS routine (LHS000101.sas), adapting them to R using the haven and dplyr packages.
#
# Version: R 4.3.3 
#
# Authors: Alvaro Quijano Angarita & Javier Escobar Martinez
# 
# Affiliation: UNC-Chapel Hill
#
# Input:
#   - Raw DHS data file: COKR72FL.SAS7BDAT (Children’s Recode 2015)
#
# Output:
#   - Derived dataset: LHS000401.rda
#
# Main steps:
#   1. Load and clean the DHS child-level data.
#   2. Create survey design variables (strata, PSU, weights).
#   3. Generate socio-demographic and maternal covariates:
#        - Region, urban/rural classification, wealth index, ethnicity, education.
#   4. Compute maternal and child health indicators:
#        - Child sex, preterm birth, neonatal/infant mortality.
#   5. Construct antenatal care (ANC) quality indicators:
#        - Number of ANC visits, tetanus immunization, timing of first visit,
#          skilled provider attendance, and ANC quality score (missing > 2 → NA).
#   6. Select relevant derived variables for analysis and save dataset.
#
# Output dimensions: 11,759 observations × 17 variables
#
# Required packages:
#   - haven (for reading DHS SAS files)
#   - dplyr (for data manipulation)
#
# Date created: 2025-10-06
# Last updated: 2025-10-16
################################################################################

# Clear the workspace
rm(list = ls(all = TRUE))

# Load required packages
library(haven)   # to read DHS data (SAS, Stata, SPSS formats)
library(dplyr)   # for data manipulation

# Load the DHS Children’s Recode (KR) dataset Colombia (2015)
kr_2015 <- read_sas("../../data/raw/COKR72FL.SAS7BDAT")

# ------------------------------------------------------------------------------
# Derive variables
# ------------------------------------------------------------------------------

LHS000401 <- kr_2015 %>%
  mutate(
    # -------------------------------
    # Design variables
    # -------------------------------
    strata = V022,
    psu = V001,
    weight = V005 / 1000000,
    
    # -------------------------------
    # Urban/Rural classification (1 = "Urban" 0 = "Rural")
    # -------------------------------
    urban = if_else(V025 == 1, 1, 0),
    
    # -------------------------------
    # Region
    # -------------------------------
    region = case_when(
      V101 == 1 ~ "Atlantica",
      V101 == 2 ~ "Oriental",
      V101 == 3 ~ "Central",
      V101 == 4 ~ "Pacifica",
      V101 == 5 ~ "Bogota",
      V101 == 6 ~ "Orinoquia/Amazonia"
    ),
    
    # -------------------------------
    # Child sex (1=female, 0=male)
    # -------------------------------
    child_sex = if_else(B4 == 2, 1, 0),
    
    # -------------------------------
    # Neonatal and infant deaths (1 = yes, 0 = no)
    # -------------------------------
    neonatal = if_else(B5 == 0 & B7 < 1, 1, 0),  # death <1 month
    infant = if_else(B5 == 0 & B7 < 12, 1, 0),   # death <12 months
    
    # -------------------------------
    # Child year of birth
    # -------------------------------
    yob = trunc(1900 + (B3 - 1) / 12),

    # -------------------------------
    # Birth order and spacing (5 categories)
    # -------------------------------
    birth_order_spacing_c5 = case_when(
      BORD == 1 ~ 0,
      BORD %in% c(2, 3) & B11 < 36 ~ 1,
      BORD %in% c(2, 3) & B11 >= 36 ~ 2,
      BORD > 3 & B11 < 36 ~ 3,
      BORD > 3 & B11 >= 36 ~ 4
    ),
    
    # -------------------------------
    # Birth weight (in grams)
    # -------------------------------
    birth_weight = if_else(M19 %in% c(9996, 9998), NA_real_, M19),
    underwt_birth = ifelse(is.na(birth_weight),NA_real_,
                           ifelse(birth_weight < 2500, 1, 0)),
    
    # -------------------------------
    # Preterm birth (0 = "No", 1 = "Yes")
    # -------------------------------
    preterm_birth = case_when(
      S445 == 1 ~ 1,
      S445 == 0 ~ 0
    ),
    
    # -------------------------------
    # Mother's age at birth (3 categories)
    # -------------------------------
    mage_birth = round((B3 - V011) / 12, 1),
    mage_birth_c3 = case_when(
      mage_birth <= 19 ~ 1,
      mage_birth > 19 & mage_birth < 35 ~ 2,
      mage_birth >= 35 ~ 3
    ),
    
    # -------------------------------
    # Mother's education (3 categories) 0 = "No education", 1 = "Primary", 
    # 2 = "Secondary", 3 = "Higher"
    # -------------------------------
    meduc_c3 = case_when(
      V106 == 0 ~ 1,
      V106 == 1 ~ 2,
      V106 %in% c(2, 3) ~ 3
    ),
    
    # -------------------------------
    # Ethnicity (3 categories)
    # -------------------------------
    ethnicity_c3 = case_when(
      V131 == 1 ~ 1,                  # Indigenous
      V131 %in% c(3, 4, 5) ~ 2,       # Afro-descendant
      V131 %in% c(2, 6) ~ 3          # Other
    ),
    
    # -------------------------------
    # Any substance use during pregnancy
    # -------------------------------
    any_substance_use = case_when(
      # All missing o 8 -> NA
      (is.na(S428) & is.na(S430) &
         (is.na(S433A) | S433A == 8) &
         (is.na(S433B) | S433B == 8) &
         (is.na(S433C) | S433C == 8) &
         (is.na(S433X) | S433X == 8)) ~ NA_integer_,
      
      # some equal to 1 -> 1
      S428 == 1 | S430 == 1 | S433A == 1 | S433B == 1 | 
        S433C == 1 | S433X == 1 ~ 1,
      
      # otherwise -> 0
      TRUE ~ 0
    ),
    
    # -------------------------------
    # Wealth index (3 categories)
    # -------------------------------
    wealth_index_c3 = case_when(
      V190 %in% c(1, 2) ~ 1,
      V190 == 3 ~ 2,
      V190 %in% c(4, 5) ~ 3
    ),
    # -------------------------------
    # Number of antenatal visits (4-categories) | Number of ANC visits
    # -------------------------------
    anc_visits_c4 = case_when(
      M14 == 98 | is.na(M14) ~ NA_real_,    # "Don't know" o missing
      M14 == 0 ~ 0,                         # No visits
      M14 %in% c(1, 2, 3) ~ 1,              # 1–3 visits
      M14 %in% c(4, 5, 6, 7) ~ 2,           # 4–7 visits
      M14 > 7 ~ 3                          # More than 7 visits
    ), 
    # -------------------------------
    # at least two tetanus toxoid injections during pregnancy (>=2)
    # -------------------------------
    tetanus_2_plus = case_when(
      is.na(M1) | M1 == 8 ~ NA_real_,   # Missing o “Don’t know”
      M1 >= 2 ~ 1,                      # Received ≥ 2 doses
      M1 < 2 ~ 0                        # Less than 2 doses
    ),
    # -------------------------------
    # Anc_1st_trimester
    # -------------------------------
    anc_1st_trimester = case_when(
      is.na(M13) | M13 == 98 ~ NA_real_,  # Missing o “Don’t know”
      M13 <= 2 ~ 1,                       # First consultation before or during the second month (first trimester)
      M13 > 2 ~ 0                         # After the first trimester
    ),
    # -------------------------------
    # Mother saw at least one skilled provider during antenatal care.
    # -------------------------------
    skilled_prenatal = case_when(
      is.na(M57E) | is.na(M57M) | is.na(M57N) | is.na(M57O) | is.na(M57P) | is.na(M57X) ~ NA_real_,
      M57E == 1 | M57M == 1 | M57N == 1 | M57O == 1 | M57P == 1 ~ 2,  # Saw at least one qualified provider
      TRUE ~ 0
    ),
    # -------------------------------
    # ANC quality score | Antenatal Care Quality Score
    # -------------------------------
    anc_quality_score = case_when(
      rowSums(is.na(cbind(
        anc_visits_c4, tetanus_2_plus, anc_1st_trimester,
        M42C, M42D, M42E, skilled_prenatal
      ))) > 2 ~ NA_real_,
      TRUE ~ anc_visits_c4 + tetanus_2_plus + anc_1st_trimester +
        M42C + M42D + M42E + skilled_prenatal
    ), 
    anc_quality_score_c2 = ifelse(anc_quality_score >=8 , 1, ifelse(is.na(anc_quality_score), NA_real_, 0))
  ) %>% 
  # Keep only the requested variables
  select(region, strata, psu, weight, urban, child_sex, yob, neonatal, infant,
         birth_order_spacing_c5, underwt_birth, preterm_birth,
         mage_birth_c3, meduc_c3, ethnicity_c3, any_substance_use,
         wealth_index_c3, anc_quality_score_c2)

# Save the derived dataset
save(LHS000401, file = "../../data/derived/LHS000401.rda")


