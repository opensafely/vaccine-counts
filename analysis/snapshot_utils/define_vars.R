## ###########################################################

##  This script:
## - Contains a general function that is used to process data that is extracted
##   for table 1

## Adapted from https://github.com/opensafely/covid_mortality_over_time
## Original script by: linda.nab@thedatalab.com - 2022024
## Updates by: edward.parker@lshtm.ac.uk

## ###########################################################

# Load libraries & functions ---
library(here)
library(dplyr)

# Function fct_case_when needed inside process_data
source(here("analysis", "snapshot_utils", "fct_case_when.R"))
source(here("analysis", "snapshot_utils", "between_vectorised.R"))

# Function ---
## Processes the extracted data in extract_data(): changes levels of factors in 
## data
## args:
## - data_extracted: a data.frame extracted by function extract_data() in 
##   ./analysis/utils/extract_data.R
## output:
## data.frame of data_extracted with factor columns with correct levels
define_vars <- function(data_extracted) {
  data_processed <-
    data_extracted %>%
    mutate(
      agegroup_narrow = fct_case_when(
        agegroup_narrow == "18-49" ~ "18-49", # = reference
        agegroup_narrow == "50-54" ~ "50-54",
        agegroup_narrow == "55-59" ~ "55-59",
        agegroup_narrow == "60-64" ~ "60-64",
        agegroup_narrow == "65-69" ~ "65-69",
        agegroup_narrow == "70-74" ~ "70-74",
        agegroup_narrow == "75-79" ~ "75-79",
        agegroup_narrow == "80plus" ~ "80+",
        TRUE ~ NA_character_
      ),
      # no missings should occur as individuals with
      # missing age are not included in the study
      
      agegroup_medium = fct_case_when(
        agegroup_medium == "18-49" ~ "18-49", # = reference
        agegroup_medium == "50-64" ~ "50-64",
        agegroup_medium == "65-74" ~ "65-74",
        agegroup_medium == "75plus" ~ "75+",
        TRUE ~ NA_character_
      ),
      
      agegroup_broad = fct_case_when(
        agegroup_broad == "18-49" ~ "18-49", # = reference
        agegroup_broad == "50-64" ~ "50-64",
        agegroup_broad == "65plus" ~ "65+",
        TRUE ~ NA_character_
      ),
      
      sex = fct_case_when(sex == "F" ~ "Female",
                          sex == "M" ~ "Male",
                          TRUE ~ NA_character_),
      # no missings should occur as only of
      # individuals with a female/male sex, data is extracted
      
      ethnicity_primary = fct_case_when(
        ethnicity_primary == "1" ~ "White",
        ethnicity_primary == "2" ~ "Mixed",
        ethnicity_primary == "3" ~ "South Asian",
        ethnicity_primary == "4" ~ "Black",
        ethnicity_primary == "5" ~ "Other",
        ethnicity_primary == "0" ~ "Unknown",
        TRUE ~ NA_character_ # no missings in real data expected
        # (all mapped into 0) but dummy data will have missings (data is joined
        # and patient ids are not necessarily the same in both cohorts)
      ),
      
      ethnicity = fct_case_when(
        ethnicity == "1" ~ "White",
        ethnicity == "2" ~ "Mixed",
        ethnicity == "3" ~ "South Asian",
        ethnicity == "4" ~ "Black",
        ethnicity == "5" ~ "Other",
        ethnicity == "0" ~ "Unknown",
        TRUE ~ NA_character_       ),
      
      ethnicity_primary_16 = fct_case_when(
        ethnicity_primary_16 == "1" ~ "British or Mixed British",
        ethnicity_primary_16 == "2" ~ "Irish",
        ethnicity_primary_16 == "3" ~ "Other White",
        ethnicity_primary_16 == "4" ~ "White + Black Caribbean",
        ethnicity_primary_16 == "5" ~ "White + Black African",
        ethnicity_primary_16 == "6" ~ "White + Asian",
        ethnicity_primary_16 == "7" ~ "Other mixed",
        ethnicity_primary_16 == "8" ~ "Indian or British Indian",
        ethnicity_primary_16 == "9" ~ "Pakistani or British Pakistani",
        ethnicity_primary_16 == "10" ~ "Bangladeshi or British Bangladeshi",
        ethnicity_primary_16 == "11" ~ "Other Asian",
        ethnicity_primary_16 == "12" ~ "Caribbean",
        ethnicity_primary_16 == "13" ~ "African",
        ethnicity_primary_16 == "14" ~ "Other Black",
        ethnicity_primary_16 == "15" ~ "Chinese",
        ethnicity_primary_16 == "16" ~ "Other",
        ethnicity_primary_16 == "0" ~ "Unknown",
        TRUE ~ NA_character_ # no missings in real data expected
        # (all mapped into 0) but dummy data will have missings (data is joined
        # and patient ids are not necessarily the same in both cohorts)
      ),
      
      ethnicity_16 = fct_case_when(
        ethnicity_16 == "1" ~ "British or Mixed British",
        ethnicity_16 == "2" ~ "Irish",
        ethnicity_16 == "3" ~ "Other White",
        ethnicity_16 == "4" ~ "White + Black Caribbean",
        ethnicity_16 == "5" ~ "White + Black African",
        ethnicity_16 == "6" ~ "White + Asian",
        ethnicity_16 == "7" ~ "Other mixed",
        ethnicity_16 == "8" ~ "Indian or British Indian",
        ethnicity_16 == "9" ~ "Pakistani or British Pakistani",
        ethnicity_16 == "10" ~ "Bangladeshi or British Bangladeshi",
        ethnicity_16 == "11" ~ "Other Asian",
        ethnicity_16 == "12" ~ "Caribbean",
        ethnicity_16 == "13" ~ "African",
        ethnicity_16 == "14" ~ "Other Black",
        ethnicity_16 == "15" ~ "Chinese",
        ethnicity_16 == "16" ~ "Other",
        ethnicity_16 == "0" ~ "Unknown",
        TRUE ~ NA_character_ 
      ),
      
      imd = fct_case_when(
        imd == "5" ~ "5 (least deprived)",
        imd == "4" ~ "4",
        imd == "3" ~ "3",
        imd == "2" ~ "2",
        imd == "1" ~ "1 (most deprived)",
        imd == "0" ~ NA_character_
      ),
      
      imd_decile = fct_case_when(
        imd_decile == "10" ~ "10 (least deprived)",
        imd_decile == "9" ~ "9",
        imd_decile == "8" ~ "8",
        imd_decile == "7" ~ "7",
        imd_decile == "6" ~ "6",
        imd_decile == "5" ~ "5",
        imd_decile == "4" ~ "4",
        imd_decile == "3" ~ "3",
        imd_decile == "2" ~ "2",
        imd_decile == "1" ~ "1 (most deprived)",
        imd_decile == "0" ~ NA_character_
      ),
      
      region = fct_case_when(
        region == "North East" ~ "North East",
        region == "North West" ~ "North West",
        region == "Yorkshire and The Humber" ~ "Yorkshire and the Humber",
        region == "East Midlands" ~ "East Midlands",
        region == "West Midlands" ~ "West Midlands",
        region == "East" ~ "East of England",
        region == "London" ~ "London",
        region == "South East" ~ "South East",
        region == "South West" ~ "South West",
        TRUE ~ NA_character_
      ),
      
      bmi = fct_case_when(
        bmi == "Not obese" ~ "Not obese",
        bmi == "Obese I (30-34.9)" ~ "Obese I (30-34.9 kg/m2)",
        bmi == "Obese II (35-39.9)" ~ "Obese II (35-39.9 kg/m2)",
        bmi == "Obese III (40+)" ~ "Obese III (40+ kg/m2)",
        TRUE ~ NA_character_
      ),
      
      smoking_status_comb = fct_case_when(
        smoking_status_comb == "N + M" ~ "Never and unknown",
        smoking_status_comb == "E" ~ "Former",
        smoking_status_comb == "S" ~ "Current",
        TRUE ~ NA_character_
      ),
      
      # comorbidities
      asthma = fct_case_when(
        asthma == "0" ~ "No asthma",
        asthma == "1" ~ "With no oral steroid use",
        asthma == "2" ~ "With oral steroid use"
      ),
      
      bp = fct_case_when(
        bp == "1" ~ "Normal",
        bp == "2" ~ "Elevated/High",
        bp == "0" ~ "Unknown"
      ),
      
      diabetes_controlled = fct_case_when(
        diabetes_controlled == "0" ~ "No diabetes",
        diabetes_controlled == "1" ~ "Controlled",
        diabetes_controlled == "2" ~ "Not controlled",
        diabetes_controlled == "3" ~ "Without recent Hb1ac measure"
      ),
      
      ckd_rrt = fct_case_when(
        ckd_rrt == "No CKD or RRT" ~ "No CKD or RRT",
        ckd_rrt == "Stage 3a" ~ "CKD stage 3a",
        ckd_rrt == "Stage 3b" ~ "CKD stage 3b",
        ckd_rrt == "Stage 4" ~ "CKD stage 4",
        ckd_rrt == "Stage 5" ~ "CKD stage 5",
        ckd_rrt == "RRT (dialysis)" ~ "RRT (dialysis)",
        ckd_rrt == "RRT (transplant)" ~ "RRT (transplant)"
      ),
      
      organ_kidney_transplant = fct_case_when(
        organ_kidney_transplant == "No transplant" ~ "No transplant",
        organ_kidney_transplant == "Kidney" ~ "Kidney transplant",
        organ_kidney_transplant == "Organ" ~ "Other organ transplant"
      ),
      
      # Yes/No variables
      bp_ht = fct_case_when(
        bp_ht == 0 ~ "No",
        bp_ht == 1 ~ "Yes"
      ),
      
      chronic_respiratory_disease = fct_case_when(
        chronic_respiratory_disease == 0 ~ "No",
        chronic_respiratory_disease == 1 ~ "Yes"
      ),
      
      chronic_cardiac_disease = fct_case_when(
        chronic_cardiac_disease == 0 ~ "No",
        chronic_cardiac_disease == 1 ~ "Yes"
      ),
      
      cancer = fct_case_when(
        cancer == 0 ~ "No",
        cancer == 1 ~ "Yes"
      ),
      
      haem_cancer = fct_case_when(
        haem_cancer == 0 ~ "No",
        haem_cancer == 1 ~ "Yes"
      ),
      
      chronic_liver_disease = fct_case_when(
        chronic_liver_disease == 0 ~ "No",
        chronic_liver_disease == 1 ~ "Yes"
      ),
      
      stroke = fct_case_when(
        stroke == 0 ~ "No",
        stroke == 1 ~ "Yes"
      ),
      
      dementia = fct_case_when(
        dementia == 0 ~ "No",
        dementia == 1 ~ "Yes"
      ),
      
      other_neuro = fct_case_when(
        other_neuro == 0 ~ "No",
        other_neuro == 1 ~ "Yes"
      ),
      
      asplenia = fct_case_when(
        asplenia == 0 ~ "No",
        asplenia == 1 ~ "Yes"
      ),
      
      ra_sle_psoriasis = fct_case_when(
        ra_sle_psoriasis == 0 ~ "No",
        ra_sle_psoriasis == 1 ~ "Yes"
      ),
      
      immunosuppression = fct_case_when(
        immunosuppression == 0 ~ "No",
        immunosuppression == 1 ~ "Yes"
      ),
      
      learning_disability = fct_case_when(
        learning_disability == 0 ~ "No",
        learning_disability == 1 ~ "Yes"
      ),
      
      sev_mental_ill = fct_case_when(
        sev_mental_ill == 0 ~ "No",
        sev_mental_ill == 1 ~ "Yes"
      ),
      
      # dose counts pre index
      n_vax = fct_case_when(
        covid_vax_date_7<=index_date ~ "7",
        covid_vax_date_6<=index_date ~ "6",
        covid_vax_date_5<=index_date ~ "5",
        covid_vax_date_4<=index_date ~ "4",
        covid_vax_date_3<=index_date ~ "3",
        covid_vax_date_2<=index_date ~ "2",
        covid_vax_date_1<=index_date ~ "1",
        TRUE ~ "0"
      ),
      
      # Define time since last vaccination
      time_since_last_vax_days = plyr::round_any(as.numeric(index_date - covid_vax_date_most_recent),7), # rounded to nearest week
      time_since_last_vax_months = time_since_last_vax_days/(365/12),
      vax_past_12m = as.numeric(time_since_last_vax_days<=365),
      vax_past_24m = as.numeric(time_since_last_vax_days<=(365*2)),
    )
  
  # Set upper limits for time since vaccination to 3 years (equivalent to 01 Sep 2020, which precedes vaccine roll-out)
  data_processed$time_since_last_vax_days[is.na(data_processed$time_since_last_vax_days)] = 365*3
  data_processed$time_since_last_vax_months[is.na(data_processed$time_since_last_vax_months)] = 36
  
  data_processed
}

