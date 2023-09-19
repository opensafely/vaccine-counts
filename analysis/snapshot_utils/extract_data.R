## ###########################################################

##  This script:
## - Contains a general function that is used to reformat data

## Adapted from https://github.com/opensafely/covid_mortality_over_time
## Original script by: linda.nab@thedatalab.com - 2022024
## Updates by: edward.parker@lshtm.ac.uk

## ###########################################################

# Load libraries & functions ---
library(dplyr)
library(here)
library(lubridate)
library(jsonlite)
library(readr)

# Function ---
## Extracts data and maps columns to the correct format (integer, factor etc)
## args:
## - file_name: string with the location of the input file extracted by the 
##   cohortextracter
## output:
## data.frame of the input file, with columns of the correct type
extract_data <- function(file_name) {
  ## read all data with default col_types 
  data_extracted <-
    read_csv(
      file_name,
      col_types = cols(
        patient_id = col_integer(),
        has_follow_up = col_logical(),
        
        # demographics
        age = col_integer(),
        agegroup_narrow = col_character(),
        agegroup_medium = col_character(),
        agegroup_broad = col_character(),
        sex = col_character(),
        ethnicity_primary = col_number(),
        ethnicity_sus = col_number(),
        ethnicity = col_number(),
        ethnicity_primary_16 = col_number(),
        ethnicity_sus_16 = col_number(),
        ethnicity_16 = col_number(),
        bmi_value = col_double(),
        bmi = col_character(),
        smoking_status = col_character(),
        smoking_status_comb = col_character(),
        imd = col_number(),
        imd_decile = col_number(),
        region = col_character(),
        
        # comorbidities (multilevel)
        asthma = col_number(),
        bp = col_number(),
        bp_ht = col_logical(),
        diabetes_controlled = col_number(),
        
        # ckd/rrt
        # dialysis or kidney transplant
        rrt_cat = col_number(),
        # calc of egfr
        creatinine = col_number(), 
        creatinine_operator = col_character(),
        creatinine_age = col_number(),
        
        # organ or kidney transplant
        organ_kidney_transplant = col_character(),
        
        # comorbidities (binary)
        hypertension = col_logical(),
        chronic_respiratory_disease = col_logical(),
        chronic_cardiac_disease = col_logical(),
        cancer = col_logical(),
        haem_cancer = col_logical(),
        chronic_liver_disease = col_logical(),
        stroke = col_logical(),
        dementia = col_logical(),
        other_neuro = col_logical(),
        asplenia = col_logical(),
        ra_sle_psoriasis = col_logical(),
        immunosuppression = col_logical(),
        learning_disability = col_logical(),
        sev_mental_ill = col_logical(),
        
        # vaccination dates
        covid_vax_date_1 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_2 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_3 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_4 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_5 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_6 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_7 = col_date(format = "%Y-%m-%d"),
        covid_vax_date_most_recent = col_date(format = "%Y-%m-%d"),
        .default = col_skip()        
      ),   
      na = character() # more stable to convert to missing later
    ) %>%
    # Filter to individuals with 3 months of follow-up at a single GP
    filter(has_follow_up == TRUE & 
             !is.na(age) & age >=18 & age<=110
             ) %>%
    # Convert TRUE/FALSE to 1/0
    mutate(across(
      where(is.logical),
      ~.x*1L 
    )) %>%
  arrange(patient_id) 
  data_extracted
}
