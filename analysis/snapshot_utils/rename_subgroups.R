## ###########################################################

##  This script:
## - Contains a function that renames the subgroups to names
## visible in the manuscript

## Adapted from https://github.com/opensafely/covid_mortality_over_time
## Original script by: linda.nab@thedatalab.com - 20220608
## Updates by: edward.parker@lshtm.ac.uk
## ###########################################################

# Load libraries & functions ---
library(dplyr)

# Function ---
## Function 'rename_subgroups'
## Arguments:
## table: table with column 'subgroup' equal to subgroups in config.yaml
## output:
## table with column 'subgroup' that is renamed 
## (e.g., agegroup = Age Group etc.)
rename_subgroups <- function(table){
  table <- 
    table %>%
    mutate(
      subgroup = case_when(
        subgroup == "N" ~ "N",
        
        # Demography
        subgroup == "agegroup_narrow" ~ "Age Group",
        subgroup == "sex" ~ "Sex",
        subgroup == "ethnicity" ~ "Ethnicity",
        subgroup == "region" ~ "Region",
        subgroup == "imd" ~ "IMD quintile",
        subgroup == "bmi" ~ "Body Mass Index",
        subgroup == "smoking_status_comb" ~ "Smoking status",
        
        # Vaccine
        subgroup == "n_vax" ~ "N doses",
        subgroup == "vax_past_12m" ~ "Vaccinated in past 12 months",
        subgroup == "vax_past_24m" ~ "Vaccinated in past 24 months",
        
        # Comorbidities
        subgroup == "asthma" ~ "Asthma",
        subgroup == "diabetes_controlled" ~ "Diabetes",
        subgroup == "ckd_rrt" ~ "CKD or RRT",
        subgroup == "organ_kidney_transplant" ~ "Organ transplant",
        subgroup == "bp_ht" ~ "Hypertension",
        subgroup == "chronic_respiratory_disease" ~ "Chronic respiratory disease",
        subgroup == "chronic_cardiac_disease" ~ "Chronic cardiac disease",
        subgroup == "cancer" ~ "Cancer (non-haematological)",
        subgroup == "haem_cancer" ~ "Haematological malignancy",
        subgroup == "chronic_liver_disease" ~ "Chronic liver disease",
        subgroup == "stroke" ~ "Stroke",
        subgroup == "dementia" ~ "Dementia",
        subgroup == "other_neuro" ~ "Neurological disease",
        subgroup == "asplenia" ~ "Asplenia",
        subgroup == "ra_sle_psoriasis" ~ "Rheumatoid arthritis, lupus, or psoriasis",
        subgroup == "immunosuppression" ~ "Immunodeficiency",
        subgroup == "learning_disability" ~ "Learning disability",
        subgroup == "sev_mental_ill" ~ "Severe mental illness",
    ),
    )
}
