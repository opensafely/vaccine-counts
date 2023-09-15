######################################

# This script 
# - produces a table summarising selected clinical and demographic groups in study cohort, stratified by primary vaccine product
# - saves table as html

######################################

## Import libraries
library(tidyverse)
library(here)
library(glue)
library(dplyr)
library(gt)
library(gtsummary)
library(reshape2)

# Import filtered data
data <- read_rds(here::here("output", "snapshot", paste0("processed_snapshot.rds")))

# Recode n_vax
data$n_vax = as.numeric(as.character(data$n_vax))

# Define broad age subsets for stratified analysis
data_50to64 <- subset(data, agegroup_broad=="50-64")
data_65plus <- subset(data, agegroup_broad=="65plus")

## Set rounding and redaction thresholds
rounding_threshold = 7

# Function to summarise vaccination history for given data frame or piped subset
summarise_vax_history = function(data) {
  data <- data %>%
    summarise(
      N = plyr::round_any(length(agegroup_narrow), rounding_threshold),
      Dose_0 = plyr::round_any(sum(n_vax==0), rounding_threshold),
      Dose_1 = plyr::round_any(sum(n_vax==1), rounding_threshold),
      Dose_2 = plyr::round_any(sum(n_vax==2), rounding_threshold),
      Dose_3 = plyr::round_any(sum(n_vax==3), rounding_threshold),
      Dose_4 = plyr::round_any(sum(n_vax==4), rounding_threshold),
      Dose_5plus = plyr::round_any(sum(n_vax>=5), rounding_threshold),
      Median_dose_count = median(n_vax),
      Q1_dose_count = quantile(n_vax, probs = 0.25),
      Q3_dose_count = quantile(n_vax, probs = 0.75),
      Vax_past_12m = plyr::round_any(sum(vax_past_12m==1, na.rm=TRUE), rounding_threshold),
      Vax_past_24m = plyr::round_any(sum(vax_past_24m==1, na.rm=TRUE), rounding_threshold),
      Median_time_since_last_dose = round(median(time_since_last_vax_months, na.rm=TRUE),0),
      Q1_dose_time_since_last_dose = round(quantile(time_since_last_vax_months, probs = 0.25, na.rm=TRUE),0),
      Q3_dose_time_since_last_dose = round(quantile(time_since_last_vax_months, probs = 0.75, na.rm=TRUE),0),
    )
}

# Function to summarise vaccination history by subgroup and clean names
summarise_by_subgroup = function(data, subgroup, clean_name) {
  data %>% 
    group_by(get(subgroup)) %>% 
    summarise_vax_history() %>%
    mutate(Subgroup = clean_name, Level = `get(subgroup)`) %>% 
    select(-`get(subgroup)`)
}

# Table for whole population
tab_whole_pop <- data %>%
  group_by(agegroup_narrow) %>%
  summarise_vax_history() %>%
  mutate(Population = "All", Subgroup = "All", Level = agegroup_narrow) %>%
  select(-agegroup_narrow)

# Table for 50 to 64
tab_50to64 <- rbind(
  summarise_by_subgroup(data_50to64, "sex", "Sex"),
  summarise_by_subgroup(data_50to64, "ethnicity", "Ethnicity"),
  summarise_by_subgroup(data_50to64, "imd", "IMD quintile"),
  summarise_by_subgroup(data_50to64, "bmi", "Body Mass Index"),
  summarise_by_subgroup(data_50to64, "smoking_status_comb", "Smoking status"),
  summarise_by_subgroup(data_50to64, "asthma", "Asthma"),
  summarise_by_subgroup(data_50to64, "diabetes_controlled", "Diabetes"),
  summarise_by_subgroup(data_50to64, "ckd_rrt", "CKD or RRT"),
  summarise_by_subgroup(data_50to64, "organ_kidney_transplant", "Organ transplant"),
  summarise_by_subgroup(data_50to64, "bp_ht", "Hypertension"),
  summarise_by_subgroup(data_50to64, "chronic_respiratory_disease", "Chronic respiratory disease"),
  summarise_by_subgroup(data_50to64, "chronic_cardiac_disease", "Chronic cardiac disease"),
  summarise_by_subgroup(data_50to64, "cancer", "Cancer (non-haematological)"),
  summarise_by_subgroup(data_50to64, "haem_cancer", "Haematological malignancy"),
  summarise_by_subgroup(data_50to64, "chronic_liver_disease", "Chronic liver disease"),
  summarise_by_subgroup(data_50to64, "stroke", "Stroke"),
  summarise_by_subgroup(data_50to64, "dementia", "Dementia"),
  summarise_by_subgroup(data_50to64, "other_neuro", "Neurological disease"),
  summarise_by_subgroup(data_50to64, "asplenia", "Asplenia"),
  summarise_by_subgroup(data_50to64, "ra_sle_psoriasis", "Rheumatoid arthritis, lupus, or psoriasis"),
  summarise_by_subgroup(data_50to64, "immunosuppression", "Immunodeficiency"),
  summarise_by_subgroup(data_50to64, "learning_disability", "Learning disability"),
  summarise_by_subgroup(data_50to64, "sev_mental_ill", "Severe mental illness")
) %>%
  mutate(Population = "50-64")


# Table for 65+
tab_65plus <- rbind(
  summarise_by_subgroup(data_65plus, "sex", "Sex"),
  summarise_by_subgroup(data_65plus, "ethnicity", "Ethnicity"),
  summarise_by_subgroup(data_65plus, "imd", "IMD quintile"),
  summarise_by_subgroup(data_65plus, "bmi", "Body Mass Index"),
  summarise_by_subgroup(data_65plus, "smoking_status_comb", "Smoking status"),
  summarise_by_subgroup(data_65plus, "asthma", "Asthma"),
  summarise_by_subgroup(data_65plus, "diabetes_controlled", "Diabetes"),
  summarise_by_subgroup(data_65plus, "ckd_rrt", "CKD or RRT"),
  summarise_by_subgroup(data_65plus, "organ_kidney_transplant", "Organ transplant"),
  summarise_by_subgroup(data_65plus, "bp_ht", "Hypertension"),
  summarise_by_subgroup(data_65plus, "chronic_respiratory_disease", "Chronic respiratory disease"),
  summarise_by_subgroup(data_65plus, "chronic_cardiac_disease", "Chronic cardiac disease"),
  summarise_by_subgroup(data_65plus, "cancer", "Cancer (non-haematological)"),
  summarise_by_subgroup(data_65plus, "haem_cancer", "Haematological malignancy"),
  summarise_by_subgroup(data_65plus, "chronic_liver_disease", "Chronic liver disease"),
  summarise_by_subgroup(data_65plus, "stroke", "Stroke"),
  summarise_by_subgroup(data_65plus, "dementia", "Dementia"),
  summarise_by_subgroup(data_65plus, "other_neuro", "Neurological disease"),
  summarise_by_subgroup(data_65plus, "asplenia", "Asplenia"),
  summarise_by_subgroup(data_65plus, "ra_sle_psoriasis", "Rheumatoid arthritis, lupus, or psoriasis"),
  summarise_by_subgroup(data_65plus, "immunosuppression", "Immunodeficiency"),
  summarise_by_subgroup(data_65plus, "learning_disability", "Learning disability"),
  summarise_by_subgroup(data_65plus, "sev_mental_ill", "Severe mental illness")
) %>% 
  mutate(Population = "65+")

## Combine outputs
tab_combined <- rbind(tab_whole_pop, tab_50to64, tab_65plus) %>%
  relocate(Population, Subgroup, Level)

## Clean output
tab_combined_clean <- tab_combined %>%
  mutate(
    `0 dose` = paste0(Dose_0," (",round(Dose_0/N*100,1),"%)"),
    `1 dose` = paste0(Dose_1," (",round(Dose_1/N*100,1),"%)"),
    `2 dose` = paste0(Dose_2," (",round(Dose_2/N*100,1),"%)"),
    `3 dose` = paste0(Dose_3," (",round(Dose_3/N*100,1),"%)"),
    `4 dose` = paste0(Dose_4," (",round(Dose_4/N*100,1),"%)"),
    `5+ dose` = paste0(Dose_5plus," (",round(Dose_5plus/N*100,1),"%)"),
    `Median (IQR) dose count` = paste0(Median_dose_count," (",Q1_dose_count,"-",Q3_dose_count,")"),
    `Vaccinated in past 12 months, n (%)` = paste0(Vax_past_12m," (",round(Vax_past_12m/N*100,1),"%)"),
    `Vaccinated in past 24 months, n (%)` = paste0(Vax_past_24m," (",round(Vax_past_24m/N*100,1),"%)"),
    `Median (IQR) time in months to last dose` = paste0(Median_time_since_last_dose," (",Q1_dose_time_since_last_dose,"-",Q3_dose_time_since_last_dose,")")
  ) %>%
  select(Population, Subgroup, Level, N, 
         `0 dose`, `1 dose`, `2 dose`, `3 dose`, `4 dose`, `5+ dose`,
         `Median (IQR) dose count`, 
         `Vaccinated in past 12 months, n (%)`,
         `Vaccinated in past 24 months, n (%)`,
         `Median (IQR) time in months to last dose`
  )


## Save outputs
output_dir <- here("output", "snapshot_report")
fs::dir_create(output_dir)
write.csv(tab_combined, file = paste0(output_dir, "/snapshot_summary.csv"))
write.csv(tab_combined_clean, file = paste0(output_dir, "/snapshot_summary_clean.csv"))
