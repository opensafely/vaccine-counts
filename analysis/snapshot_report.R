######################################

# This script 
# - produces a table summarising selected clinical and demographic groups in study cohort, stratified by primary vaccine product
# - saves table as html

######################################

# Import libraries
library(tidyverse)
library(here)
library(glue)
library(dplyr)
library(gt)
library(gtsummary)
library(reshape2)

# Import filtered data
data <- read_rds(here::here("output", "snapshot", paste0("processed_snapshot.rds")))

# Recode n_vax as numeric
data$n_vax = as.numeric(as.character(data$n_vax))

# Define age subsets for stratified analysis
data_18to49 <- subset(data, agegroup_medium=="18-49")
data_50to64 <- subset(data, agegroup_medium=="50-64")
data_65to74 <- subset(data, agegroup_medium=="65-74")
data_75plus <- subset(data, agegroup_medium=="75+")

## Set rounding and redaction thresholds
rounding_threshold = 7
redaction_threshold = 7

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
      Median_time_since_last_dose_months = round(median(time_since_last_vax_months, na.rm=TRUE),1),
      Q1_time_since_last_dose_months = round(quantile(time_since_last_vax_months, probs = 0.25, na.rm=TRUE),1),
      Q3_time_since_last_dose_months = round(quantile(time_since_last_vax_months, probs = 0.75, na.rm=TRUE),1),
    )
}

# Function to summarise vaccination history by covariate and add clean names
summarise_by_covariate = function(data, covariate, clean_name) {
  data %>% 
    group_by(get(covariate)) %>% 
    summarise_vax_history() %>%
    mutate(Covariate = clean_name, Level = `get(covariate)`) %>% 
    select(-`get(covariate)`)
}

# Function to collate demographic and clinical data for input population
summarise_covariates = function(data, population_label) {
  rbind(
    summarise_by_covariate(data, "sex", "Sex"),
    summarise_by_covariate(data, "ethnicity", "Ethnicity (broad categories)"),
    summarise_by_covariate(data, "ethnicity_16", "Ethnicity (narrow categories)"),
    summarise_by_covariate(data, "imd", "IMD quintile"),
    summarise_by_covariate(data, "bmi", "Body Mass Index"),
    summarise_by_covariate(data, "smoking_status_comb", "Smoking status"),
    summarise_by_covariate(data, "asthma", "Asthma"),
    summarise_by_covariate(data, "diabetes_controlled", "Diabetes"),
    summarise_by_covariate(data, "ckd_rrt", "CKD or RRT"),
    summarise_by_covariate(data, "organ_kidney_transplant", "Organ transplant"),
    summarise_by_covariate(data, "bp_ht", "Hypertension"),
    summarise_by_covariate(data, "chronic_respiratory_disease", "Chronic respiratory disease"),
    summarise_by_covariate(data, "chronic_cardiac_disease", "Chronic cardiac disease"),
    summarise_by_covariate(data, "cancer", "Cancer (non-haematological)"),
    summarise_by_covariate(data, "haem_cancer", "Haematological malignancy"),
    summarise_by_covariate(data, "chronic_liver_disease", "Chronic liver disease"),
    summarise_by_covariate(data, "stroke", "Stroke"),
    summarise_by_covariate(data, "dementia", "Dementia"),
    summarise_by_covariate(data, "other_neuro", "Neurological disease"),
    summarise_by_covariate(data, "asplenia", "Asplenia"),
    summarise_by_covariate(data, "ra_sle_psoriasis", "Rheumatoid arthritis, lupus, or psoriasis"),
    summarise_by_covariate(data, "immunosuppression", "Immunodeficiency"),
    summarise_by_covariate(data, "learning_disability", "Learning disability"),
    summarise_by_covariate(data, "sev_mental_ill", "Severe mental illness")
) %>%
  mutate(Population = population_label)
}

# Table for whole population
tab_whole_pop <- data %>%
  group_by(agegroup_medium) %>%
  summarise_vax_history() %>%
  mutate(Population = "All", Covariate = "All", Level = agegroup_medium) %>%
  select(-agegroup_medium)

# Tabulate for population covariates
tab_18to49 <- summarise_covariates(data_18to49, "18-49")
tab_50to64 <- summarise_covariates(data_50to64, "50-64")
tab_65to74 <- summarise_covariates(data_65to74, "65-74")
tab_75plus <- summarise_covariates(data_75plus, "75+")

# Combine outputs
tab_combined <- rbind(tab_whole_pop, tab_18to49, tab_50to64, tab_65to74, tab_75plus) %>%
  relocate(Population, Covariate, Level)

# Redact summary metrics for groups with <100
tab_combined$Median_dose_count[tab_combined$N<100] = NA
tab_combined$Q1_dose_count[tab_combined$N<100] = NA
tab_combined$Q3_dose_count[tab_combined$N<100] = NA
tab_combined$Median_time_since_last_dose[tab_combined$N<100] = NA
tab_combined$Q1_dose_time_since_last_dose[tab_combined$N<100] = NA
tab_combined$Q3_dose_time_since_last_dose[tab_combined$N<100] = NA

# Redact rounded counts <= redaction threshold 
redact_count_column = function(data, col_name) {
  data[,col_name][data$N<100 | data[,col_name]<=redaction_threshold] = NA
  data
}

# Apply redaction to count columns
tab_combined = redact_count_column(tab_combined, "Dose_0")
tab_combined = redact_count_column(tab_combined, "Dose_1")
tab_combined = redact_count_column(tab_combined, "Dose_2")
tab_combined = redact_count_column(tab_combined, "Dose_3")
tab_combined = redact_count_column(tab_combined, "Dose_4")
tab_combined = redact_count_column(tab_combined, "Dose_5plus")
tab_combined = redact_count_column(tab_combined, "Vax_past_12m")
tab_combined = redact_count_column(tab_combined, "Vax_past_24m")

# Apply final redaction to N
tab_combined[,"N"][tab_combined[,"N"]<=redaction_threshold] = NA

# Drop dementia reporting for <65s
tab_combined = subset(tab_combined, !(Covariate=="Dementia" & Population %in% c("18-49","50-64")))

# Save outputs
output_dir <- here("output", "snapshot_report")
fs::dir_create(output_dir)
write.csv(tab_combined, file = paste0(output_dir, "/snapshot_summary.csv"))
saveRDS(object = tab_combined, file = paste0(output_dir, "/snapshot_summary.rds"), compress = TRUE)
