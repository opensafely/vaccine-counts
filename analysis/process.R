######################################

# This script:
# imports vaccination date data extracted by cohort extractor
# organises vaccination date data to "vax X type", "vax X date" (rather than "pfizer X date", "az X date", ...)
######################################

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('lubridate')
library('arrow')
library('here')
library('glue')

# Import custom user functions
source(here("analysis", "utility.R"))


start_date=as.Date("2020-06-01")
end_date=as.Date("2023-12-31")

## output processed data to rds ----
output_dir <- here("output", "process")
fs::dir_create(output_dir)


## Import fixed data ----

data_extract_fixed <-
  import_extract(
    here("lib", "dummydata", "dummyinput_fixed.feather"),
    here("output", "input_fixed.feather")
  )

data_processed_fixed <- data_extract_fixed %>%
  mutate(

    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      #sex == "I" ~ "Inter-sex",
      #sex == "U" ~ "Unknown",
      TRUE ~ NA_character_
    ) %>% factor(),

    ageband = cut(
      age,
      breaks=c(-Inf, 18, 40, 55, 65, 75, Inf),
      labels=c("under 18", "18-39", "40-54", "55-64", "65-74", "75+"),
      right=FALSE
    ),
    region= fct_collapse(
      region,
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    )
  )

data_processed_fixed %>%
  select(
    patient_id,
    registered,
    sex,
    age,
    ageband,
    region,
    stp,
    death_date
  ) %>%
  write_rds(fs::path(output_dir, "data_fixed.rds"), compress="gz")


## delete in-memory objects to save space
rm(data_processed_fixed)
rm(data_extract_fixed)



## Import time-varying data ----


data_extract_varying <-
  import_extract(
    here("lib", "dummydata", "dummyinput_varying.feather"),
    here("output", "input_varying.feather")
  )


standardise_characteristics <- function(i){
  rlang::quos(

    "ageband_{i}" := cut(
      .data[[glue("age_{i}")]],
      breaks=c(-Inf, 18, 40, 55, 65, 75, Inf),
      labels=c("under 18", "18-39", "40-54", "55-64", "65-74", "75+"),
      right=FALSE
    ),
    "region_{i}" := fct_collapse(
      .data[[glue("region_{i}")]],
      `East of England` = "East",
      `London` = "London",
      `Midlands` = c("West Midlands", "East Midlands"),
      `North East and Yorkshire` = c("Yorkshire and The Humber", "North East"),
      `North West` = "North West",
      `South East` = "South East",
      `South West` = "South West"
    )
  )
}

data_processed_fixed <- data_extract_varying %>%
  mutate(

    !!!standardise_characteristics(1),
    !!!standardise_characteristics(2),
    !!!standardise_characteristics(3),
    !!!standardise_characteristics(4),
    !!!standardise_characteristics(5),
    !!!standardise_characteristics(6),
    !!!standardise_characteristics(7),
    !!!standardise_characteristics(8),
    !!!standardise_characteristics(9),
    !!!standardise_characteristics(10),
  )


# reshape vaccination data ----

data_vax_any <-
  data_processed_fixed %>%
  select(
    patient_id,
    matches("any_covid_vax\\_\\d+\\_date"),
    #matches("product_type_\\d+\\"),
    matches("registered_\\d+"),
    matches("dereg_\\d+"),
    matches("age_\\d+"),
    matches("ageband_\\d+"),
    matches("region_\\d+"),
    matches("stp_\\d+"),
  ) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c(".value", "vax_index"),
    names_pattern = "^(.*)_(\\d+)",
    values_drop_na = TRUE,
    names_transform = list(vax_index = as.integer)
  ) %>%
  rename(vax_date = any_covid_vax) %>%
  arrange(patient_id, vax_date)

data_vax_type <-
  data_processed_fixed %>%
  select(patient_id, matches("^covid\\_\\w+\\_\\d+\\_date")) %>%
  pivot_longer(
    cols = -patient_id,
    names_to = c(NA, "vax_type", "vax_index"),
    names_pattern = "^(.*)_(\\w+)_(\\d+)_date",
    values_to = "date",
    values_drop_na = TRUE,
  ) %>%
  rename(vax_date = date) %>%
  arrange(patient_id, vax_date)

data_vax_all <-
  left_join(
    data_vax_any,
    data_vax_type %>% select(-vax_index),
    by=c("patient_id", "vax_date")
  ) %>%
  mutate(
    vax_type = replace_na(vax_type, "other"),
  ) %>%
  arrange(patient_id, vax_date) %>%
  group_by(patient_id) %>%
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date,1))
  ) %>%
  ungroup()


write_rds(data_vax_all, fs::path(output_dir, "data_vax_all.rds"), compress="gz")

data_vax_all_clean <-
  # remove vaccine events occurring within 14 days of a previous vaccine event
  data_vax_all %>%
  filter(
    !is.na(vax_date),
    is.na(vax_interval) | vax_interval>=14,
    vax_date >= start_date,
    vax_date <= end_date
  ) %>%
  group_by(patient_id) %>%
  mutate(
    vax_index = row_number()
  ) %>%
  ungroup()

write_rds(data_vax_all_clean, fs::path(output_dir, "data_vax_all_clean.rds"), compress="gz")




