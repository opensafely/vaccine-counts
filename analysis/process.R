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

start_date=as.Date("2020-06-01")
end_date=as.Date("2022-12-31")

## output processed data to rds ----
fs::dir_create(here("output", "data"))

## Import dataprocess ----

# use externally created dummy data if not running in the server
# check variables are as they should be
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){

  # ideally in future this will check column existence and types from metadata,
  # rather than from a cohort-extractor-generated dummy data

  data_studydef_dummy <- read_feather(here("output", "input.feather")) %>%
    # because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"), ~ as.Date(.))) %>%
    mutate(patient_id = as.integer(patient_id))

  data_custom_dummy <- read_feather(here("lib", "dummydata", "dummyinput.feather"))

  not_in_studydef <- names(data_custom_dummy)[!( names(data_custom_dummy) %in% names(data_studydef_dummy) )]
  not_in_custom  <- names(data_studydef_dummy)[!( names(data_studydef_dummy) %in% names(data_custom_dummy) )]

  if(length(not_in_custom)!=0) stop(
    paste(
      "These variables are in studydef but not in custom: ",
      paste(not_in_custom, collapse=", ")
    )
  )

  if(length(not_in_studydef)!=0) stop(
    paste(
      "These variables are in custom but not in studydef: ",
      paste(not_in_studydef, collapse=", ")
    )
  )

  # reorder columns
  data_studydef_dummy <- data_studydef_dummy[ , names(data_custom_dummy)]

  unmatched_types <- cbind(
    map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")),
    map_chr(data_custom_dummy, ~paste(class(.), collapse=", "))
  )[ (map_chr(data_studydef_dummy, ~paste(class(.), collapse=", ")) != map_chr(data_custom_dummy, ~paste(class(.), collapse=", ")) ), ] %>%
    as.data.frame() %>%
    rownames_to_column()


  if(nrow(unmatched_types)>0) stop(
    #unmatched_types
    "inconsistent typing in studydef : dummy dataset\n",
    apply(unmatched_types, 1, function(row) paste(paste(row, collapse=" : "), "\n"))
  )

  data_extract <- data_custom_dummy
} else {
  data_extract <- read_feather(here("output", "input.feather")) %>%
    #because date types are not returned consistently by cohort extractor
    mutate(across(ends_with("_date"),  as.Date))
}





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

data_processed <- data_extract %>%
  mutate(

    sex = case_when(
      sex == "F" ~ "Female",
      sex == "M" ~ "Male",
      #sex == "I" ~ "Inter-sex",
      #sex == "U" ~ "Unknown",
      TRUE ~ NA_character_
    ) %>% factor(),

    !!!standardise_characteristics(1),
    !!!standardise_characteristics(2),
    !!!standardise_characteristics(3),
    !!!standardise_characteristics(4),
    !!!standardise_characteristics(5),
    !!!standardise_characteristics(6),
    !!!standardise_characteristics(7),
  )

data_processed %>%
  select(
    patient_id,
    sex,
    death_date
  ) %>%
write_rds(here("output", "data", "data_fixed.rds"), compress="gz")


# reshape vaccination data ----

data_vax_any <-
  data_processed %>%
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
  data_processed %>%
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
    type = replace_na(vax_type, "other"),
  ) %>%
  arrange(patient_id, vax_date) %>%
  group_by(patient_id) %>%
  mutate(
    vax_interval = as.integer(vax_date - lag(vax_date,1))
  ) %>%
  ungroup()


write_rds(data_vax_all, here("output", "data", "data_vax_all.rds"), compress="gz")

data_vax_all_clean <-
  # remove vaccine events occurring within 14 days of a previous vaccine event
  data_vax_all %>%
  filter(
    !is.na(vax_date),
    is.na(vax_interval) | vax_interval=>14,
    vax_date >= start_date,
    vax_date <= end_date
  ) %>%
  group_by(patient_id) %>%
  mutate(
    vax_index = row_number()
  ) %>%
  ungroup()

write_rds(data_vax_all_clean, here("output", "data", "data_vax_all_clean.rds"), compress="gz")




