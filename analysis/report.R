# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Purpose:  To assess distribution of vaccines schedules
#           in different population subgroups
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Import libraries ----
library('tidyverse')
library('lubridate')
library('glue')
library('here')


# Import custom user functions
source(here("analysis", "utility.R"))

# output processed data to rds ----
output_dir <- here("output", "report")
fs::dir_create(output_dir)

start_date=as.Date("2020-06-01")
end_date=as.Date("2023-09-01")
snapshot_date=as.Date("2023-09-01") # should be the same as the index date used in `study_definition_fixed.py`

# Import processed data ----
data_fixed <- read_rds(here("output", "process", "data_fixed.rds"))
data_varying <- read_rds(here("output", "process", "data_vax.rds"))
data_varying_clean <- read_rds(here("output", "process", "data_vax_clean.rds"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# processing ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## add time-invariant info to vaccine data and format variables for printing / plots ----

# only use first 6 vaccines in look up list, and set the rest to "unknown"
vax_type_lookup_6 <- vax_type_lookup[c(1:6, length(vax_type_lookup))]

data_vax <-
  left_join(
    data_varying,
    data_fixed %>% select(patient_id, sex, death_date),
    by="patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit =  "week", week_start = 1),
    vax_type = fct_recode(factor(vax_type, levels=vax_type_lookup_6), !!!vax_type_lookup_6),
    all=""
  )

data_vax_clean <-
  left_join(
    data_varying_clean,
    data_fixed %>% select(patient_id, sex, death_date),
    by="patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit =  "week", week_start = 1),
    vax_type = fct_recode(factor(vax_type, levels=vax_type_lookup_6), !!!vax_type_lookup_6),
    all=""
  )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# data validation checks ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## output vax date validation info ----

summary_validation <-
  data_vax %>%
  summarise(
    n=ceiling_any(n(), 100),
    n_missing_date=sum(is.na(vax_date)),
    pct_missing_date=mean(is.na(vax_date)),
    n_earlier_than_start_date=sum(vax_date<start_date, na.rm=TRUE),
    pct_earlier_than_start_date=mean(vax_date<start_date, na.rm=TRUE),
    n_interval_within_14days=sum(vax_interval<14, na.rm=TRUE),
    pct_interval_within_14days=mean(vax_interval<14, na.rm=TRUE)
  ) %>%
  ungroup()

write_csv(summary_validation, fs::path(output_dir, "validation.csv"))


## output vax date validation info, stratified by dose number and type ----

summary_validation_stratified <-
  data_vax %>%
  group_by(
    vax_index, vax_type
  ) %>%
  summarise(
    n=ceiling_any(n(), 100),
    n_missing_date=sum(is.na(vax_date)),
    pct_missing_date=mean(is.na(vax_date)),
    n_earlier_than_start_date=sum(vax_date<start_date, na.rm=TRUE),
    pct_earlier_than_start_date=mean(vax_date<start_date, na.rm=TRUE),
    n_interval_within_14days=sum(vax_interval<14, na.rm=TRUE),
    pct_interval_within_14days=mean(vax_interval<14, na.rm=TRUE)
  ) %>%
  ungroup()

write_csv(summary_validation_stratified, fs::path(output_dir, "validation_stratified.csv"))






# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Report info using characteristics recorded on each vaccination date ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

## note that all patient characteristics are determined as at the date of vaccination.
## for example, a person who moves from london to manchester between their first and second dose will be classed as in "london" for their first dose and "north west" for their second dose.


## output fully stratified vaccine counts ----

## this is useful for anyone wanting to externally re-construct different cuts of data for plotting etc

summary_stratified <-
  data_vax %>%
  group_by(
    vax_index, vax_type, vax_week,
    sex, ageband, region
  ) %>%
  summarise(
    n=ceiling_any(n(), 100)
  ) %>%
  ungroup()

write_csv(summary_stratified, fs::path(output_dir, "vax_counts_stratified.csv"))

## output plots of vaccine counts by type, dose number, and other characteristics ----

plot_vax_dates <- function(rows, cols){

  summary_by <- data_vax_clean %>%
    group_by(vax_type, vax_week) %>%
    group_by({{rows}}, {{cols}}, .add=TRUE) %>%
    summarise(
      n=roundmid_any(n(), 6)
    )


  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x=vax_week, y=n, fill=vax_type, group=vax_type),
      alpha=0.5,
      position=position_stack(reverse=TRUE),
      #position=position_identity(),
      width=7
    )+
    facet_grid(
      rows=vars({{rows}}),
      cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Date",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    scale_x_date(
      breaks=as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")),
      date_minor_breaks="month",
      date_labels="%Y", # labels = scales::label_date("%b"),
      # sec.axis = sec_axis(
      #   breaks=as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")),
      #   trans = ~as.Date(.),
      #   labels = scales::label_date("%Y")
      # )
    )+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      #axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name = deparse(substitute(rows))
  col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_dates_{row_name}_{col_name}.png")), plot=temp_plot)
}

plot_vax_dates(ageband, vax_dosenumber)
plot_vax_dates(region, vax_dosenumber)
plot_vax_dates(sex, vax_dosenumber)
plot_vax_dates(vax_dosenumber, all)

## output plots of time since previous vaccination by type, dose number, and other characteristics ----

plot_vax_intervals <- function(rows, cols){

  summary_by <- data_vax_clean %>%
    filter(vax_index != 1) %>%
    mutate(
      vax_interval = roundmid_any(vax_interval+1, 7) #to split into 0-6, 7-13, 14-20, 21-28, ....
    ) %>%
    group_by(vax_dosenumber, vax_type, vax_interval) %>%
    group_by({{rows}}, {{cols}}, .add=TRUE) %>%
    summarise(
      n=roundmid_any(n(), 6)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x=vax_interval, y=n, fill=vax_type, group=vax_type),
      alpha=0.5,
      position=position_stack(reverse=TRUE),
      #position=position_identity(),
      width=7
    )+
    facet_grid(
      rows=vars({{rows}}),
      cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Interval (days)",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    scale_x_continuous(
      breaks = (0:100)*4*7,
      #limits = c(0, NA),
      sec.axis = sec_axis(
        trans = ~./7
      )
    )+
    #scale_y_continuous(limits=c(0,100))+
    theme_minimal()+
    theme(
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.ticks.x = element_line(),
      #axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  print(temp_plot)

  row_name = deparse(substitute(rows))
  col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("vax_intervals_{row_name}_{col_name}.png")), plot=temp_plot)
}

plot_vax_intervals(ageband, vax_dosenumber)
plot_vax_intervals(region, vax_dosenumber)
plot_vax_intervals(sex, vax_dosenumber)
plot_vax_intervals(vax_dosenumber, all)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Report vaccination info stratifying by characteristics recorded on the "snapshot_date" ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

default_date <- as.Date("2020-06-01") # date used for unvaccinated people

data_last_vax_date_clean <-
  data_vax_clean %>%
  group_by(patient_id) %>%
  filter(vax_index == max(vax_index)) %>%
  select(
    patient_id,
    vax_index,
    vax_date,
    vax_type,
    vax_dosenumber,
    vax_week
  )

# check there's only one patient per row:
check_1rpp <-
  data_last_vax_date_clean %>%
  group_by(patient_id) %>%
  filter(row_number()!=1)
stopifnot("data_last_vax_date_clean should not have multiple rows per patient" = nrow(check_1rpp)==0)

data_snapshot <-
  left_join(
    data_fixed %>%
      filter(
        registered,
        is.na(death_date) | (death_date >= snapshot_date),
      ) %>%
      select(
        patient_id,
        sex,
        age,
        ageband,
        region,
        stp
      ),
    data_last_vax_date_clean,
    by="patient_id"
  ) %>%
  mutate(
    vax_index = replace_na(vax_index, 0L),
    vax_type = fct_explicit_na(vax_type, "unvaccinated"),
    #last_vax_date = replace_na(vax_date, default_date), # replace "no first dose" with "2020-12-01" and relabel later
    last_vax_date = if_else(vax_index==0, default_date+as.integer(runif(n(),0,170)), vax_date),
    vax_dosenumber = fct_explicit_na(vax_dosenumber, "Unvaccinated"),
    last_vax_week = floor_date(last_vax_date, unit =  "week", week_start = 1),
    all=""
  )


## output plots of date of last dose by type and other characteristics ----


plot_date_of_last_dose <- function(rows){

  summary_by <- data_snapshot %>%
    group_by(vax_type, last_vax_week) %>%
    group_by({{rows}}, .add=TRUE) %>%
    summarise(
      n=roundmid_any(n(), 6)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x=last_vax_week, y=n, fill=vax_type, group=vax_type),
      alpha=0.5,
      position=position_stack(reverse=TRUE),
      #position=position_identity(),
      width=7
    )+
    facet_grid(
      rows=vars({{rows}}),
      #cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Date of most recent COVID-19 vaccine",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    scale_x_date(
      breaks=c(default_date-30, as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01"))),
      date_minor_breaks="month",
      #labels = ~{c("Unvaccinated", scales::label_date("%Y")(.x[-1]))},
      labels = c("Unvaccinated", scales::label_date("%Y")(as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")))),
    )+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      axis.ticks.x = element_line(),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  print(temp_plot)

  row_name = deparse(substitute(rows))
  #col_name = deparse(substitute(cols))

  ggsave(fs::path(output_dir, glue("last_vax_date_{row_name}.png")), plot=temp_plot)
}

plot_date_of_last_dose(ageband)
plot_date_of_last_dose(region)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(all)










