# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Purpose:  To assess distribution of vaccines schedules
#           in different population subgroups
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Import libraries ----
library('tidyverse')
library('lubridate')
library('glue')
library('here')

# output processed data to rds ----
output_dir <- here("output", "report")
fs::dir_create(output_dir)

start_date=as.Date("2020-06-01")
end_date=as.Date("2023-09-01")

roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

ceiling_any <- function(x, to=1){
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x/to, 1/100000000))*to
}

# Import processed data ----
data_fixed <- read_rds(here("output", "data", "data_fixed.rds"))
data_vax_all <- read_rds(here("output", "data", "data_vax_all.rds"))
data_vax_all_clean <- read_rds(here("output", "data", "data_vax_all_clean.rds"))


vax_type_lookup = c(
  "BNT162b2"="pfizer",
  "ChAdOx1"="az",
  "mRNA-1273"="moderna",
  "BNT162b2/omicron"="pfizeromicron",
  "mRNA-1273/omicron"="modernaomicron",
  "BNT162b2/children"="pfizerchildren",
  "ChAdOx1/2"="az2",
  "Other"="other"
)

data_vax <-
  left_join(
    data_vax_all,
    data_fixed,
    by="patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit =  "week", week_start = 1),
    vax_type = fct_recode(factor(vax_type, levels=vax_type_lookup), !!!vax_type_lookup),
    all=""
  )

data_vax_clean <-
  left_join(
    data_vax_all_clean,
    data_fixed,
    by="patient_id"
  ) %>%
  mutate(
    vax_dosenumber = factor(vax_index, levels = sort(unique(vax_index)), labels = paste("Dose", sort(unique(vax_index)))),
    vax_week = floor_date(vax_date, unit =  "week", week_start = 1),
    vax_type = fct_recode(factor(vax_type, levels=vax_type_lookup), !!!vax_type_lookup),
    all=""
  )


## note that all patient characteristics are determined as at the date of vaccination.
## for example, a person who moves from london to manchester between their first and second dose will be classed as in "london" for their first dose and "north west" for their second dose.
##



# output vax date validation info ----

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

write_csv(summary_validation_stratified, here("output", "report", "validation_stratified.csv"))

# output vax date validation info ----

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

write_csv(summary_validation, here("output", "report", "validation.csv"))



# output fully stratified vaccine counts ----

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

write_csv(summary_stratified, here("output", "report", "vax_counts_stratified.csv"))

# output plots of vaccine counts by type, dose number, and other characteristics ----


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
    #scale_y_continuous(limits=c(0,100))+
    scale_x_date(
      breaks=as.Date(c("2021-01-01","2021-07-01","2022-01-01","2022-07-01","2023-01-01","2023-07-01","2024-01-01")),
      date_minor_breaks="month",
      date_labels="%b", # labels = scales::label_date("%b"),
      sec.axis = sec_axis(
        breaks=as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")),
        trans = ~as.Date(.),
        labels = scales::label_date("%Y")
      )
    )+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name = deparse(substitute(rows))
  col_name = deparse(substitute(cols))

  ggsave(here("output","report", glue("vax_dates_{row_name}_{col_name}.png")), plot=temp_plot)
}


plot_vax_dates(ageband, vax_dosenumber)
plot_vax_dates(region, vax_dosenumber)
plot_vax_dates(sex, vax_dosenumber)
plot_vax_dates(vax_dosenumber, all)

# output plots of time since previous vaccination by type, dose number, and other characteristics ----


plot_vax_intervals <- function(rows, cols){

  summary_by <- data_vax_clean %>%
    filter(vax_index != 1) %>%
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
      width=1
    )+
    facet_grid(
      rows=vars({{rows}}),
      cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Interval",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    #scale_y_continuous(limits=c(0,100))+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name = deparse(substitute(rows))
  col_name = deparse(substitute(cols))

  ggsave(here("output","report", glue("vax_intervals_{row_name}_{col_name}.png")), plot=temp_plot)
}

plot_vax_intervals(ageband, vax_dosenumber)
plot_vax_intervals(region, vax_dosenumber)
plot_vax_intervals(sex, vax_dosenumber)
plot_vax_intervals(vax_dosenumber, all)



# output plots of time since most recent vaccination by type and other characteristics ----

plot_date_since_last_dose <- function(rows, cols){

  summary_by <- data_vax_clean %>%
    group_by(patient_id) %>%
    filter(vax_index == max(vax_index)) %>%
    mutate(
      days_since_last_dose = as.integer(end_date - pmax(vax_date, as.Date("2020-12-01"), na.rm=TRUE))
    ) %>%
    group_by(vax_type, days_since_last_dose) %>%
    group_by({{rows}}, {{cols}}, .add=TRUE) %>%
    summarise(
      n=roundmid_any(n(), 6)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x=days_since_last_dose, y=n, fill=vax_type, group=vax_type),
      alpha=0.5,
      position=position_stack(reverse=TRUE),
      #position=position_identity(),
      width=1
    )+
    facet_grid(
      rows=vars({{rows}}),
      cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Days since most recent COVID-19 vaccine receipt",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    scale_x_continuous(breaks=as.integer((0:5)*365.25))+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name = deparse(substitute(rows))
  col_name = deparse(substitute(cols))

  ggsave(here("output","report", glue("days_time_last_dose_{row_name}_{col_name}.png")), plot=temp_plot)
}

plot_days_since_last_dose(ageband, all)
plot_days_since_last_dose(region, all)
plot_days_since_last_dose(sex, all)
#plot_days_since_last_dose(all, all)



# output plots of date of last dose by type and other characteristics ----

plot_date_of_last_dose <- function(rows){

  default_date <- as.Date("2020-12-01")

  summary_by <- data_vax_clean %>%
    group_by(patient_id) %>%
    filter(vax_index == max(vax_index)) %>%
    mutate(
      last_vax_date = pmax(vax_date, default_date, na.rm=TRUE) # replace "no first dose" with "2020-12-01" and relabel later
    ) %>%
    group_by(vax_type, last_vax_date) %>%
    group_by({{rows}}, .add=TRUE) %>%
    summarise(
      n=roundmid_any(n(), 6)
    )

  temp_plot <-
    ggplot(summary_by) +
    geom_col(
      aes(x=last_vax_date, y=n, fill=vax_type, group=vax_type),
      alpha=0.5,
      position=position_stack(reverse=TRUE),
      #position=position_identity(),
      width=1
    )+
    facet_grid(
      rows=vars({{rows}}),
      #cols=vars({{cols}}),
      switch="y",
      space="free_x",
      scales="free_x"
    )+
    labs(
      x="Days since most recent COVID-19 vaccine receipt",
      y=NULL,
      fill=NULL
    )+
    scale_fill_brewer(palette="Set2")+
    scale_x_date(
      breaks=c(default_date, as.Date(c("2021-01-01","2021-07-01","2022-01-01","2022-07-01","2023-01-01","2023-07-01","2024-01-01"))),
      date_minor_breaks="month",
      labels = ~{c("Unvaccinated", scales::label_date("%b")(.x[-1]))},
      sec.axis = sec_axis(
        breaks=as.Date(c("2021-01-01","2022-01-01","2023-01-01","2024-01-01")),
        trans = ~as.Date(.),
        labels = scales::label_date("%Y")
      )
    )+
    theme_minimal()+
    theme(
      axis.text.x.top=element_text(hjust=0),
      axis.text.x.bottom=element_text(hjust=0),
      strip.text.y.left = element_text(angle = 0, hjust=1),
      strip.placement = "outside",
      axis.text.y = element_blank(),
      legend.position = "bottom"
    )

  #print(temp_plot)

  row_name = deparse(substitute(rows))
  #col_name = deparse(substitute(cols))

  ggsave(here("output","report", glue("last_vax_date_{row_name}.png")), plot=temp_plot)
}

plot_date_of_last_dose(ageband)
plot_date_of_last_dose(region)
plot_date_of_last_dose(sex)
plot_date_of_last_dose(all)
