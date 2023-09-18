
library('tidyverse')
library('arrow')
library('here')
library('glue')


# Import custom user functions
source(here("analysis", "utility.R"))


remotes::install_github("https://github.com/wjchulme/dd4d")
library('dd4d')


population_size <- 1000

# get nth largest value from list
nthmax <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=TRUE), n)
}

nthmin <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=FALSE), n)
}

index_date <- as.Date("2020-01-01")
studystart_date <- as.Date("2020-12-07")

index_day <- 0L
studystart_day <- as.integer(studystart_date - index_date)

known_variables <- c(
  "index_date", "studystart_date",
  "index_day",  "studystart_day"
)

sim_list_varying_i <- function(i){

  vax_variable <- glue("covid_vax_{i}_day")

  lst(
    "deregistered_{i}_day" := bn_node(
      ~as.integer(runif(n=..n, index_day, index_day+1200)),
      missing_rate = ~0.99,
      needs = vax_variable
    ),

    "age_{i}" := bn_node(
      ~as.integer(rnorm(n=..n, mean=60, sd=14)),
      needs = vax_variable
    ),

    "registered_{i}" := bn_node(
      ~rbernoulli(n=..n, p = 0.99),
      needs = vax_variable
    ),

    "stp_{i}" := bn_node(
      ~factor(as.integer(runif(n=..n, 1, 36)), levels=1:36),
      needs = vax_variable
    ),

    "region_{i}" := bn_node(
      variable_formula = ~rfactor(n=..n, levels=c(
        "North East",
        "North West",
        "Yorkshire and The Humber",
        "East Midlands",
        "West Midlands",
        "East",
        "London",
        "South East",
        "South West"
      ), p = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05)),
      needs = vax_variable
    ),
  )
}

#sim_list_varying_i(1)

sim_list_varying = splice(
  sim_list_varying_i(1),
  sim_list_varying_i(2),
  sim_list_varying_i(3),
  sim_list_varying_i(4),
  sim_list_varying_i(5),
  sim_list_varying_i(6),
  sim_list_varying_i(7),
  sim_list_varying_i(8),
  sim_list_varying_i(9),
  sim_list_varying_i(10)
)

sim_list_vax_info = lst(

  ## vaccination variables

  # covid vax any
  covid_vax_1_day = bn_node(
    ~runif(n=..n, studystart_day, studystart_day+365),
    missing_rate = ~0.05,
  ),
  covid_vax_2_day = bn_node(
    ~runif(n=..n, covid_vax_1_day+14, covid_vax_1_day+100),
    missing_rate = ~0.001,
    needs = "covid_vax_1_day"
  ),
  covid_vax_3_day = bn_node(
    ~runif(n=..n, covid_vax_2_day+250, covid_vax_2_day+350),
    missing_rate = ~0.3,
    needs = "covid_vax_2_day"
  ),
  covid_vax_4_day = bn_node(
    ~runif(n=..n, covid_vax_3_day+300, covid_vax_3_day+400),
    missing_rate = ~0.7,
    needs = "covid_vax_3_day"
  ),
  covid_vax_5_day = bn_node(
    ~runif(n=..n, covid_vax_4_day+300, covid_vax_4_day+400),
    missing_rate = ~0.9,
    needs = "covid_vax_4_day"
  ),
  covid_vax_6_day = bn_node(
    ~runif(n=..n, covid_vax_5_day+300, covid_vax_5_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_5_day"
  ),
  covid_vax_7_day = bn_node(
    ~runif(n=..n, covid_vax_6_day+300, covid_vax_6_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_6_day"
  ),
  covid_vax_8_day = bn_node(
    ~runif(n=..n, covid_vax_7_day+300, covid_vax_7_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_7_day"
  ),
  covid_vax_9_day = bn_node(
    ~runif(n=..n, covid_vax_8_day+300, covid_vax_8_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_8_day"
  ),
  covid_vax_10_day = bn_node(
    ~runif(n=..n, covid_vax_9_day+300, covid_vax_9_day+400),
    missing_rate = ~0.99,
    needs = "covid_vax_9_day"
  ),

  covid_vax_type_1 = bn_node(~rcat(n=..n, c("pfizer","az"), c(0.5,0.5)), needs = "covid_vax_1_day"),
  covid_vax_type_2 = bn_node(~if_else(runif(..n)<0.98, covid_vax_type_1, "az"), needs = "covid_vax_2_day"),
  covid_vax_type_3 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_3_day"),
  covid_vax_type_4 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_4_day"),
  covid_vax_type_5 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_5_day"),
  covid_vax_type_6 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_6_day"),
  covid_vax_type_7 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_7_day"),
  covid_vax_type_8 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_8_day"),
  covid_vax_type_9 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_9_day"),
  covid_vax_type_10 = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), needs = "covid_vax_10_day"),

)


sim_list <- splice(
  sim_list_vax_info,
  sim_list_varying
)

bn <- bn_create(sim_list, known_variables = known_variables)

bn_plot(bn)
bn_plot(bn, connected_only=TRUE)

set.seed(10)

dummydata <-bn_simulate(bn, pop_size = population_size, keep_all = FALSE, .id="patient_id")


dummydata_processed <- dummydata %>%
  #convert logical to integer as study defs output 0/1 not TRUE/FALSE
  #mutate(across(where(is.logical), ~ as.integer(.))) %>%
  #convert integer days to dates since index date and rename vars
  mutate(across(ends_with("_day"), ~ as.Date(as.character(index_date + .)))) %>%
  rename_with(~str_replace(., "_day", "_date"), ends_with("_day")) %>%
  # convert vaccine product short names to full product names
  mutate(across(starts_with("covid_vax_type_"), ~fct_recode(., !!!set_names(names(vax_product_lookup), vax_product_lookup))))




fs::dir_create(here("lib", "dummydata"))
write_feather(dummydata_processed, sink = here("lib", "dummydata", "dummyinput_varying.feather"))


