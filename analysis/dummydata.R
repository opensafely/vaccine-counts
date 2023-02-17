
library('tidyverse')
library('arrow')
library('here')
library('glue')

#source(here("analysis", "lib", "utility_functions.R"))

remotes::install_github("https://github.com/wjchulme/dd4d")
library('dd4d')


population_size <- 20000

# get nth largest value from list
nthmax <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=TRUE), n)
}

nthmin <- function(x, n=1){
  dplyr::nth(sort(x, decreasing=FALSE), n)
}

index_date <- as.Date("2020-01-01")

studystart_date <- as.Date("2020-12-07")

firstpfizer_date <- as.Date("2020-12-07")
firstaz_date <- as.Date("2021-01-04")
firstmoderna_date <- as.Date("2021-04-04")

index_day <- 0L
studystart_day <- as.integer(studystart_date - index_date)

firstpfizer_day <- as.integer(firstpfizer_date - index_date)
firstaz_day <- as.integer(firstaz_date - index_date)
firstmoderna_day <- as.integer(firstmoderna_date - index_date)


known_variables <- c(
  "index_date", "studystart_date", "firstpfizer_date", "firstaz_date", "firstmoderna_date",
  "index_day",  "studystart_day", "firstpfizer_day", "firstaz_day", "firstmoderna_day"
)

sim_list_fixed = lst(
  sex = bn_node(
    ~rfactor(n=..n, levels = c("F", "M"), p = c(0.51, 0.49)),
    missing_rate = ~0.001 # this is shorthand for ~(rbernoulli(n=..n, p = 0.2))
  ),
  death_day = bn_node(
    ~as.integer(runif(n=..n, index_day, index_day+2000)),
    missing_rate = ~0.99
  )
)


sim_list_varying_i <- function(i){

  vax_variable <- glue("any_covid_vax_{i}_day")

  lst(
    "dereg_{i}_day" := bn_node(
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

sim_list_varying_i(1)

sim_list_varying = splice(
  sim_list_varying_i(1),
  sim_list_varying_i(2),
  sim_list_varying_i(3),
  sim_list_varying_i(4),
  sim_list_varying_i(5),
  sim_list_varying_i(6),
  sim_list_varying_i(7)
)

sim_list_vax_info = lst(

  ## vaccination variables

  # covid vax any
  any_covid_vax_1_day = bn_node(
    ~runif(n=..n, studystart_day, studystart_day+365),
    missing_rate = ~0.05,
  ),
  any_covid_vax_2_day = bn_node(
    ~runif(n=..n, any_covid_vax_1_day+14, any_covid_vax_1_day+100),
    missing_rate = ~0.001,
    needs = "any_covid_vax_1_day"
  ),
  any_covid_vax_3_day = bn_node(
    ~runif(n=..n, any_covid_vax_2_day+250, any_covid_vax_2_day+350),
    missing_rate = ~0.3,
    needs = "any_covid_vax_2_day"
  ),
  any_covid_vax_4_day = bn_node(
    ~runif(n=..n, any_covid_vax_3_day+300, any_covid_vax_3_day+400),
    missing_rate = ~0.7,
    needs = "any_covid_vax_3_day"
  ),
  any_covid_vax_5_day = bn_node(
    ~runif(n=..n, any_covid_vax_4_day+300, any_covid_vax_4_day+400),
    missing_rate = ~0.9,
    needs = "any_covid_vax_4_day"
  ),
  any_covid_vax_6_day = bn_node(
    ~runif(n=..n, any_covid_vax_5_day+300, any_covid_vax_5_day+400),
    missing_rate = ~0.99,
    needs = "any_covid_vax_5_day"
  ),
  any_covid_vax_7_day = bn_node(
    ~runif(n=..n, any_covid_vax_6_day+300, any_covid_vax_6_day+400),
    missing_rate = ~0.99,
    needs = "any_covid_vax_6_day"
  ),
  # any_covid_vax_8_day = bn_node(
  #   ~runif(n=..n, any_covid_vax_7_day+300, any_covid_vax_7_day+400),
  #   missing_rate = ~0.99,
  #   needs = "any_covid_vax_7_day"
  # ),
  # any_covid_vax_9_day = bn_node(
  #   ~runif(n=..n, any_covid_vax_8_day+300, any_covid_vax_8_day+400),
  #   missing_rate = ~0.99,
  #   needs = "any_covid_vax_8_day"
  # ),
  # any_covid_vax_10_day = bn_node(
  #   ~runif(n=..n, any_covid_vax_9_day+300, any_covid_vax_9_day+400),
  #   missing_rate = ~0.99,
  #   needs = "any_covid_vax_9_day"
  # ),

  covid_vax_1_type = bn_node(~rcat(n=..n, c("pfizer","az"), c(0.5,0.5)), keep=FALSE),
  covid_vax_2_type = bn_node(~if_else(runif(..n)<0.98, covid_vax_1_type, "az"), keep=FALSE),
  covid_vax_3_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  covid_vax_4_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  covid_vax_5_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  covid_vax_6_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  covid_vax_7_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  # covid_vax_8_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  # covid_vax_9_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),
  # covid_vax_10_type = bn_node(~rcat(n=..n, c("pfizer","moderna"), c(0.5,0.5)), keep=FALSE),


  # pfizer
  covid_vax_pfizer_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="pfizer"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_pfizer_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="pfizer"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_pfizer_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="pfizer"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_pfizer_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="pfizer"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_pfizer_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="pfizer"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_pfizer_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="pfizer"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_pfizer_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="pfizer"),
    needs = "any_covid_vax_7_day"
  ),


  # az
  covid_vax_az_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="az"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_az_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="az"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_az_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="az"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_az_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="az"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_az_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="az"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_az_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="az"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_az_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="az"),
    needs = "any_covid_vax_7_day"
  ),


  # moderna
  covid_vax_moderna_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="moderna"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_moderna_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="moderna"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_moderna_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="moderna"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_moderna_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="moderna"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_moderna_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="moderna"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_moderna_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="moderna"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_moderna_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="moderna"),
    needs = "any_covid_vax_7_day"
  ),

  # pfizeromicron
  covid_vax_pfizeromicron_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="pfizeromicron"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_pfizeromicron_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="pfizeromicron"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_pfizeromicron_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="pfizeromicron"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_pfizeromicron_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="pfizeromicron"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_pfizeromicron_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="pfizeromicron"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_pfizeromicron_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="pfizeromicron"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_pfizeromicron_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="pfizeromicron"),
    needs = "any_covid_vax_7_day"
  ),

  # modernaomicron
  covid_vax_modernaomicron_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="modernaomicron"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_modernaomicron_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="modernaomicron"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_modernaomicron_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="modernaomicron"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_modernaomicron_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="modernaomicron"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_modernaomicron_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="modernaomicron"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_modernaomicron_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="modernaomicron"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_modernaomicron_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="modernaomicron"),
    needs = "any_covid_vax_7_day"
  ),

  # pfizerchildren
  covid_vax_pfizerchildren_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="pfizerchildren"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_pfizerchildren_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="pfizerchildren"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_pfizerchildren_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="pfizerchildren"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_pfizerchildren_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="pfizerchildren"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_pfizerchildren_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="pfizerchildren"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_pfizerchildren_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="pfizerchildren"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_pfizerchildren_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="pfizerchildren"),
    needs = "any_covid_vax_7_day"
  ),


  # az2
  covid_vax_az2_1_day = bn_node(
    ~any_covid_vax_1_day,
    missing_rate = ~1-(covid_vax_1_type=="az2"),
    needs = "any_covid_vax_1_day"
  ),
  covid_vax_az2_2_day = bn_node(
    ~any_covid_vax_2_day,
    missing_rate = ~1-(covid_vax_2_type=="az2"),
    needs = "any_covid_vax_2_day"
  ),
  covid_vax_az2_3_day = bn_node(
    ~any_covid_vax_3_day,
    missing_rate = ~1-(covid_vax_3_type=="az2"),
    needs = "any_covid_vax_3_day"
  ),
  covid_vax_az2_4_day = bn_node(
    ~any_covid_vax_4_day,
    missing_rate = ~1-(covid_vax_4_type=="az2"),
    needs = "any_covid_vax_4_day"
  ),
  covid_vax_az2_5_day = bn_node(
    ~any_covid_vax_5_day,
    missing_rate = ~1-(covid_vax_5_type=="az2"),
    needs = "any_covid_vax_5_day"
  ),
  covid_vax_az2_6_day = bn_node(
    ~any_covid_vax_6_day,
    missing_rate = ~1-(covid_vax_6_type=="az2"),
    needs = "any_covid_vax_6_day"
  ),
  covid_vax_az2_7_day = bn_node(
    ~any_covid_vax_7_day,
    missing_rate = ~1-(covid_vax_7_type=="az2"),
    needs = "any_covid_vax_7_day"
  ),

)


sim_list <- splice(
  sim_list_vax_info,
  sim_list_fixed,
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
  rename_with(~str_replace(., "_day", "_date"), ends_with("_day"))


fs::dir_create(here("lib", "dummydata"))
write_feather(dummydata_processed, sink = here("lib", "dummydata", "dummyinput.feather"))


