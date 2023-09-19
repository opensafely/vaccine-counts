
library('tidyverse')
library('arrow')
library('here')
library('glue')

#source(here("analysis", "lib", "utility_functions.R"))

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

index_date <- as.Date("2023-01-01")

index_day <- 0L

known_variables <- c(
  "index_date",
  "index_day",
)

sim_list = lst(

  registered = bn_node(
    ~rbernoulli(n=..n, p = 0.99),
  ),

  sex = bn_node(
    ~rfactor(n=..n, levels = c("F", "M"), p = c(0.51, 0.49)),
    missing_rate = ~0.001 # this is shorthand for ~(rbernoulli(n=..n, p = 0.2))
  ),

  age = bn_node(
    ~as.integer(rnorm(n=..n, mean=60, sd=14)),
  ),

  stp = bn_node(
    ~factor(as.integer(runif(n=..n, 1, 36)), levels=1:36),
  ),

  region = bn_node(
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
  ),

  death_day = bn_node(
    ~as.integer(runif(n=..n, index_day, index_day+2000)),
    missing_rate = ~0.99
  )
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
write_feather(dummydata_processed, sink = here("lib", "dummydata", "dummyinput_fixed.feather"))


