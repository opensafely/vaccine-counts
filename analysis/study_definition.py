
##########################
# extract patient information as at each vaccination date
##########################

from cohortextractor import (
  StudyDefinition,
  patients,
  codelist_from_csv,
  codelist,
  filter_codes_by_category,
  combine_codelists,
)

# Import codelists from codelists.py
import codelists


index_date = "2020-01-01"

## Functions for extracting a series of time dependent variables
# These define study defintion variable signatures such that
# variable_1_date is the the first event date on or after the index date
# variable_2_date is the first event date strictly after variable_2_date
# ...
# variable_n_date is the first event date strictly after variable_n-1_date


def vaccination_date_X(name, index_date, n, product_name_matches=None):
  # vaccination date, given product_name
  def var_signature(
    name,
    on_or_after,
    product_name_matches
  ):
    return {
      name: patients.with_tpp_vaccination_record(
        product_name_matches=product_name_matches,
        on_or_after=on_or_after,
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD"
      ),
    }
    
  variables = var_signature(f"{name}_1_date", index_date, product_name_matches)
  for i in range(2, n+1):
    variables.update(var_signature(
      f"{name}_{i}_date", 
      f"{name}_{i-1}_date + 1 days",
      # pick up subsequent vaccines occurring one day or later -- people with unrealistic dosing intervals are later excluded
      product_name_matches
    ))
  return variables


# get all vaccination dates, and patient characteristics as on those dates
def vaccination_info(index_date, n):

  def variable_signatures(i):

    if  (i==1):
      from_date = index_date
    else:
      from_date = f"any_covid_vax_{i-1}_date + 1 days"
    
    return {
    
      f"any_covid_vax_{i}_date": patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        on_or_after=from_date,
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD"
      ),
      
      # f"any_covid_vax_{i}_type": patients.with_tpp_vaccination_record(
      #   target_disease_matches="SARS-2 CORONAVIRUS",
      #   on_or_after=from_date,
      #   find_first_match_in_period=True,
      #   returning="product_name",
      #   date_format="YYYY-MM-DD"
      # ),
    
      f"age_{i}": patients.age_as_of( 
        f"any_covid_vax_{i}_date",
      ),
      f"registered_{i}": patients.registered_as_of(
        f"any_covid_vax_{i}_date",
      ),
      f"dereg_{i}_date": patients.date_deregistered_from_all_supported_practices(
        on_or_after=f"any_covid_vax_{i}_date",
        date_format="YYYY-MM-DD",
      ),
      f"stp_{i}": patients.registered_practice_as_of(
        f"any_covid_vax_{i}_date",
        returning="stp_code",
        return_expectations={
          "rate": "universal",
          "category": {
            "ratios": {
              "STP1": 0.1,
              "STP2": 0.1,
              "STP3": 0.1,
              "STP4": 0.1,
              "STP5": 0.1,
              "STP6": 0.1,
              "STP7": 0.1,
              "STP8": 0.1,
              "STP9": 0.1,
              "STP10": 0.1,
            }
          },
        },
      ),
      f"region_{i}": patients.registered_practice_as_of(
        f"any_covid_vax_{i}_date",
        returning="nuts1_region_name",
        return_expectations={
          "rate": "universal",
          "category": {
            "ratios": {
              "North East": 0.1,
              "North West": 0.1,
              "Yorkshire and The Humber": 0.2,
              "East Midlands": 0.1,
              "West Midlands": 0.1,
              "East": 0.1,
              "London": 0.1,
              "South East": 0.1,
              "South West": 0.1
              #"" : 0.01
            },
          },
        },
      ),
  
    }
  
  variable_dict = {}
  for i in range(1, n+1):
    variable_dict.update(variable_signatures(i))
  
  return variable_dict



# Specify study defeinition
study = StudyDefinition(
  
  # Configure the expectations framework
  default_expectations={
    "date": {"earliest": "2020-01-01", "latest": "2023-01-01"},
    "rate": "uniform",
    "incidence": 0.2,
    "int": {"distribution": "normal", "mean": 1000, "stddev": 100},
    "float": {"distribution": "normal", "mean": 25, "stddev": 5},
  },
  
  index_date = index_date,
  
  # This line defines the study population
  population=patients.all(),
  
    
  ###############################################################################
  ## non-date-specific info
  ###############################################################################
  
  sex = patients.sex(
    return_expectations={
      "rate": "universal",
      "category": {"ratios": {"M": 0.49, "F": 0.51}},
      "incidence": 1,
    }
  ),
  # All-cause death
  death_date=patients.died_from_any_cause(
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
  
  #################################################################
  ## Covid vaccine dates and info as at vax date
  #################################################################
  
  # any covid vaccine
  **vaccination_info(
    # use 1900 to capture all possible recorded covid vaccinations, including date errors
    # any vaccines occurring before national rollout can be later excluded if necessary
    index_date = "1900-01-01",
    n = 10
  ),
  
  ## all other known covid-19 vaccine product names in use
  # see this workspace https://jobs.opensafely.org/datalab/opensafely-internal/tpp-vaccination-names/ for latest known product names
  
  # pfizer
  **vaccination_date_X(
    name = "covid_vax_pfizer",
    index_date = "1900-01-01", 
    n = 10,
    product_name_matches="COVID-19 mRNA Vaccine Comirnaty 30micrograms/0.3ml dose conc for susp for inj MDV (Pfizer)"
  ),
  
  # az
  **vaccination_date_X(
    name = "covid_vax_az",
    index_date = "1900-01-01",
    n = 10,
    product_name_matches="COVID-19 Vaccine Vaxzevria 0.5ml inj multidose vials (AstraZeneca)"
  ),
  
  # moderna
  **vaccination_date_X(
    name = "covid_vax_moderna",
    index_date = "1900-01-01",
    n = 10,
    product_name_matches="COVID-19 mRNA Vaccine Spikevax (nucleoside modified) 0.1mg/0.5mL dose disp for inj MDV (Moderna)"
  ),
  
  # pfizer omicron
  **vaccination_date_X(
    name = "covid_vax_pfizeromicron",
    index_date = "1900-01-01", 
    n = 10,
    product_name_matches="Comirnaty Original/Omicron BA.1 COVID-19 Vacc md vials"
  ),
  
  # moderna omicron
  **vaccination_date_X(
    name = "covid_vax_modernaomicron",
    index_date = "1900-01-01",
    n = 10,
    product_name_matches="COVID-19 Vac Spikevax (Zero)/(Omicron) inj md vials"
  ),
  
  # pfizer children
  **vaccination_date_X(
    name = "covid_vax_pfizerchildren",
    index_date = "1900-01-01",
    n = 10,
    product_name_matches="COVID-19 mRNA Vaccine Comirnaty Children 5-11yrs 10mcg/0.2ml dose conc for disp for inj MDV (Pfizer)"
  ),
  
  #az half dose
  **vaccination_date_X(
    name = "covid_vax_az2",
    index_date = "1900-01-01",
    n = 10,
    product_name_matches="COVID-19 Vac AZD2816 (ChAdOx1 nCOV-19) 3.5x10*9 viral part/0.5ml dose sol for inj MDV (AstraZeneca)"
  ),
  
  
  
  
  
  
  
  
  
  


)
