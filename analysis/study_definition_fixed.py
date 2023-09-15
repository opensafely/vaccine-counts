
##########################
# extract patient information as at a fixed date, with vaccination dates ascertained retrospectively
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


index_date = "2023-09-01"

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
  
  age = patients.age_as_of( 
    index_date,
  ),
  registered = patients.registered_as_of(
    index_date
  ),
  stp = patients.registered_practice_as_of(
    index_date,
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
  region = patients.registered_practice_as_of(
    index_date,
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

  # All-cause death
  death_date=patients.died_from_any_cause(
    returning="date_of_death",
    date_format="YYYY-MM-DD",
  ),
  
)
