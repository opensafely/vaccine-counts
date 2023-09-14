######################################

# This script provides the formal specification of the study data that will
# be extracted from the OpenSAFELY database.

######################################

# IMPORT STATEMENTS ----
# Import code building blocks from cohort extractor package
from cohortextractor import (
    StudyDefinition,
    patients,
    combine_codelists
)

# Import standard variable sets
from snapshot_demographic_vars import demographic_variables
from snapshot_comorbidity_vars import comorbidity_variables
import codelists

# Set snapshot date
snapshot_date = "2023-09-01"

# DEFINE STUDY POPULATION ----
# Define study population and variables
study = StudyDefinition(
    
    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": snapshot_date},
        "rate": "uniform",
        "incidence": 0.95,
    },
    
    # Set index date to snapshot date
    index_date=snapshot_date,
    # Define the study population
    # IN AND EXCLUSION CRITERIA
    # (= > 1 year follow up, aged > 18 and no missings in age and sex)
    # missings in age are the ones > 110
    # missings in sex can be sex = U or sex = I (so filter on M and F)
    population=patients.satisfying(
        """
        NOT died
        AND
        (age >=18 AND age <= 110)
        AND
        (sex = "M" OR sex = "F")
        AND
        index_of_multiple_deprivation != -1
        """,
        
        died=patients.died_from_any_cause(
            on_or_before="index_date",
            returning="binary_flag",
            return_expectations={"incidence": 0.01},
        ),
    ),
    
    
    # DEMOGRAPHICS
    **demographic_variables,
    
    # COMORBIDITIES
    **comorbidity_variables,
 
    # VACCINATION HISTORY
    # Date of first COVID vaccination -
    covid_vax_date_1=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["2020-12-01", snapshot_date],  # any dose recorded after 01/12/2020
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.8,
        },
    ),
    
    # Date of second COVID vaccination -
    covid_vax_date_2=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_1 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.6,
        },
    ),
    
    # Date of third COVID vaccination (primary or booster) -
    covid_vax_date_3=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_2 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
    
    # Date of fourth COVID vaccination (booster) -
    covid_vax_date_4=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_3 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
    
    # Date of fifth COVID vaccination (booster) -
    covid_vax_date_5=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_4 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
    
    # Date of sixth COVID vaccination (booster) -
    covid_vax_date_6=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_5 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
    
    # Date of seventh COVID vaccination (booster) -
    covid_vax_date_7=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["covid_vax_date_6 + 14 days", snapshot_date],  # from day after previous dose
        find_first_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
    
    # Date of most recent COVID vaccination
    covid_vax_date_most_recent=patients.with_tpp_vaccination_record(
        target_disease_matches="SARS-2 CORONAVIRUS",
        between=["2020-12-01", snapshot_date],
        find_last_match_in_period=True,
        returning="date",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2020-12-01", "latest": snapshot_date},
            "incidence": 0.5,
        },
    ),
)
