# Adapted from https://github.com/opensafely/covid_mortality_over_time
# Modifications: addition of ethnicity and IMD decile; removal of msoa, stp, and rurality; modification of age bands
# Define demographic variables

from cohortextractor import (
    patients,
    filter_codes_by_category,
)

import codelists

demographic_variables = dict(
    # Follow-up
    has_follow_up=patients.registered_with_one_practice_between(
            "index_date - 3 months", "index_date"
    ),

    # Age
    age=patients.age_as_of(
        "index_date",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),
    
    # Age group in narrow bands
    agegroup_narrow=patients.categorised_as(
        {
            "18-49": "age >= 18 AND age < 50",
            "50-54": "age >= 50 AND age < 55",
            "55-59": "age >= 55 AND age < 60",
            "60-64": "age >= 60 AND age < 65",
            "65-69": "age >= 65 AND age < 70",
            "70-74": "age >= 70 AND age < 75",
            "75-79": "age >= 75 AND age < 80",
            "80plus": "age >= 80",
            "missing": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "18-49": 0.3,
                    "50-54": 0.1,
                    "55-59": 0.1,
                    "60-64": 0.1,
                    "65-69": 0.1,
                    "70-74": 0.1,
                    "75-79": 0.1,
                    "80plus": 0.1,
                    "missing": 0,
                }
            },
        },
    ),
    
    # Age group in medium bands
    agegroup_medium=patients.categorised_as(
        {
            "18-49": "age >= 18 AND age < 50",
            "50-64": "age >= 50 AND age < 65",
            "65-74": "age >= 65 AND age < 75",
            "75plus": "age >= 75",
            "missing": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "18-49": 0.3,
                    "50-64": 0.3,
                    "65-74": 0.2,
                    "75plus": 0.2,
                    "missing": 0,
                }
            },
        },
    ),
    
    # Age group in broad bands
    agegroup_broad=patients.categorised_as(
        {
            "18-49": "age >= 18 AND age < 50",
            "50-64": "age >= 50 AND age < 65",
            "65plus": "age >= 65",
            "missing": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "18-49": 0.3,
                    "50-64": 0.3,
                    "65plus": 0.4,
                    "missing": 0,
                }
            },
        },
    ),
    
    # Sex
    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        }
    ),
    
    # Ethnicity
    ethnicity_primary=patients.with_these_clinical_events(
        codelists.ethnicity_codes,
        returning="category",
        on_or_before="index_date",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={
                                "category": {
                                    "ratios": {
                                        "1": 0.2,
                                        "2": 0.2,
                                        "3": 0.2,
                                        "4": 0.2,
                                        "5": 0.2
                                        }
                                    },
                                "incidence": 0.75,
                                },
    ),
    ethnicity_sus=patients.with_ethnicity_from_sus(
        returning="group_6",
        use_most_frequent_code=True,
        return_expectations={
            "category": {
                            "ratios": {
                                "1": 0.2,
                                "2": 0.2,
                                "3": 0.2,
                                "4": 0.2,
                                "5": 0.2
                                }
                            },
            "incidence": 0.4,
            },
    ),
    ethnicity=patients.categorised_as(
            {
                "0": "DEFAULT",
                "1": "ethnicity_primary='1' OR (NOT ethnicity_primary AND ethnicity_sus='1')",
                "2": "ethnicity_primary='2' OR (NOT ethnicity_primary AND ethnicity_sus='2')",
                "3": "ethnicity_primary='3' OR (NOT ethnicity_primary AND ethnicity_sus='3')",
                "4": "ethnicity_primary='4' OR (NOT ethnicity_primary AND ethnicity_sus='4')",
                "5": "ethnicity_primary='5' OR (NOT ethnicity_primary AND ethnicity_sus='5')",
            },
            return_expectations={
                "category": {
                                "ratios": {
                                    "0": 0.5,  # missing in 50%
                                    "1": 0.1,
                                    "2": 0.1,
                                    "3": 0.1,
                                    "4": 0.1,
                                    "5": 0.1
                                    }
                                },
                "rate": "universal",
            },
    ),
    
    # Ethnicity in 16 categories
    ethnicity_primary_16=patients.with_these_clinical_events(
        codelists.ethnicity_codes_16,
        returning="category",
        on_or_before="index_date",
        find_last_match_in_period=True,
        include_date_of_match=False,
        return_expectations={
                                "category": {
                                    "ratios": {
                                        "1": 0.0625,
                                        "2": 0.0625,
                                        "3": 0.0625,
                                        "4": 0.0625,
                                        "5": 0.0625,
                                        "6": 0.0625,
                                        "7": 0.0625,
                                        "8": 0.0625,
                                        "9": 0.0625,
                                        "10": 0.0625,
                                        "11": 0.0625,
                                        "12": 0.0625,
                                        "13": 0.0625,
                                        "14": 0.0625,
                                        "15": 0.0625,
                                        "16": 0.0625,
                                        }
                                    },
                                "incidence": 0.75,
                                },
    ),
    ethnicity_sus_16=patients.with_ethnicity_from_sus(
        returning="group_16",
        use_most_frequent_code=True,
        return_expectations={
            "category": {
                            "ratios": {
                                "1": 0.0625,
                                "2": 0.0625,
                                "3": 0.0625,
                                "4": 0.0625,
                                "5": 0.0625,
                                "6": 0.0625,
                                "7": 0.0625,
                                "8": 0.0625,
                                "9": 0.0625,
                                "10": 0.0625,
                                "11": 0.0625,
                                "12": 0.0625,
                                "13": 0.0625,
                                "14": 0.0625,
                                "15": 0.0625,
                                "16": 0.0625,
                                }
                            },
            "incidence": 0.4,
            },
    ),
    ethnicity_16=patients.categorised_as(
            {
                "0": "DEFAULT",
                "1": "ethnicity_primary_16='1' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='1')",
                "2": "ethnicity_primary_16='2' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='2')",
                "3": "ethnicity_primary_16='3' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='3')",
                "4": "ethnicity_primary_16='4' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='4')",
                "5": "ethnicity_primary_16='5' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='5')",
                "6": "ethnicity_primary_16='6' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='6')",
                "7": "ethnicity_primary_16='7' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='7')",
                "8": "ethnicity_primary_16='8' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='8')",
                "9": "ethnicity_primary_16='9' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='9')",
                "10": "ethnicity_primary_16='10' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='10')",
                "11": "ethnicity_primary_16='11' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='11')",
                "12": "ethnicity_primary_16='12' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='12')",
                "13": "ethnicity_primary_16='13' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='13')",
                "14": "ethnicity_primary_16='14' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='14')",
                "15": "ethnicity_primary_16='15' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='15')",
                "16": "ethnicity_primary_16='16' OR (NOT ethnicity_primary_16 AND ethnicity_sus_16='16')",
            },
            return_expectations={
                "category": {
                                "ratios": {
                                    "0": 0.5,  # missing in 50%
                                    "1": 0.03125,
                                    "2": 0.03125,
                                    "3": 0.03125,
                                    "4": 0.03125,
                                    "5": 0.03125,
                                    "6": 0.03125,
                                    "7": 0.03125,
                                    "8": 0.03125,
                                    "9": 0.03125,
                                    "10": 0.03125,
                                    "11": 0.03125,
                                    "12": 0.03125,
                                    "13": 0.03125,
                                    "14": 0.03125,
                                    "15": 0.03125,
                                    "16": 0.03125,
                                    }
                                },
                "rate": "universal",
            },
    ),
    
    # BMI
    # Set maximum to avoid any impossibly extreme values being classified as
    # Obese
    bmi_value=patients.most_recent_bmi(
        on_or_after="index_date - 5 years",
        minimum_age_at_measurement=16,
        return_expectations={
            "date": {"latest": "index_date"},
            "float": {"distribution": "normal", "mean": 25.0, "stddev": 7.5},
            "incidence": 0.8,
        },
    ),
    bmi=patients.categorised_as(
        {
            "Not obese": "DEFAULT",
            "Obese I (30-34.9)": """ bmi_value >= 30 AND bmi_value < 35""",
            "Obese II (35-39.9)": """ bmi_value >= 35 AND bmi_value < 40""",
            "Obese III (40+)": """ bmi_value >= 40 AND bmi_value < 100""",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "Not obese": 0.7,
                    "Obese I (30-34.9)": 0.1,
                    "Obese II (35-39.9)": 0.1,
                    "Obese III (40+)": 0.1,
                }
            },
            "incidence": 1.0,
        },
    ),
    
    # Smoking status
    smoking_status=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                     most_recent_smoking_code = 'E' OR (
                       most_recent_smoking_code = 'N' AND ever_smoked
                    )
                """,
            "N": "most_recent_smoking_code = 'N' AND NOT ever_smoked",
            "M": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "S": 0.6,
                    "E": 0.1,
                    "N": 0.2,
                    "M": 0.1,
                }
            },
        },
        most_recent_smoking_code=patients.with_these_clinical_events(
            codelists.clear_smoking_codes,
            find_last_match_in_period=True,
            on_or_before="index_date",
            returning="category",
        ),
        ever_smoked=patients.with_these_clinical_events(
            filter_codes_by_category(codelists.clear_smoking_codes, include=["S", "E"]),
            on_or_before="index_date",
        ),
    ),
    
    # Smoking status (combining never and missing)
    smoking_status_comb=patients.categorised_as(
        {
            "S": "most_recent_smoking_code = 'S'",
            "E": """
                     most_recent_smoking_code = 'E' OR (
                       most_recent_smoking_code = 'N' AND ever_smoked
                    )
                """,
            "N + M": "DEFAULT",
        },
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"S": 0.6, "E": 0.1, "N + M": 0.3}, }
        },
    ),
    
    # IMD (index of multiple deprivation) quintile
    index_of_multiple_deprivation=patients.address_as_of(
        date="index_date",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0,
                    "1": 0.2,
                    "2": 0.2,
                    "3": 0.2,
                    "4": 0.2,
                    "5": 0.2,
                }
            },
        },
    ),
    imd=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": "index_of_multiple_deprivation >= 0 AND index_of_multiple_deprivation < 32800*1/5",
            "2": "index_of_multiple_deprivation >= 32800*1/5 AND index_of_multiple_deprivation < 32800*2/5",
            "3": "index_of_multiple_deprivation >= 32800*2/5 AND index_of_multiple_deprivation < 32800*3/5",
            "4": "index_of_multiple_deprivation >= 32800*3/5 AND index_of_multiple_deprivation < 32800*4/5",
            "5": "index_of_multiple_deprivation >= 32800*4/5 AND index_of_multiple_deprivation <= 32800",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0,
                    "1": 0.2,
                    "2": 0.2,
                    "3": 0.2,
                    "4": 0.2,
                    "5": 0.2,
                }
            },
            "incidence": 1.0,
        },
    ),
    imd_decile=patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": "index_of_multiple_deprivation >= 0 AND index_of_multiple_deprivation < 32800*1/10",
            "2": "index_of_multiple_deprivation >= 32800*1/10 AND index_of_multiple_deprivation < 32800*2/10",
            "3": "index_of_multiple_deprivation >= 32800*2/10 AND index_of_multiple_deprivation < 32800*3/10",
            "4": "index_of_multiple_deprivation >= 32800*3/10 AND index_of_multiple_deprivation < 32800*4/10",
            "5": "index_of_multiple_deprivation >= 32800*4/10 AND index_of_multiple_deprivation < 32800*5/10",
            "6": "index_of_multiple_deprivation >= 32800*5/10 AND index_of_multiple_deprivation < 32800*6/10",
            "7": "index_of_multiple_deprivation >= 32800*6/10 AND index_of_multiple_deprivation < 32800*7/10",
            "8": "index_of_multiple_deprivation >= 32800*7/10 AND index_of_multiple_deprivation < 32800*8/10",
            "9": "index_of_multiple_deprivation >= 32800*8/10 AND index_of_multiple_deprivation < 32800*9/10",
            "10": "index_of_multiple_deprivation >= 32800*9/10 AND index_of_multiple_deprivation <= 32800",
        },
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0,
                    "1": 0.1,
                    "2": 0.1,
                    "3": 0.1,
                    "4": 0.1,
                    "5": 0.1,
                    "6": 0.1,
                    "7": 0.1,
                    "8": 0.1,
                    "9": 0.1,
                    "10": 0.1,
                }
            },
            "incidence": 1.0,
        },
    ),
    
    # Region (one of NHS England 9 regions)
    region=patients.registered_practice_as_of(
        "index_date",
        returning="nuts1_region_name",
        return_expectations={
            "rate": "universal",
            "category": {
                "ratios": {
                    "North East": 0.1,
                    "North West": 0.1,
                    "Yorkshire and the Humber": 0.1,
                    "East Midlands": 0.1,
                    "West Midlands": 0.1,
                    "East of England": 0.1,
                    "London": 0.2,
                    "South East": 0.2,
                },
            },
        },
    ),
)
