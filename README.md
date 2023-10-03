# vaccine-counts

You can run this project via [Gitpod](https://gitpod.io) in a web browser by clicking on this badge: [![Gitpod ready-to-code](https://img.shields.io/badge/Gitpod-ready--to--code-908a85?logo=gitpod)](https://gitpod.io/#https://github.com/opensafely/vaccine-counts)

[View on OpenSAFELY](https://jobs.opensafely.org/repo/https%253A%252F%252Fgithub.com%252Fopensafely%252Fvaccine-counts)

Details of the purpose and any published outputs from this project can be found at the link above.

The contents of this repository MUST NOT be considered an accurate or valid representation of the study or its purpose. 
This repository may reflect an incomplete or incorrect analysis with no further ongoing work.
The content has ONLY been made public to support the OpenSAFELY [open science and transparency principles](https://www.opensafely.org/about/#contributing-to-best-practice-around-open-science) and to support the sharing of re-usable code for other subsequent users.
No clinical, policy or safety conclusions must be drawn from the contents of this repository.

# About the OpenSAFELY framework

The OpenSAFELY framework is a Trusted Research Environment (TRE) for electronic
health records research in the NHS, with a focus on public accountability and
research quality.

Read more at [OpenSAFELY.org](https://opensafely.org).

# Licences
As standard, research projects have a MIT license. 

# About the Vaccine Coverage Snapshot and other related work

* This repository contains the code for the [OpenSAFELY COVID-19 Vaccine Coverage Snapshot](https://reports.opensafely.org/reports/vaccine-coverage-index/).
* This analysis builds on our prior [OpenSAFELY Vaccine Coverage Reports](https://reports.opensafely.org/reports/vaccine-coverage-index/), including:
    - [First doses for adults (over 16s)](https://reports.opensafely.org/reports/vaccine-coverage/)
    - [Second doses for adults (over 16s)](https://reports.opensafely.org/reports/vaccine-coverage-second-doses/)
    - [Boosters/third doses for adults (over 16s)](https://reports.opensafely.org/reports/vaccine-coverage-thirdbooster-doses/)
    - [First doses for children and adolescents (5-15 year olds)](https://reports.opensafely.org/reports/vaccine-coverage-children-1st-dose/)
    - [Second doses for children and adolescents (5-15 year olds)](https://reports.opensafely.org/reports/vaccine-coverage-children-2nd-dose/)
* Code for our prior vaccine coverage reports is available on [Github](https://github.com/opensafely/nhs-covid-vaccination-coverage#opensafely-covid-19-vaccine-coverage-report).
* The Vaccine Coverage Snapshot runs on data from English general practices that use TPP electronic health record software. We have also analysed vaccine coverage across practices using either EMIS or TPP software. This work is summarised [in our BJGP article](https://bjgp.org/content/72/714/e51) and is based on [this Github repository](https://github.com/opensafely/covid19-vaccine-coverage-tpp-emis). 
* Further details about the purpose and any published outputs from this project can be found in its [OpenSAFELY workspace](https://jobs.opensafely.org/covid-19-vaccine-effectiveness/vaccine-counts/).

 # Summary of analysis code for Vaccine Coverage Snapshot
 
* If you are interested in how we defined our variables, take a look at the [study definition](analysis/study_definition_snapshot.py); this is written in `python`, but non-programmers should be able to get a relatively good idea of what is going on here. The script refers to separate files that define demographic factors ([snapshot_demographic_vars.py](analysis/snapshot_demographic_vars.py)) and clinical subgroups ([snapshot_comorbidity_vars.py](analysis/snapshot_comorbidity_vars.py)).
* Age, demography, clinical subgroups, and vaccination history are defined as of **01 September 2023**.
* If you are interested in how we defined our code lists, look in the [codelists folder](./codelists/). The definition of demographic and clinical subgroups follows the methods used in our [Lancet Public Health study of changes in COVID-19 mortality rates](https://www.thelancet.com/journals/lanpub/article/PIIS2468-2667(23)00079-8/fulltext) based on [this Github repository](https://github.com/opensafely/covid_mortality_over_time). We also incorporate more narrowly defined ethnic groups, as per previous vaccine coverage reports.
* Data are processed and summarised via the [snapshot_process.R](analysis/snapshot_process.R) and [snapshot_report.R](analysis/snapshot_report.R) scripts, respectively, and the report is then generated via the [vaccine_snapshot_report.Rmd](anaysis/vaccine_snapshot_report.Rmd) script. These are written in the programming language `R`.
