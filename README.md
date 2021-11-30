CohortOperationsShinyApp
================
Javier Gracia-Tabuenca

-   [Intro](#intro)
-   [Development](#development)
    -   [Install in laptop](#install-in-laptop)
    -   [Install in Sandbox](#install-in-sandbox)
    -   [Configure](#configure)
    -   [Run](#run)
-   [Deployment](#deployment)
    -   [Configure](#configure-1)
    -   [Build](#build)
    -   [Run](#run-1)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Intro

The goal of CohortOperationsShinyApp is to provide a graphical interface
to create new cohorts based on cohorts imported from Atlas or from a
text file.

A cohort in this tool follows the epidemiological definition: “A cohort
is a set of persons who satisfy one or more inclusion criteria for a
duration of time”
[\[1\]](https://ohdsi.github.io/TheBookOfOhdsi/Cohorts.html#what-is-a-cohort).

This web app is a graphical interface to other R-packages which provide
the underlying functionality. For programming use cases check these:

-   [FINNGEN/FinnGenTableTypes](https://github.com/FINNGEN/FinnGenTableTypes):
    Provides tools to create and operate cohortData tables.
-   [FINNGEN/CDMTools](https://github.com/FINNGEN/CDMTools): Provides
    connection to Atlas and Atlas’ CDM databases.

This project follows [Golem](https://engineering-shiny.org/golem.html)
philosophy for the development of Shiny Apps, including modularisation,
unit testing, shiny server testing, yalm configuration, and
dockerisation. Moreover, this projects uses
[renv](https://rstudio.github.io/renv/articles/renv.html) for pakage
dependency management.

## Development

### Install in laptop

For development you can clone this repository and use `renv::` to
install all the dependent packages.

`renv::` automatically installs its self at the opening of the project.
Then, run `renv::restore()` to install the dependent packages.

### Install in Sandbox

Github’s zip can be uploaded to sandbox.

To restore the dependencies you need to first collect and compile them
in a machine with the same chareacteristies than Sandbox.

> First open the R package using RStudio on a computer with internet
> (note: this should have the same operating system and R version).
> Specify the folder where your packages are stored by setting the
> RENV\_PATHS\_CACHE location (run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache")))`. Then
> run `renv::restore()` in the console. Manually move the study package
> to the environment without internet (this now includes all required R
> packages), activate the current project with renv::activate() and
> again run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`
> followed by `renv::restore()` in the console.

### Configure

Configuration for your environment can be set in
`inst/golem-config.yml`. Currently this file includes three
environments:

-   `no_connection`: To work with no connection to an Atlas instance.
-   `atlas-development`: To work with Atlas install in an ivm in the
    atlas-development project in GCP.  
-   `sandbox`: To work in sandbox. This environment needs of an
    additional environmental variable `GCP_BILLING_PROJECT_ID` with
    bigquery’s billing project name (eg. “fg-production-sandbox-4”)

### Run

``` r
# load package
devtools::load_all(".")

# set configuration 
Sys.setenv(GOLEM_CONFIG_ACTIVE="<config_tag_in_golem-config.yml>")
#Sys.setenv(GCP_BILLING_PROJECT_ID="fg-production-sandbox-<your sandbox number>" ) # if in sandbox

# run shiny app
run_app()
```

## Deployment

### Configure

Get a token from github
[instructions](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token).
Is recommended to set the live of the token to one day.

### Build

### Run
