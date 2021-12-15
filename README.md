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

Before any of the following steps you need a github token to access the
code in the private repositories. Set the environmental variable
`GITHUB_PAT` with a token generated in
[github.com/settings/tokens](https://github.com/settings/tokens). Token
should include following permissions: “gist, repo, user, workflow”.
Following code helps you to do that:

``` r
# generate tocken 
usethis::create_github_token()

# copy and paste in 
Sys.setenv(GITHUB_PAT="<paste_token>")
```

### Install in laptop

For development you can clone this repository and use `renv::` to
install all the dependent packages.

`renv::` automatically installs its self at the opening of the project.
Then, run `renv::restore()` to install the dependent packages.

### Install in Sandbox

Github’s zip can be uploaded to sandbox. However, dependencies cant be
installed into the internetless sandbox IVM.

The dependencies can be collected before in an machine with internet
connection. Then zipped and uploaded to sandbox. Follow `renv::`
instruction for a such situation:

> First open the R package using RStudio on a computer with internet
> (note: this should have the same operating system and R version).
> Specify the folder where your packages are stored by setting the
> RENV\_PATHS\_CACHE location (run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`). Then
> run `renv::restore()` in the console. Manually move the study package
> to the environment without internet (this now includes all required R
> packages), activate the current project with renv::activate() and
> again run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`
> followed by `renv::restore()` in the console.

Alternatively, you can find a pre-build zip for sandbox for some
releases in the github repository.

### Configure

Configuration for your development environment can be set in
`inst/golem-config.yml`. Currently this file includes three
environments:

-   `no_connection`: To work with no connection to an Atlas instance.
-   `atlas-development`: To work with Atlas installed in an ivm in the
    atlas-development project in GCP. This environment needs the
    following additional yalm variables to configure
    [FINNGEN/CDMTools](https://github.com/FINNGEN/CDMTools). -
    `CDMTOOLS_dbms` = “bigquery-dbi”
    -   `GCP_PROJECT_ID` = “atlas-development-270609”
    -   `GCP_BILLING_PROJECT_ID` = “atlas-development-270609”
    -   `CDMTOOLS_webapi_url` = “<http://localhost/WebAPI>”
    -   `CDMTOOLS_CDM_source_key_test` =
        “dummy\_df6v2\_1k\_13\_finngen\_omop\_bq”
    -   `GCP_SERVICE_KEY`: path to the GCP key to access the BQ in
        atlas-development
-   `sandbox`: To work in sandbox. Sandbox must have set the following
    environmental variable `BUCKET_SANDBOX_IVM` with bigquery’s billing
    project name ending in "\_ivm" (eg. “fg-production-sandbox-4\_ivm”).
    This environment also needs the following yalm variables:
    -   `CDMTOOLS_dbms` = “bigquery-dbi”
    -   `GCP_PROJECT_ID` = “finngen-production-library”
    -   `CDMTOOLS_webapi_url` =
        “<https://ohdsi-webapi.app.finngen.fi/WebAPI>”
    -   `CDMTOOLS_CDM_source_key_test` = “FINNGEN\_CDM\_R7”

### Run

``` r
# load package
devtools::load_all(".")

# set configuration 
Sys.setenv(GOLEM_CONFIG_ACTIVE="<config_tag_in_golem-config.yml>")
#Sys.setenv(GCP_BILLING_PROJECT_ID="fg-production-sandbox-<your sandbox number>" ) # necesary if in sandbox

# run shiny app
run_app()
)
```

## Deployment

### Configure

Before any of the following steps you need a github token to access the
code in the private repositories. Get a token from github
[github.com/settings/tokens](https://github.com/settings/tokens). Is
recommended to set the live of the token to one day.

### Build

Clone repository, cd into main file and build giving as argument the
github token:

``` bash
git clone <CohortOperationsShinyApp_url>
cd <CohortOperationsShinyApp>
sudo docker build -t <docker_name> --build-arg GITHUB_PAT=<paste_token> .
```

### Run
