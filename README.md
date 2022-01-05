CohortOperationsShinyApp
================
Javier Gracia-Tabuenca

-   [Intro](#intro)
-   [Development](#development)
    -   [Development in laptop](#development-in-laptop)
    -   [Development in Sandbox](#development-in-sandbox)
    -   [Configure development
        enviroment](#configure-development-enviroment)
    -   [Run in development](#run-in-development)
-   [Deployment](#deployment)
    -   [Build docker image](#build-docker-image)
    -   [Move to sandbox](#move-to-sandbox)
    -   [Push to GCP Container
        Register](#push-to-gcp-container-register)
    -   [Run](#run)

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
[renv](https://rstudio.github.io/renv/articles/renv.html) for package
dependency management.

## Development

Before any of the following steps you need a github token to access the
code in the private repositories. Set the environmental variable
`GITHUB_PAT` with a token generated in
[github.com/settings/tokens](https://github.com/settings/tokens). Token
should include following permissions: “gist, repo, user, workflow”.
Following code helps you to do that from R:

``` r
# generate tocken 
usethis::create_github_token()

# copy and paste in 
Sys.setenv(GITHUB_PAT="<paste_token>")
```

### Development in laptop

For development you can clone this repository and use `renv::` to
install all the dependent packages.

`renv::` automatically installs its self at the opening of the project.
Then, run `renv::restore()` to install the dependent packages.

### Development in Sandbox

Sandbox has not connection to the internet. However, dependencies cant
be build in a temporal IVM with connection to the internet, zipped, and
copy into Sandbox IVM. (note: the temporal IVM should have the same
operating system and R version)

Follow `renv::` instruction for a such situation:

> Clone the package on a computer with internet connection. Specify the
> folder where your packages are stored by setting the
> RENV\_PATHS\_CACHE location (run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`). Then
> run `renv::restore()` in the console. Manually move the study package
> to the environment without internet (this now includes all required R
> packages), activate the current project with `renv::activate()` and
> again run
> `Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`
> followed by `renv::restore()` in the console.

### Configure development enviroment

Configuration for your development environment can be set in
`inst/golem-config.yml`. And selected on run time seting the envar
`GOLEM_CONFIG_ACTIVE` (see:
[golem-config](https://engineering-shiny.org/golem.html?q=GOLEM_CONFIG_ACTIVE#golem-config))

Currently this file includes three environments:

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

### Run in development

Open R in the package folder, or open project in Rstudio.

``` r
# load package
devtools::load_all(".")

# set configuration 
Sys.setenv(GOLEM_CONFIG_ACTIVE="<config_tag_in_golem-config.yml>")
# if this is not set, default configuration is no_connection environment
# Rstudio in sandbox is not reading the system environmental variables, force the envar as 
# Sys.setenv(BUCKET_SANDBOX_IVM="fg-production-sandbox-<n sandbox>_ivm")

# run shiny app
run_app()
)
```

## Deployment

### Build docker image

Docker image can be build from scratch or, to save time, it can be built
using the pre-compiled dependencies built in above section “Development
in Sandbox”.

Both methods use the same command:

``` bash
cd <CohortOperationsShinyApp>
sudo docker build -t <docker_image_name> --build-arg GITHUB_PAT=<paste_PAT_token> .
```

If `renv::restore()` was run with option
`Sys.setenv("RENV_PATHS_CACHE"=paste0(getwd(),"/renv/cache"))`. The
cache is copied into the docker image during building. If so, you mush
keep updated the cache if it changes during development.

Alternatively, you can run `renv::restore()` or erase “./renv/cache”
folder. In this case, all packages will be downloaded and install during
the building process.

### Move to sandbox

Docker image can be save with:

``` bash
docker save --output <docker_image_name.tar> <docker_image_name>
```

Downloaded. For example using :

``` bash
python3 -m http.server 8888
```

Uploaded and loaded into sanxbox:

``` bash
docker load --input <docker_image_name.tar>
```

### Push to GCP Container Register

Make sure you are [running docker without
sudo](https://github.com/sindresorhus/guides/blob/main/docker-without-sudo.md).

Authenticate with application-default login and configure docker. Use
your Finngen account

``` bash
gcloud auth login
#gcloud auth application-default login #QUESTION: this was not working, is it ok the above way ??
gcloud auth configure-docker
# you can heck that credentials exist.
#cat ~/.config/gcloud/application_default_credentials.json
```

Tag the image.

``` bash
docker tag <docker_image_name> eu.gcr.io/atlas-development-270609/<docker_image_name>:<version_tag>
```

Push newly tagged image to destination.

``` bash
docker push eu.gcr.io/atlas-development-270609/<docker_image_name>:<version_tag>
```

Revoke credentials.

``` bash
gcloud auth revoke
```

([other help
link](https://cloud.google.com/container-registry/docs/advanced-authentication))

### Run

Running image needs to tunnel the port and set envar
`BUCKET_SANDBOX_IVM`

TEMP: at the moment BUCKET\_SANDBOX\_IVM needs to be passed, bcs the
docker is not getting the envars from the main.

``` bash
docker run -p 8888:8888 -e BUCKET_SANDBOX_IVM=$BUCKET_SANDBOX_IVM <docker_image_name>
```
