CohortOperationsShinyApp
================
Javier Gracia-Tabuenca

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

-   [FinnGen/FinnGenTableTypes](https://github.com/FinnGen/FinnGenTableTypes):
    Provides tools to create and operate cohortData tables.
-   [FinnGen/CDMTools](https://github.com/FinnGen/CDMTools): Provides
    connection to Atlas and Atlas’ CDM databases.

This project follows [Golem](https://engineering-shiny.org/golem.html)
philosophy for the development of Shiny Apps, including modularisation,
unit testing, shiny server testing, yalm configuration, and
dockerisation. Moreover, this projects uses
[renv](https://rstudio.github.io/renv/articles/renv.html) for package
dependency management.

For details on the development, testing, and running of the app see
detailed documents in [dev/](dev/).

## Running in SandBox

If it is the first time you run the app you need first to download it
into SandBox.

Open a terminal inside SandBox. Pull the image :

``` bash
docker pull eu.gcr.io/finngen-sandbox-v3-containers/cohort_operations_shiny_app:latest
```

Once the image is available in SandBox you can run the following
command Run:

``` bash
docker run -p 8888:8888 -e BUCKET_SANDBOX_IVM=$BUCKET_SANDBOX_IVM eu.gcr.io/finngen-sandbox-v3-containers/cohort_operations_shiny_app:latest > /home/ivm/cohort_operations_shiny_app & sleep 5 && firefox localhost:8888
```
This should also open Firefox browser. If not, open it and browse to `http://0.0.0.0:8888`.
