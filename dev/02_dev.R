# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering
golem::get_golem_things()

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "FGpheWAS " )
usethis::use_package( "CDMTools" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "toastui" )
usethis::use_package( "bigrquery" )
usethis::use_package( "FinnGenTableTypes" )
usethis::use_package("shinyWidgets")
usethis::use_package("shinyjqui")
usethis::use_package("dplyr")
usethis::use_package("httr")
usethis::use_package("reactable")
usethis::use_package("readr")
usethis::use_package("DatabaseConnector")
usethis::use_package("markdown")
usethis::use_package("shinyjs")
usethis::use_package("shinycustomloader")
usethis::use_package("stringr")
usethis::use_package("shinyFeedback")


## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "info_box" )
golem::add_module( name = "append_cohort" )
#golem::add_module( name = "name_of_module2" ) # Name of the module
golem::add_module( name = "connection_to_db" )
#
golem::add_module( name = "cohorts_table" )
#
golem::add_module( name = "import_cohorts" )
golem::add_module( name = "import_cohort_file" )
golem::add_module( name = "import_cohort_atlas" )
golem::add_module( name = "import_cohort_endpoints" )
#
golem::add_module( name = "operate_cohorts" )
golem::add_module( name = "compare_cohorts" )
#
golem::add_module( name = "phewas" )



## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct( "config_CDMTools" )
golem::add_fct( "config_FGpheWAS" )
golem::add_fct( "config_GWAS" )
golem::add_fct( "spiners" )
golem::add_fct( "cohortMatch" )
#golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
#golem::add_html_template("info_test")
#golem::add_js_file( "script" )
#golem::add_js_handler( "handlers" )
#golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "testing_data", open = FALSE )

## Tests ----
## Add one line by test you want to create
usethis::use_test("mod_connection_to_db")
usethis::use_test( "mod_append_cohort" )
#
usethis::use_test("mod_import_cohorts")
usethis::use_test( "mod_import_cohort_file" )
usethis::use_test( "mod_import_cohort_atlas" )
#
usethis::use_test( "mod_operate_cohorts")


usethis::use_test( "fct_cohortMatch" )

# Documentation

## Vignette ----
#usethis::use_vignette("CohortOperationsShinyApp")
#devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
#usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
#covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github(organisation = "FINNGEN", private = TRUE)



