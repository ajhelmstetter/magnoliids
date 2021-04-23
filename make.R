#' @title PAFTOL_magnoliids: A Research Compendium
#'
#' @description
#' A paragraph providing a full description of the project and describing each
#' step of the workflow.
#'
#' @author Andrew Helmstetter \email{andrew.j.helmstetter@gmail.com}
#'
#' @date 2021/04/20



## Install Dependencies (listed in DESCRIPTION) ----

devtools::install_deps(upgrade = "never")


## Load Project Addins (R Functions and Packages) ----

devtools::load_all()


## Global Variables ----

# You can list global variables here (or in a separate R script)


## Run Project ----

# List all R scripts in a sequential order and using the following form:
# source(here::here("rscripts", "script_X.R"))
rmarkdown::render(input = here::here("rscripts","prelim_results.Rmd"),
                  output_file = here::here("outputs","prelim_results.pdf"))
