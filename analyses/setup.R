#' Setup the Project
#'
#' This R script installs missing packages and loads required R functions
#' (listed in the R directory) and packages.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


## Install Missing Packages (listed in DESCRIPTION file) ----

if (!("remotes" %in% installed.packages())) install.packages("remotes")

remotes::install_deps(upgrade = "never")


## Load Project Addings (R functions and packages) ----

devtools::load_all()
