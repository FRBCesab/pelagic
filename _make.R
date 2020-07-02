#' Run the Entire Project
#'
#' This script runs the entire project and produces all figures present in the
#' Mouillot et al.'s 2020 paper.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


## Settings ----

if (!("here" %in% installed.packages())) install.packages("here")

source(here::here("analyses", "setup.R"))


## Main Figure ----

highly_restricted <- c("Ia", "Ib")
source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

for (type in c("points", "densities")) {
  
  figname <- paste0("Figure_Main_", type)
  source(here::here("analyses", "plot_enfa.R"))
}


## Supplementary Figure ----

highly_restricted <- c("Ia", "Ib", "II")
source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

for (type in c("points", "densities")) {

  figname <- paste0("Figure_Supplementary_", type)
  source(here::here("analyses", "plot_enfa.R"))
}
