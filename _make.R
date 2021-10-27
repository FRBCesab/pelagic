#' Run the Entire Project
#'
#' This script runs the entire project and produces all figures present in the
#' Mouillot et al.'s 2020 paper.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/11/11


## Settings ----

if (!("here" %in% installed.packages())) install.packages("here")

source(here::here("analyses", "setup.R"))

type <- "points"


## Figure 1 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "all"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-1")
source(here::here("analyses", "figure_1.R"))


## Figure S2 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Socioeconomic"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S2")
source(here::here("analyses", "figure_1.R"))



## Figure S3 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Environment"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S3")
source(here::here("analyses", "figure_1.R"))


## Figure S1 ----

source(here::here("analyses", "figure_s1.R"))


## Figure 4 ----

source(here::here("analyses", "figure_4.R"))

