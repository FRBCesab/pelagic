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


## Figure 1 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "all"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-1")
source(here::here("analyses", "plot_enfa.R"))


## Figure S2 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Socioeconomic"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S2")
source(here::here("analyses", "plot_enfa.R"))



## Figure S3 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Environment"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S3")
source(here::here("analyses", "plot_enfa.R"))



## Supplementary Figure ----

# highly_restricted <- c("Ia", "Ib", "II")
# source(here::here("analyses", "params.R"))
# source(here::here("analyses", "run_enfa.R"))
# 
# categories <- c("Non protected", "Protected", "IUCN I & II")
# names(color_pas[["terrestrial"]]) <- categories
# names(color_pas[["marine"]])      <- categories
# 
# for (type in c("points", "densities")) {
# 
#   figname <- paste0("Figure_Supplementary_", type)
#   source(here::here("analyses", "plot_enfa.R"))
# }
