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

longlat <- get(load("data/IUCN_I_VI_ATP_coords.RData"))
datas[[1]] <- cbind(longlat, datas[[1]])

longlat <- get(load("data/IUCN_I_VI_AMP_coords.RData"))
datas[[2]] <- cbind(longlat, datas[[2]])

save(datas, file = "outputs/enfa_coords_all_vars.RData")


## Figure S2 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Socioeconomic"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S2")
source(here::here("analyses", "figure_1.R"))


longlat <- get(load("data/IUCN_I_VI_ATP_coords.RData"))
datas[[1]] <- cbind(longlat, datas[[1]])

longlat <- get(load("data/IUCN_I_VI_AMP_coords.RData"))
datas[[2]] <- cbind(longlat, datas[[2]])

save(datas, file = "outputs/enfa_coords_only_socio.RData")



## Figure S3 ----

highly_restricted <- c("Ia", "Ib")
variables_type    <- "Environment"

source(here::here("analyses", "params.R"))
source(here::here("analyses", "run_enfa.R"))

figname <- paste0("mouillot_etal_fig-S3")
source(here::here("analyses", "figure_1.R"))


longlat <- get(load("data/IUCN_I_VI_ATP_coords.RData"))
datas[[1]] <- cbind(longlat, datas[[1]])

longlat <- get(load("data/IUCN_I_VI_AMP_coords.RData"))
datas[[2]] <- cbind(longlat, datas[[2]])

save(datas, file = "outputs/enfa_coords_only_env.RData")



## Figure S1 ----

# source(here::here("analyses", "figure_s1.R"))


## Figure 4 ----

# source(here::here("analyses", "figure_4.R"))

