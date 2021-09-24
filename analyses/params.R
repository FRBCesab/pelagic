#' Set Project Parameters
#'
#' This R script sets project parameters, e.g. main variables, colors values,
#' etc.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


## Define Main Variables ----

groupes <- c("terrestrial", "marine")

pca_list     <- list()            ## To store PCA results
enfa_list    <- list()            ## To store ENFA results
vars_coords  <- vector("list", 2) ## To store variables coordinates on ENFA axes

names(vars_coords) <- groupes


## Colors ----

grey_light    <- "#888888"
grey_dark     <- "#333333"
par_fg        <- "#666666"

color_pas        <- vector("list", 2)
names(color_pas) <- groupes

categories <- c("None", "Restricted", "Highly Restricted")

color_pas[["terrestrial"]]        <- c("#c3c3c3", "#fec44f", "#8c2d04")
color_pas[["marine"]]             <- c("#c3c3c3", "#74a9cf", "#034e7b")
names(color_pas[["terrestrial"]]) <- categories
names(color_pas[["marine"]])      <- categories

alpha  <- 0.44
family <- "serif"

color_cat <- c("#80B9B3", "#D5AC7B")
names(color_cat) <- c("Environment", "Socioeconomic")


## Load Data for ENFA ----

datas        <- vector("list", 2)
datas[[1]]   <- get(load(file = here::here("data", "cov_imp_ATP.RData")))
datas[[2]]   <- get(load(file = here::here("data", "cov_imp_AMP.RData")))
names(datas) <- groupes

rm(list = c("amp", "atp"))


## Load Variables Names ----

vars_list        <- vector("list", 2)
vars_list[[1]]   <- readr::read_csv(
  here::here("data", "list_variables_terrestrial.csv")
)
vars_list[[2]]   <- readr::read_csv(
  here::here("data", "list_variables_marine.csv")
)
names(vars_list) <- groupes
