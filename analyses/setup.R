#' --------------------------------------------------------------------------   @Header
#'
#' @title Project setup
#'
#' @description
#' This R script sets up the project PELAGIC
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @author Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/01/16
#'
#' --------------------------------------------------------------------------   @Header



#'  -------------------------------------------------------------------------   @SetOptionsParameters


options(warn = -1)



#' ----------------------------------------------------------------------------- @InstallCranLibs


pkgs <- c(
  "ade4",
  "adehabitatHS",
  "readr",
  "ggplot2",
  "ggalt",
  "gridExtra"
)

nip <- pkgs[!(pkgs %in% installed.packages())]
nip <- lapply(nip, install.packages, dependencies = TRUE)



#' ----------------------------------------------------------------------------- @LoadLibs


ip   <- unlist(lapply(pkgs, require, character.only = TRUE, quietly = TRUE))

if (sum(ip) != length(pkgs)) { cat("Some packages failed to load.\n") }

rm(list = c("pkgs", "nip", "ip"))



#' ----------------------------------------------------------------------------- @LoadRFunctions


rfun <- list.files(path = "R", pattern = "^__.+\\.R$", full.names = TRUE)
rfun <- unlist(lapply(rfun, source, verbose = FALSE))

rm(list = "rfun")



#' ----------------------------------------------------------------------------- @CreateFolders


dir_names <- c(
  "data",
  "figures"
)

dir_vars  <- c(
  "path_data",
  "path_figs"
)

dirs <- lapply(

  X   = 1:length(dir_names),

  FUN = function(i) {

    dir.create(
      path         = dir_names[i],
      showWarnings = FALSE,
      recursive    = TRUE
    )

    assign(
      x     = dir_vars[i],
      value = dir_names[i],
      envir = .GlobalEnv
    )
  }
)

rm(list = c("dir_names", "dir_vars", "dirs"))



#' ----------------------------------------------------------------------------- @SetFigureNames


fig_names <- c(
  "Figure_1",
  "Figure_S1"
)

fig_vars  <- c(
  "figname1",
  "fignameS1"
)

figs <- lapply(

  X   = 1:length(fig_names),

  FUN = function(i) {

    assign(
      x     = fig_vars[i],
      value = fig_names[i],
      envir = .GlobalEnv
    )
  }
)

rm(list = c("fig_names", "fig_vars", "figs"))



#'  -------------------------------------------------------------------------   @GlobalParameters


groupes <- c("marine", "terrestrial")

pca_list  <- list()
enfa_list <- list()



#'  -------------------------------------------------------------------------   @ColorsParameters


grey_light    <- "#888888"
grey_dark     <- "#333333"

par_fg        <- "#666666"

color_pas        <- vector("list", 2)
names(color_pas) <- groupes

color_pas[["marine"]]             <- c("#c3c3c3", "#74a9cf", "#034e7b")
names(color_pas[["marine"]])      <- c("NPA", "MPA", "NO TAKE")

color_pas[["terrestrial"]]        <- c("#c3c3c3", "#fec44f", "#8c2d04")
names(color_pas[["terrestrial"]]) <- c("NPA", "TPA", "NO TAKE")

alpha  <- 0.44
family <- "serif"



#'  -------------------------------------------------------------------------   @LoadAllData

datas        <- vector("list", 2)
datas[[1]]   <- get(load(file = file.path(path_data, "dataIUCNAMP_All.RData")))
datas[[2]]   <- get(load(file = file.path(path_data, "dataIUCNATP_All.RData")))
names(datas) <- groupes

rm(list = c("dataIUCNAMP_All", "dataIUCNATP_All"))

vars_list        <- vector("list", 2)
vars_list[[1]]   <- readr::read_csv(file.path(path_data, "list_variables_marine.csv"))
vars_list[[2]]   <- readr::read_csv(file.path(path_data, "list_variables_terrestrial.csv"))
names(vars_list) <- groupes

vars_coords        <- vector("list", 2)
names(vars_coords) <- groupes



#'  -------------------------------------------------------------------------   @GGPlotTheme

theme_empty <- theme(
  axis.title.x       = element_blank(),
  axis.title.y       = element_blank(),
  axis.text.x        = element_text(
    family = family,
    face   = "plain",
    colour = grey_dark,
    size   = 17
  ),
  axis.text.y        = element_blank(),
  axis.ticks         = element_blank(),
  axis.line.x        = element_blank(),
  axis.line.y        = element_blank(),

  panel.background   = element_blank(),
  panel.border       = element_blank(),

  panel.grid.major.x = element_line(
    colour   = grey_light,
    size     = 0.1,
    linetype = 4
  ),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),

  plot.background    = element_blank(),
  plot.title         = element_text(
    family = family,
    face   = "bold",
    colour = grey_dark,
    size   = 17,
    hjust  = 0.5#,
    # margin = margin(10, 0, 5, 0)
  ),
  plot.margin        = margin(t = 0.50, r = 0, b = 0.25, l = 0, unit = "cm")
)
