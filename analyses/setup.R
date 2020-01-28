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
  "readr"
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
  "Figure_2"
)

fig_vars  <- c(
  "figname1",
  "figname2"
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


color_mar     <- "#ff4500"             # ~ Red
color_ter     <- "#00afbb"            # ~ Turquoise

light_grey    <- "#888888"
dark_grey     <- "#333333"
par_fg        <- "#666666"

color_grp        <- c(color_mar, color_ter)
names(color_grp) <- groupes

n_classes <- 8

# color_mnpa <- colorRampPalette(c("#", "#"))(n_classes)
# color_mrpa <- colorRampPalette(c("#", "#"))(n_classes)
# color_mspa <- colorRampPalette(c("#", "#"))(n_classes)

# color_tnpa <- colorRampPalette(c("#", "#"))(n_classes)
# color_trpa <- colorRampPalette(c("#", "#"))(n_classes)
# color_tspa <- colorRampPalette(c("#", "#"))(n_classes)


alpha      <- "88"
par_family <- "serif"



#'  -------------------------------------------------------------------------   @Fig1Parameters






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
