#' --------------------------------------------------------------------------   @Header
#'
#' @title Run Project
#'
#' @description
#' This R script runs the project PELAGIC
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#' @author Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/01/16
#'
#' --------------------------------------------------------------------------   @Header



rm(list = ls())



#'  -------------------------------------------------------------------------   @ProjectSetup


source(file.path("analyses", "setup.R"))



#'  -------------------------------------------------------------------------   @RunPCAENFA


source(file.path("analyses", "run_enfa.R"))


#'  -------------------------------------------------------------------------   @Figure1


source(file.path("analyses", "Figure_1_ggplot.R"))
