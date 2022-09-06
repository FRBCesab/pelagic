#' Figure 4
#'
#' This script produces the Figure 4.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2021/10/25


## Figure Export Settings ----

ragg::agg_png(filename   = here::here("figures", "mouillot_etal_fig-4.png"), 
              width      = 21.00, 
              height     = 14.00, 
              units      = "in", 
              res        = 600, 
              pointsize  = 18,
              background = "white")


## Create Layout ----

mat <- matrix(c(1, 3, 3, 2, 4, 4), nrow = 2, byrow = TRUE)
layout(mat, widths = rep(1, ncol(mat)), heights = rep(1, ncol(mat)))


## Biplots ----

source(here::here("analyses", "fig-4_biplots.R"))


## Maps ----

par(cex.axis = 1.2)
source(here::here("analyses", "fig-4_maps.R"))


dev.off()
