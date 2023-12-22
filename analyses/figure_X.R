#' Figure 4
#'
#' This script produces the Figure 4.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2021/10/25


## Figure Export Settings ----

ragg::agg_png(filename   = here::here("figures", "mouillot_etal_fig-4_new.png"), 
              width      = 14.00, 
              height     = 16.00, 
              units      = "in", 
              res        = 600, 
              pointsize  = 18,
              background = "white")


## Create Layout ----

mat <- matrix(c(1, 1, 2, 2, 3, 3), nrow = 3, byrow = TRUE)
layout(mat, widths = rep(1, ncol(mat)), heights = c(1, 1, 0.15))


## Maps ----

par(cex.axis = 1.2)
source(here::here("analyses", "fig-X_maps.R"))


dev.off()
