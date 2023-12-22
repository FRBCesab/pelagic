#' Figure 4
#'
#' This script produces the Figure 4.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2021/10/25


devtools::load_all()


## Figure Export Settings ----

ragg::agg_png(filename   = here::here("figures", "mouillot_etal_fig-5_new.png"), 
              width      = 21.00, 
              height     = 15.00, 
              units      = "in", 
              res        = 600, 
              pointsize  = 18,
              background = "white")


## Create Layout ----

mat <- matrix(c(1, 3, 3, 2, 4, 4, 5, 6, 6), nrow = 3, byrow = TRUE)
layout(mat, widths = rep(1, ncol(mat)), heights = c(1, 1, 0.1))


## Biplots ----

source(here::here("analyses", "fig-4_biplots.R"))


## Maps ----

par(cex.axis = 1.2)
source(here::here("analyses", "fig-4_maps.R"))


dev.off()
