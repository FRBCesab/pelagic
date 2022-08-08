#' Plot ENFA Results
#'
#' This script plots ENFA results.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


## Figure Export Settings ----

ragg::agg_png(filename   = here::here("figures", paste0(figname, ".png")),
              width      = 14.00,
              height     =  9.00,
              units      = "in",
              res        = 600,
              pointsize  = 18,
              background = "white")


## Create Layout ----

mat <- matrix(c(15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 
                2,  2,  2,  2,  2,  2,  7,  7,  9,  9,  9,  9,  9,  9, 14, 14,
                1,  1,  1,  1,  1,  1,  3,  3,  8,  8,  8,  8,  8,  8, 10, 10,
                4,  4,  5,  5,  5,  6,  6,  6, 11, 11, 12, 12, 12, 13, 13, 13),
              nrow  = 4, byrow = TRUE)

layout(mat, widths  = rep(1, ncol(mat)), heights = c(0.25, 1.25, 4, 2.25))



for (group in groupes) {


  ## General Graphical Parameters ----
  
  par(cex.axis = 0.70,
      mgp      = c(2, .5, 0),
      tcl      = -0.15,
      xpd      = FALSE,
      col      = par_fg,
      col.axis = par_fg,
      fg       = par_fg,
      family   = "serif",
      xaxs     = "r",
      yaxs     = "r")

  
  ## Central Scatterplot/Density Plot ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "fig-1_a.R"))

  
  ## Top 1D Density Plot (ENFA Axis 1) ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "fig-1_b.R"))

  
  ## Right 1D Density Plot (ENFA Axis 2) ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "fig-1_c.R"))

  
  ## Bottom Barplot (Variable Coordinates) ----
  
  par(mar = c(0.5, 0.5, 0.5, 0), xaxs = "i", yaxs = "i")
  source(here::here("analyses", "fig-1_d.R"))

  
  ## Top-Right Corner Legend ----
  
  par(mar = c(0.25, 0.25, 0.50, 0.50), mgp = c(2, 0.5, 0))
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "fig-1_legend.R"))

}

## Titles ----

par(mar = rep(0, 4), xaxs = "i", yaxs = "i")
plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
text(0, 0, "Terrestrial", font = 2, cex = 1.25)

plot(0, 0, type = "n", axes = FALSE, ann = FALSE)
text(0, 0, "Marine", font = 2, cex = 1.25)


dev.off()
