#' Plot ENFA Results
#'
#' This script plots ENFA results.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


## Figure Export Settings ----

png(
  file      = here::here("figures", paste0(figname, ".png")),
  width     = 12.00,
  height    =  8.00,
  units     = "in",
  res       = 600,
  pointsize = 18
)


## Create Layout ----

mat <- matrix(
  c(
     2,  2,  2,  2,  7,  9,  9,  9,  9, 14,
     1,  1,  1,  1,  3,  8,  8,  8,  8, 10,
     4,  5,  5,  6,  6, 11, 12, 12, 13, 13
  ),
  nrow  = 3,
  byrow = TRUE
)

layout(
  mat     = mat,
  widths  = c(1, 1, 1, 1, 1),
  heights = c(1, 4, 1.75, 4, 1.75)
)



for (group in groupes) {


  ## General Graphical Parameters ----
  
  par(
    cex.axis = 0.70,
    mgp      = c(2, .5, 0),
    tcl      = -0.15,
    xpd      = FALSE,
    col      = par_fg,
    col.axis = par_fg,
    fg       = par_fg,
    family   = "serif",
    xaxs     = "r",
    yaxs     = "r"
  )

  
  ## Central Scatterplot/Density Plot ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "subplot_a.R"))

  
  ## Top 1D Density Plot (ENFA Axis 1) ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "subplot_b.R"))

  
  ## Right 1D Density Plot (ENFA Axis 2) ----
  
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "subplot_c.R"))

  
  ## Bottom Barplot (Variable Coordinates) ----
  
  par(mar = c(0.5, 0.5, 0.5, 0), xaxs = "i", yaxs = "i")
  source(here::here("analyses", "subplot_d.R"))

  
  ## Top-Right Corner Legend ----
  par(mar = c(0.25, 0.25, 0.50, 0.50), mgp = c(2, 0.5, 0))
  par(xaxs = "r", yaxs = "r")
  source(here::here("analyses", "legend.R"))

}

dev.off()


## Message ----


## Message ----

usethis::ui_done(paste(figname, "successfully exported in figures/"))
