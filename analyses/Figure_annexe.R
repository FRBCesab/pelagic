png(
  file      = file.path(path_figs, paste0(fignameS1, ".png")),
  width     = 12.00,
  height    =  8.00,
  units     = "in",
  res       = 600,
  pointsize = 18
)

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


  if (group == "marine") {

    categories <- c("NPA", "MPA", "NO TAKE")
    pa_colors  <- c("#c3c3c3", "#74a9cf", "#034e7b")
    names(pa_colors) <- categories

  } else  {

    categories <- c("NPA", "TPA", "NO TAKE")
    pa_colors  <- c("#c3c3c3", "#fec44f", "#8c2d04")
    names(pa_colors) <- categories
  }


  par(
    cex.axis = .70,
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

  par(xaxs = "r", yaxs = "r")
  source(file.path("analyses", "_subplot_1b.R"))

  par(xaxs = "r", yaxs = "r")
  source(file.path("analyses", "_subplot_1a.R"))

  par(xaxs = "r", yaxs = "r")
  source(file.path("analyses", "_subplot_1c.R"))

  par(mar = c(0.5, 0.5, 0.5, 0), xaxs = "i", yaxs = "i")
  source(file.path("analyses", "_subplot_1d.R"))

  par(mar = c(.25, .25, .5, .5), mgp = c(2, .5, 0))
  par(xaxs = "r", yaxs = "r")
  source(file.path("analyses", "_legend_1.R"))

}

dev.off()
