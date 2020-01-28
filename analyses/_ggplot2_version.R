library("ggplot2")
library("gridExtra")


gplot <- uplot <- rplot <- vector("list", 2)
names(gplot) <- names(uplot) <- names(rplot) <- groupes


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


  gplot[[group]] <- ggplot() + theme_light()
  uplot[[group]] <- ggplot() + theme_light()
  rplot[[group]] <- ggplot() + theme_light()

  for (category in categories) {

    subdatas <- subset(datas[[group]], datas[[group]][ , "catPA"] == category)
    hull     <- subdatas[chull(subdatas$PCA1, subdatas$PCA2), ]

    gplot[[group]] <- gplot[[group]] +

    geom_point(
      data  = subdatas,
      mapping = aes(x = PCA1, y = PCA2),
      color   = pa_colors[category],
      size  = 0.1,
      alpha = .05
    ) +

    geom_polygon(
      data    = hull,
      mapping = aes(x = PCA1, y = PCA2),
      color   = "white",
      size    = 0.5,
      fill    = NA
    ) +

    geom_polygon(
      data    = hull,
      mapping = aes(x = PCA1, y = PCA2),
      color   = pa_colors[category],
      size    = 0.2,
      fill    = NA
    )

    uplot[[group]] <- uplot[[group]] +

    geom_density(
      data    = subdatas,
      mapping = aes(x = PCA1),
      alpha   = 0.25,
      size    = 0.2,
      fill    = pa_colors[category],
      color   = pa_colors[category]
    )

    rplot[[group]] <- rplot[[group]] +

    geom_density(
      data    = subdatas,
      mapping = aes(x = PCA2),
      alpha   = 0.25,
      size    = 0.2,
      fill    = pa_colors[category],
      color   = pa_colors[category]
    )
  }

  gplot[[group]] <- gplot[[group]] +

  labs(
    x = "ENFA axis 1",
    y = "ENFA axis 2"
  )

  uplot[[group]] <- uplot[[group]] +

  theme(
    axis.title.x = element_blank(),
    axis.text.x  = element_blank(),
    axis.ticks.x = element_blank()
  ) +

  labs(y = "Density")

  rplot[[group]] <- rplot[[group]] +

  theme(
    axis.title.y = element_blank(),
    axis.text.y  = element_blank(),
    axis.ticks.y = element_blank()
  ) +

  labs(y = "Density") +

  coord_flip()
}


zplot <- ggplot() +

geom_blank(aes(1, 1)) +

theme(
  plot.background  = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border     = element_blank(),
  panel.background = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  axis.text.x      = element_blank(),
  axis.text.y      = element_blank(),
  axis.ticks       = element_blank()
)

grid.arrange(
  uplot[["marine"]], zplot, uplot[["terrestrial"]], zplot,
  gplot[["marine"]], rplot[["marine"]], gplot[["terrestrial"]], rplot[["terrestrial"]],
  ncol = 4,
  nrow = 2,
  widths = c(4, 1.4, 4, 1.4),
  heights = c(1.4, 4)
)
