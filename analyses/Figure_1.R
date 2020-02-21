plots <- list()
k     <- 0

for (group in groupes) {

  k <- k + 1

  plots[[k]] <- ggplot(
    data    = datas[[group]],
    mapping = aes(
      x     = PCA1,
      y     = PCA2,
      color = catPA,
      fill  = catPA
    )
  ) +

  geom_polygon(
    stat  = "density_2d",
    alpha = 0.3
  ) +

  geom_point(colour = NA) +

  scale_fill_manual(
    values = color_pas[[group]]
  ) +

  scale_colour_manual(
    values = color_pas[[group]]
  ) +

  theme_bw() +

  theme(
    legend.justification = c(0.95, 0.975),
    legend.position      = c(0.95, 0.975),
    legend.title         = element_blank(),
    legend.background    = element_blank(),
    legend.direction     = "horizontal",

    axis.text            = element_text(
      colour = grey_dark,
      family = family,
      size   = 17
    ),
    legend.text          = element_text(
      colour = grey_dark,
      family = family,
      size   = 17,
      face   = "bold"
    ),
    axis.title           = element_text(
      colour = grey_dark,
      family = family,
      size   = 17,
      face   = "bold"
    ),
    panel.grid.major     = element_line(
      colour   = grey_light,
      size     = 0.1,
      linetype = 4
    ),
    panel.grid.minor.x   = element_blank(),
    panel.grid.minor.y   = element_blank(),
    axis.ticks           = element_blank()
  ) +

  labs(
    x = "ENFA axis 1",
    y = "ENFA axis 2"
  ) +

  geom_encircle(
    data    = datas[[group]],
    mapping = aes(
      x      = PCA1,
      y      = PCA2,
      colour = catPA
    ),
    size        = 2.5,
    s_shape     = 1,
    expand      = 0,
    show.legend = FALSE,
    fill        = NA
  )

  k <- k + 1

  plots[[k]] <- ggplot(data.frame()) +
    annotate(
      geom     = "text",
      x        = 1,
      y        = 1:nrow(vars_coords[[group]]),
      label    = vars_coords[[group]]$variable,
      hjust    = 1,
      vjust    = 0.5,
      size     = 6,
      color    = grey_dark,
      family   = family,
      fontface = "plain"
    ) +
    xlim(c(.99, 1)) +
    ylim(c(1 - 0.495, nrow(vars_coords[[group]]) + 0.750)) +
    theme_empty +
    theme(
      plot.margin = margin(0.50, 0.10, 0.10, 0.10, "cm"),
      axis.text.x        = element_text(color = NA),
      panel.grid.major.x = element_blank()
    ) +
    coord_cartesian(clip = "off", expand = FALSE)


  for (axe in c("PCA1", "PCA2")) {

    k <- k + 1

    plots[[k]] <- ggplot(data.frame())

    for (i in 1:nrow(vars_coords[[group]])) {

      plots[[k]] <- plots[[k]] + annotate(
        geom     = "rect",
        xmin     = 0,
        xmax     = vars_coords[[group]][i, axe],
        ymin     = i - 0.33,
        ymax     = i + 0.33,
        alpha    = alpha,
        fill     = color_pas[[group]][3],
        colour   = color_pas[[group]][3]
      )
    }

    plots[[k]] <- plots[[k]] +
      geom_segment(
        mapping = aes(
          x    = 0,
          y    = 1 - 0.495,
          xend = 0,
          yend = nrow(vars_coords[[group]]) + 0.495
        ),
        colour  = grey_dark,
        size    = 0.75
      ) +
      ylim(c(1 - 0.495, nrow(vars_coords[[group]]) + 0.750)) +
      theme_empty

    if (axe == "PCA1") {

      plots[[k]] <- plots[[k]] +
        theme(
          plot.margin = margin(0.50, 0.00, 0.10, 0.20, "cm")
        )

      texte <- "ENFA axis 1"
    }

    if (axe == "PCA2") {

      plots[[k]] <- plots[[k]] +
        theme(
          plot.margin = margin(0.50, 0.10, 0.10, 0.10, "cm")
       )

      texte <- "ENFA axis 2"
    }

    plots[[k]] <- plots[[k]] +
      coord_cartesian(clip = "off", expand = FALSE) +
      annotate(
        geom     = "text",
        x        = 0,
        y        = 12.75,
        label    = texte,
        hjust    = 0.5,
        vjust    = 0,
        size     = 6,
        color    = grey_dark,
        family   = family,
        fontface = "bold"
      )
  }
}


plots <- grid.arrange(
  grobs         = plots,
  widths        = c(1.30, 2.85, 2.85, 1.30, 2.85, 2.85),
  heights       = c(7, 2.25),
  layout_matrix = rbind(
    c(1, 1, 1, 5, 5, 5),
    c(2, 3, 4, 6, 7, 8)
  )
)

ggsave(
  filename  = file.path(path_figs, paste0(figname1, ".png")),
  plot      = plots,
  height    = 15.45,
  width     = 24.00,
  units     = "in",
  dpi       = 600
)
