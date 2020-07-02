#' Central Scatterplot/Density Plot
#'
#' This script plots ENFA results (scatter plot or 2D density plot).
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


subdatas <- vector("list", length(categories))
hull     <- vector("list", length(categories))

names(subdatas) <- categories
names(hull)     <- categories



for (category in categories) {

  
  ## Highly Restricted must be in Restricted ----
  
  if (category == "Restricted") cats <- c('Restricted', 'Highly Restricted')
  else cats <- category
  
  
  ## Subset Data ----

  subdatas[[category]] <- datas[[group]][datas[[group]]$"pa_category" %in% cats, ]
  
  
  ## Compute Convex Hull ----
  
  hull_cells <- chull(subdatas[[category]]$"PCA1", subdatas[[category]]$"PCA2")
  hull[[category]] <- subdatas[[category]][hull_cells, ]
}

xrng <- range(datas[[group]]$"PCA1")
yrng <- range(datas[[group]]$"PCA2")

par(mar = c(2.5, 2.5, 0.25, 0.25))

plot(0, xlim = xrng, ylim = yrng, axes = FALSE, bty = "n", ann = FALSE, type = "n")

xats <- axTicks(side = 1)
yats <- axTicks(side = 2)

grid(lwd = 0.5)


## Add Data ----

for (category in categories) {

  
  if (type == "points") {
    
    points(
      x     = subdatas[[category]]$"PCA1",
      y     = subdatas[[category]]$"PCA2",
      pch   = 19,
      cex   = 0.15,
      col   = paste0(color_pas[[group]][category], "22"),
    ) 
  
  } else {
    
    kernels  <- MASS::kde2d(
      x = subdatas[[category]]$"PCA1", 
      y = subdatas[[category]]$"PCA2", 
      n = 200
    )
    
    contours <- contourLines(kernels)
    
    for (i in 1:length(contours)) {
      
      polygon(
        x      = contours[[i]]$"x", 
        y      = contours[[i]]$"y", 
        border = color_pas[[group]][category], 
        col    = paste0(color_pas[[group]][category], "44")
      )
    }
  }
}



## Add Convex Hulls ----

for (category in categories) {

  polygon(
    x      = hull[[category]]$"PCA1",
    y      = hull[[category]]$"PCA2",
    col    = NA,
    border = "white",
    lwd    = 1
  )

  polygon(
    x      = hull[[category]]$"PCA1",
    y      = hull[[category]]$"PCA2",
    col    = NA,
    border = color_pas[[group]][category],
    lwd    = 0.5
  )
}


## Zero Axes ----

abline(h = 0, v = 0, col = "white", lwd = 1.0)
abline(h = 0, v = 0, col = par()$col.axis, lwd = 0.5)


## Add Axes ----

par(mgp = c(2, 0.05, 0))
axis(1, lwd = 0, lwd.ticks = .5)
mtext(side = 1, line = 1.05, "ENFA axis 1", font = 1, cex = .60)

par(mgp = c(2, .35, 0))
axis(2, las = 1, lwd = 0, lwd.ticks = .5)
mtext(side = 2, line = 1.35, "ENFA axis 2", font = 1, cex = .60)

box("plot", lwd = .5)
