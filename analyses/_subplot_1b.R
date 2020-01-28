subdatas <- hull <- vector("list", length(categories))
names(subdatas) <- names(hull) <- categories

for (category in categories) {

  subdatas[[category]]  <- subset(datas[[group]], datas[[group]][ , "catPA"] == category)
  hull[[category]]      <- subdatas[[category]][chull(subdatas[[category]]$PCA1, subdatas[[category]]$PCA2), ]
}

xrng <- range(datas[[group]]$PCA1)
yrng <- range(datas[[group]]$PCA2)

par(mar = c(2.5, 2.5, .25, .25))

plot(0, xlim = xrng, ylim = yrng, axes = FALSE, bty = "n", ann = FALSE, type = "n")

grid(lwd = .5)

for (category in categories) {
  
  points(
    x     = subdatas[[category]]$PCA1,
    y     = subdatas[[category]]$PCA2,
    pch   = 19,
    cex   = 0.15,
    col   = paste0(pa_colors[category], "22"),
  )
}

for (category in categories) {

  polygon(
    x      = hull[[category]]$PCA1,
    y      = hull[[category]]$PCA2,
    col    = NA,
    border = "white",
    lwd    = 1
  )

  polygon(
    x      = hull[[category]]$PCA1,
    y      = hull[[category]]$PCA2,
    col    = NA,
    border = pa_colors[category],
    lwd    = 0.5
  )
}

par(mgp = c(2, .05, 0))
axis(1, lwd = 0, lwd.ticks = .5)
mtext(side = 1, line = 1.05, "ENFA axis 1", font = 1, cex = .60)

par(mgp = c(2, .35, 0))
axis(2, las = 1, lwd = 0, lwd.ticks = .5)
mtext(side = 2, line = 1.35, "ENFA axis 2", font = 1, cex = .60)

box("plot", lwd = .5)
