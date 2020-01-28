densities <- subdatas <- hull <- vector("list", length(categories))
names(densities) <- names(subdatas) <- names(hull) <- categories

for (category in categories) {

  subdatas[[category]]  <- subset(datas[[group]], datas[[group]][ , "catPA"] == category)
  densities[[category]] <- density(subdatas[[category]]$PCA1)
}

xrng <- range(datas[[group]]$PCA1)
yrng <- c(0, max(unlist(lapply(densities, function(.) max(.$y)))) + 0.1)

par(mar = c(.25, 2.5, .5, .25))

plot(0, xlim = xrng, ylim = yrng, axes = FALSE, bty = "n", ann = FALSE, type = "n")

if (group == "marine") {
  yats <- seq(0, 1, by = 0.5)
} else {
  yats <- seq(0, 0.6, by = 0.3)
}

xats <- seq(-4, 6, by = 2)

abline(h = yats, lty = 3, lwd = .5, col = "lightgray")
abline(v = xats, lty = 3, lwd = .5, col = "lightgray")

par(xpd = FALSE)
for (category in categories) {

  polygon(
    x      = c(densities[[category]]$x, densities[[category]]$x[1]),
    y      = c(densities[[category]]$y, densities[[category]]$y[1]),
    col    = paste0(pa_colors[category], "44"),
    border = pa_colors[category],
    lwd    = 1
  )
}
par(xpd = FALSE)

par(mgp = c(2, .35, 0))
axis(2, at = yats, las = 1, lwd = 0, lwd.ticks = .5)
mtext(side = 2, line = 1.35, "Density", font = 1, cex = .60)

box("plot", lwd = .5)
