densities <- subdatas <- hull <- vector("list", length(categories))
names(densities) <- names(subdatas) <- names(hull) <- categories

for (category in categories) {

  subdatas[[category]]  <- subset(datas[[group]], datas[[group]][ , "catPA"] == category)
  densities[[category]] <- density(subdatas[[category]]$PCA2)
}

xrng <- range(datas[[group]]$PCA2)
yrng <- c(0, max(unlist(lapply(densities, function(.) max(.$y)))) + 0.1)

par(mar = c(2.5, .25, .25, .5))

plot(0, xlim = yrng, ylim = xrng, axes = FALSE, bty = "n", ann = FALSE, type = "n")


xxats <- axTicks(side = 1)

abline(h = yats, lty = 3, lwd = .5, col = "lightgray")
abline(v = xxats, lty = 3, lwd = .5, col = "lightgray")


par(xpd = FALSE)
for (category in categories) {

  polygon(
    y      = c(densities[[category]]$x, densities[[category]]$x[1]),
    x      = c(densities[[category]]$y, densities[[category]]$y[1]),
    col    = paste0(pa_colors[category], "44"),
    border = pa_colors[category],
    lwd    = 1
  )
}
par(xpd = FALSE)

par(mgp = c(2, .05, 0))
axis(1, at = xxats, lwd = 0, lwd.ticks = .5)
box("plot", lwd = .5)

mtext(side = 1, line = 1.05, "Density", font = 1, cex = .60)
