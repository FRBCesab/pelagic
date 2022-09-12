#' Right ENFA Axis 2 Density Plot
#'
#' This script plots density of ENFA Axis 2 (right panel).
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


densities <- vector("list", length(categories))
subdatas  <- vector("list", length(categories))
hull      <- vector("list", length(categories))

names(densities) <- categories
names(subdatas)  <- categories
names(hull)      <- categories



for (category in categories) {
  
  
  ## Highly Restricted must be in Restricted ----
  
  if (category == "Restricted") cats <- c('Restricted', 'Highly Restricted')
  else cats <- category
  
  
  ## Subset Data ----
  
  subdatas[[category]] <- datas[[group]][datas[[group]]$"pa_category" %in% cats, ]
  
  
  ## Compute Density ----
  
  densities[[category]] <- density(subdatas[[category]]$"PCA2", adjust = 4)
}

xrng <- range(datas[[group]]$"PCA2")
yrng <- c(0, max(unlist(lapply(densities, function(.) max(.$"y")))) + 0.1)

par(mar = c(2.50, 0.25, 0.25, 0.50))

plot(0, xlim = yrng, ylim = xrng, axes = FALSE, bty = "n", ann = FALSE, 
     type = "n")


## Grid ----

xxats <- axTicks(side = 1)

abline(h = yats, v = xxats, lty = 3, lwd = .5, col = "lightgray")


## Add Data ----

for (category in categories) {

  polygon(
    y      = c(densities[[category]]$"x", densities[[category]]$"x"[1]),
    x      = c(densities[[category]]$"y", densities[[category]]$"y"[1]),
    col    = paste0(color_pas[[group]][category], "44"),
    border = color_pas[[group]][category],
    lwd    = 1
  )
}


## Zero Axes ----

abline(h = 0, col = "white", lwd = 1.0)
abline(h = 0, col = par()$col.axis, lwd = 0.5)


## Add Axes ----

par(mgp = c(2, .05, 0))
axis(1, at = xxats, lwd = 0, lwd.ticks = .5)
mtext(side = 1, line = 1.05, "Density", font = 1, cex = .60)

box("plot", lwd = .5)