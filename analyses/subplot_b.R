#' Top ENFA Axis 1 Density Plot
#'
#' This script plots density of ENFA Axis 1 (top panel).
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
  
  densities[[category]] <- density(subdatas[[category]]$"PCA1")
}

xrng <- range(datas[[group]]$"PCA1")
yrng <- c(0, max(unlist(lapply(densities, function(.) max(.$"y")))) + 0.1)

par(mar = c(0.25, 2.50, 0.50, 0.25))

plot(0, xlim = xrng, ylim = yrng, axes = FALSE, bty = "n", ann = FALSE, type = "n")


## Grid ----

if (group == "marine")      yyats <- seq(0, 1.0, by = 0.3)
if (group == "terrestrial") yyats <- seq(0, 0.6, by = 0.2)

abline(h = yyats, v = xats, lty = 3, lwd = .5, col = "lightgray")


## Add Data ----

for (category in categories) {

  polygon(
    x      = c(densities[[category]]$"x", densities[[category]]$"x"[1]),
    y      = c(densities[[category]]$"y", densities[[category]]$"y"[1]),
    col    = paste0(color_pas[[group]][category], "44"),
    border = color_pas[[group]][category],
    lwd    = 1
  )
}


## Zero Axes ----

abline(v = 0, col = "white", lwd = 1.0)
abline(v = 0, col = par()$col.axis, lwd = 0.5)


## Add Axes ----

par(mgp = c(2, 0.35, 0))
axis(2, at = yyats, las = 1, lwd = 0, lwd.ticks = .5)
mtext(side = 2, line = 1.35, "Density", font = 1, cex = .60)

box("plot", lwd = .5)