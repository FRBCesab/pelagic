nnn <- nrow(vars_coords[[group]]) # Number of variables


## Plot 1 - variables labels ----

plot(
  x    = 0,
  type = "n",
  xlim = c(0, 1),
  ylim = c(-1, nnn + 2),
  ann  = FALSE,
  bty  = "n",
  axes = FALSE
)
abline(h = 0.5, lwd = 2, col = "white")

text(
  x      = rep(1, nnn),
  y      = 1:nnn,
  labels = vars_coords[[group]]$variable,
  pos    = 2,
  cex    = 0.65
)

lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[3], 2), lwd = .5)
lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[4], 2), lwd = .5)
lines(x = rep(par()$usr[1], 2), y = c(par()$usr[3], par()$usr[4]), lwd = .5)


## Barplot

for (pca in c("PCA1", "PCA2")) {

  if (pca == "PCA1") {
    par(mar = c(0.5, 0, 0.5, 0), xaxs = "i", yaxs = "i")
  } else {
    par(mar = c(0.5, 0, 0.5, 0.5), xaxs = "i", yaxs = "i")
  }


  plot(
    x    = 0,
    type = "n",
    xlim = c(
      min(vars_coords[[group]][ , pca]) - 0.090,
      max(vars_coords[[group]][ , pca]) + 0.090
    ),
    ylim = c(-1, nnn + 2),
    ann  = FALSE,
    bty  = "n",
    axes = FALSE
  )
  abline(h = 0.5, lwd = 2, col = "white")

  # x-axis
  for (i in axTicks(side = 1)) {

    text(i, .5, i, pos = 1, col = "#666666", cex = .65)
    lines(x = rep(i, 2), y = c(1 - 0.50,   1 - 0.60), col = "#666666", lwd = 0.5)
    lines(x = rep(i, 2), y = c(1 - 0.33, nnn + 0.55), col = "#999999", lwd = 0.5, lty = 3)
  }

  # Data
  couleur <- ifelse(group == "marine", "#034e7b", "#8c2d04")

  for (i in 1:nnn) {

    if (vars_coords[[group]][i, pca] < 0) {

      rect(vars_coords[[group]][i, pca], i - 0.33, 0, i + 0.33, col = paste0(couleur, "aa"), border = couleur)

    } else {

      rect(0, i - 0.33, vars_coords[[group]][i, pca], i + 0.33, col = paste0(couleur, "aa"), border = couleur)
    }
  }

  # x = 0
  lines(c(0, 0), c(1-.55, nnn + .55), lty = 1, col = "#666666")

  # Title
  text(
    x      = (par()$usr[1] + par()$usr[2]) / 2,
    y      = nnn + 1.15,
    labels = paste0("ENFA axis ", gsub("PCA", "", pca)),
    font = 2, cex = .70
  )

  abline(h = 6.5, lwd = 2, col = "white")

  lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[3], 2), lwd = .5)
  lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[4], 2), lwd = .5)
}

lines(x = rep(par()$usr[2], 2), y = c(par()$usr[3], par()$usr[4]), lwd = 0.5)
