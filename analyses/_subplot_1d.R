plot(0, type = "n", xlim = c(0, 1), ylim = c(-1, 14), ann = FALSE, bty = "n", axes = FALSE)
abline(h = 0.5, lwd = 2, col = "white")

text(rep(1, 12), 1:12, vars_coords[[group]]$variable, pos = 2, cex = .65, font = 1)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[3], par()$usr[3]),
  lty = 1,
  lwd = .5
)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[4], par()$usr[4]),
  lty = 1,
  lwd = .5
)

lines(
  x   = c(par()$usr[1], par()$usr[1]),
  y   = c(par()$usr[3], par()$usr[4]),
  lty = 1,
  lwd = .5
)





par(mar = c(0.5, 0, 0.5, 0), family = "serif", xaxs = "i", yaxs = "i")

plot(0, type = "n", xlim = c(min(vars_coords[[group]]$PCA1)-.1, max(vars_coords[[group]]$PCA1)+.1), ylim = c(-1, 14), ann = FALSE, bty = "n", axes = FALSE)

abline(h = 0.5, lwd = 2, col = "white")

for (i in seq(-1, 1, by = .2)) {
  text(i, .5, i, pos = 1, col = "#666666", cex = .65)
  lines(x = c(i, i), y = c(1-.5, 1-.6), col = "#666666", lwd = .5)
  lines(x = c(i, i), y = c(1-.33, 12+.55), lty = 3, col = "#999999", lwd = .5)
}

couleur <- ifelse(group == "marine", "#034e7b", "#8c2d04")

for (i in 1:12) {
  if (vars_coords[[group]][i, "PCA1"] < 0) {
    rect(vars_coords[[group]][i, "PCA1"], i - .33, 0, i + .33, col = paste0(couleur, "aa"), border = couleur)
  } else {
    rect(0, i - .33, vars_coords[[group]][i, "PCA1"], i + .33, col = paste0(couleur, "aa"), border = couleur)
  }
}
lines(c(0, 0), c(1-.55, 12+.55), lty = 1, col = "#666666")
text(x = (par()$usr[1] + par()$usr[2]) / 2, y = 13.15, "ENFA axis 1", font = 2, cex = .70)

abline(h = 6.5, lwd = 2, col = "white")

par(xpd = TRUE)
lines(c(par()$usr[2], par()$usr[2]), c(-.55, 13.45), lwd = 1, col = "#666666")
par(xpd = FALSE)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[3], par()$usr[3]),
  lty = 1,
  lwd = .5
)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[4], par()$usr[4]),
  lty = 1,
  lwd = .5
)





par(mar = c(0.5, 0, 0.5, 0.5), family = "serif", xaxs = "i", yaxs = "i")

plot(0, type = "n", xlim = c(min(vars_coords[[group]]$PCA2)-.1, max(vars_coords[[group]]$PCA2)+.1), ylim = c(-1, 14), ann = FALSE, bty = "n", axes = FALSE)
# rect(par()$usr[1], 7-.5, par()$usr[2], 12+.5, col = "#cccccc", border = "#cccccc", lwd = 2)
# rect(par()$usr[1], 1-.5, par()$usr[2], 6+.5, col = "#efefef", border = "#efefef", lwd = 2)

abline(h = 0.5, lwd = 2, col = "white")

if (group == "marine") {
  grad <- seq(-1, .2, by = .2)
} else {
  grad <- round(seq(-.6, .4, by = .2), 1)
}


for (i in grad) {
  text(i, .5, i, pos = 1, col = "#666666", cex = .65)
  lines(c(i, i), y = c(1-.5, 1-.6), col = "#666666", lwd = .5)
  lines(c(i, i), c(1-.33, 12+.55), lty = 3, col = "#999999", lwd = .5)
}


for (i in 1:12) {
  if (vars_coords[[group]][i, "PCA2"] < 0) {
    rect(vars_coords[[group]][i, "PCA2"], i - .33, 0, i + .33, col = paste0(couleur, "aa"), border = couleur)
  } else {
    rect(0, i - .33, vars_coords[[group]][i, "PCA2"], i + .33, col = paste0(couleur, "aa"), border = couleur)
  }
}
lines(c(0, 0), c(1-.55, 12+.55), lty = 1, col = "#666666")
text(x = (par()$usr[1] + par()$usr[2]) / 2, y = 13.15, "ENFA axis 2", font = 2, cex = .70)

abline(h = 6.5, lwd = 2, col = "white")

par(xpd = TRUE)
lines(c(par()$usr[1], par()$usr[1]), c(-.55, 13.45), lwd = 1, col = "#666666")
par(xpd = FALSE)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[3], par()$usr[3]),
  lty = 1,
  lwd = .5
)

lines(
  x   = c(par()$usr[1], par()$usr[2]),
  y   = c(par()$usr[4], par()$usr[4]),
  lty = 1,
  lwd = .5
)

lines(
  x   = c(par()$usr[2], par()$usr[2]),
  y   = c(par()$usr[3], par()$usr[4]),
  lty = 1,
  lwd = .5
)
