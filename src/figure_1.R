
mat <- matrix(c(1, 2, 2, 3, 3), nrow = 1, byrow = TRUE)
layout(mat, widths = c(1, 1), heights = c(1))

par(mar = rep(0, 4), family = "serif", xaxs = "i", yaxs = "i")
plot(0, type = "n", xlim = c(0, 1), ylim = c(-.25, 13.25), ann = FALSE, bty = "n", axes = FALSE)
rect(par()$usr[1], 7-.5, par()$usr[2], 12+.5, col = "#cccccc", border = "#cccccc", lwd = 2)
rect(par()$usr[1], 1-.5, par()$usr[2], 6+.5, col = "#efefef", border = "#efefef", lwd = 2)

abline(h = 0.5, lwd = 2, col = "white")

text(rep(1, 12), 1:12, vars$variable, pos = 2, cex = 1, font = 1)

abline(h = 12.55, lwd = 2, col = "white")
abline(h = 6.5, lwd = 2, col = "white")

plot(0, type = "n", xlim = c(min(vars$PCA1)-.1, max(vars$PCA1)+.1), ylim = c(-.25, 13.25), ann = FALSE, bty = "n", axes = FALSE)
rect(par()$usr[1], 7-.5, par()$usr[2], 12+.5, col = "#cccccc", border = "#cccccc", lwd = 2)
rect(par()$usr[1], 1-.5, par()$usr[2], 6+.5, col = "#efefef", border = "#efefef", lwd = 2)

abline(h = 0.5, lwd = 2, col = "white")

for (i in seq(-1, 1, by = .2)) {
  text(i, .5, i, pos = 1, col = "#666666")
  lines(c(i, i), y = c(1-.5, 1-.6), col = "#666666")
  lines(c(i, i), c(1-.33, 12+.55), lty = 3, col = "#999999")
}

for (i in 1:12) {
  if (vars[i, "PCA1"] < 0) {
    rect(vars[i, "PCA1"], i - .33, 0, i + .33, col = "steelblue", border = "steelblue")
  } else {
    rect(0, i - .33, vars[i, "PCA1"], i + .33, col = "steelblue", border = "steelblue")
  }
}
lines(c(0, 0), c(1-.55, 12+.55), lty = 1, col = "#666666")
text(x = (par()$usr[1] + par()$usr[2]) / 2, y = 12.85, "PCA 1", font = 2)

abline(h = 12.55, lwd = 2, col = "white")
abline(h = 6.5, lwd = 2, col = "white")


plot(0, type = "n", xlim = c(min(vars$PCA2)-.1, max(vars$PCA2)+.1), ylim = c(-.25, 13.25), ann = FALSE, bty = "n", axes = FALSE)
rect(par()$usr[1], 7-.5, par()$usr[2], 12+.5, col = "#cccccc", border = "#cccccc", lwd = 2)
rect(par()$usr[1], 1-.5, par()$usr[2], 6+.5, col = "#efefef", border = "#efefef", lwd = 2)

abline(h = 0.5, lwd = 2, col = "white")

for (i in seq(-.2, 1, by = .2)) {
  text(i, .5, i, pos = 1, col = "#666666")
  lines(c(i, i), y = c(1-.5, 1-.6), col = "#666666")
  lines(c(i, i), c(1-.33, 12+.55), lty = 3, col = "#999999")
}


for (i in 1:12) {
  if (vars[i, "PCA2"] < 0) {
    rect(vars[i, "PCA2"], i - .33, 0, i + .33, col = "steelblue", border = "steelblue")
  } else {
    rect(0, i - .33, vars[i, "PCA2"], i + .33, col = "steelblue", border = "steelblue")
  }
}
lines(c(0, 0), c(1-.55, 12+.55), lty = 1, col = "#666666")
text(x = (par()$usr[1] + par()$usr[2]) / 2, y = 12.85, "PCA 2", font = 2)

abline(h = 12.55, lwd = 2, col = "white")
abline(h = 6.5, lwd = 2, col = "white")
