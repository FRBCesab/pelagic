






par(family = "sans", xaxs = "i", yaxs = "i", mgp = c(2, 0.3, 0), mar = c(3, 3, 0.75, 0.75))

plot(0, xlim = c(-0.01, 1.01), ylim = c(-0.01, 1.01), type = 'n', bty = 'n', axes = FALSE, 
     xlab = "Ranking of unprotected areas for conservation benefits of vertebrates", 
     ylab = "Probability of protecting unprotected areas")




for (j in 1:length(bornes)) {
  for (k in 1:length(bornes)) {

    rect(bornes[j] - inc, bornes[k] - inc, bornes[j], bornes[k], border = NA,
         col = paste0(col_2[k], "FF"))
    rect(bornes[j] - inc, bornes[k] - inc, bornes[j], bornes[k], border = NA,
         col = paste0(col_1[j], "88"))
  }
}

for (k in 1:length(bornes)) {
  rect(bornes[j] - inc, bornes[k] - inc, bornes[j], bornes[k], border = NA,
       col = paste0(col_2[k], ""))
}

axis(1, c(par()$usr[1], par()$usr[2]), lwd = 3, lwd.ticks = 0, labels = FALSE, col = "white")
axis(2, c(par()$usr[3], par()$usr[4]), lwd = 3, lwd.ticks = 0, las = 1, labels = FALSE, col = "white")
axis(3, c(par()$usr[1], par()$usr[2]), lwd = 3, lwd.ticks = 0, labels = FALSE, col = "white")
axis(4, c(par()$usr[3], par()$usr[4]), lwd = 3, lwd.ticks = 0, las = 1, labels = FALSE, col = "white")


axis(1, c(par()$usr[1], par()$usr[2]), lwd = 2, lwd.ticks = 0, labels = FALSE)
axis(1, at = seq(0, 1.0, 0.2), labels = paste0(seq(0, 100, 20), "%"), lwd = 0)

axis(3, c(par()$usr[1], par()$usr[2]), lwd = 2, lwd.ticks = 0, labels = FALSE)

axis(2, c(par()$usr[3], par()$usr[4]), lwd = 2, lwd.ticks = 0, las = 1, labels = FALSE)
axis(2, lwd = 0, las = 1)

axis(4, c(par()$usr[3], par()$usr[4]), lwd = 2, lwd.ticks = 0, las = 1, labels = FALSE)

# rect(0.9, 0.0, 1.0, 1.0, lwd = 4, border = "white", xpd = TRUE)

# rect(0.0, 0.9, 0.1, 1.0, lwd = 3, col = "white", density = 20, angle = 45, xpd = TRUE)
rect(0.0, 0.9, 0.1, 1.0, lwd = 1, border = "white", density = 20, angle = 45, xpd = TRUE)
rect(0.0, 0.9, 0.1, 1.0, lwd = 4, border = "white", xpd = TRUE)
rect(0.0, 0.9, 0.1, 1.0, lwd = 2, border = "black", xpd = TRUE)
text(0.03, 0.86, paste0("Easy no gain"), font = 4, xpd = TRUE, col = "black", pos = 4, cex = 1.25)

# rect(0.9, 0.9, 1.0, 1.0, lwd = 3, col = "white", density = 20, angle = -45, xpd = TRUE)
rect(0.9, 0.9, 1.0, 1.0, lwd = 1, col = "white", density = 20, angle = -45, xpd = TRUE)
rect(0.9, 0.9, 1.0, 1.0, lwd = 4, border = "white", xpd = TRUE)
rect(0.9, 0.9, 1.0, 1.0, lwd = 2, border = "black", xpd = TRUE)
text(0.97, 0.86, paste0("Easy gain"), font = 4, xpd = TRUE, col = "white", pos = 2, cex = 1.25)

# rect(0.9, 0.0, 1.0, 0.1, lwd = 3, col = "white", density = 20, angle = 45, xpd = TRUE)
rect(0.9, 0.0, 1.0, 0.1, lwd = 1, col = "white", density = 20, angle = 45, xpd = TRUE)
rect(0.9, 0.0, 1.0, 0.1, lwd = 4, border = "white", xpd = TRUE)
rect(0.9, 0.0, 1.0, 0.1, lwd = 2, border = "black", xpd = TRUE)
text(0.97, 0.14, paste0("Hard gain"), font = 4, xpd = TRUE, col = "white", pos = 2, cex = 1.25)

# rect(0.0, 0.0, 0.1, 0.1, lwd = 3, col = "white", density = 20, angle = -45, xpd = TRUE)
rect(0.0, 0.0, 0.1, 0.1, lwd = 1, border = "white", density = 20, angle = -45, xpd = TRUE)
rect(0.0, 0.0, 0.1, 0.1, lwd = 4, border = "white", xpd = TRUE)
rect(0.0, 0.0, 0.1, 0.1, lwd = 2, border = "black", xpd = TRUE)
text(0.03, 0.14, paste0("Hard no gain"), font = 4, xpd = TRUE, col = "black", pos = 4, cex = 1.25)

lines(x = rep(0.5, 2), y = c(-0.02, 1.02), lwd = 4, lty = 1, col = "white", xpd = TRUE)
lines(y = rep(0.5, 2), x = c(-0.02, 1.02), lwd = 4, lty = 1, col = "white", xpd = TRUE)
lines(x = rep(0.5, 2), y = c(-0.02, 1.02), lwd = 2, lty = 2, xpd = TRUE)
lines(y = rep(0.5, 2), x = c(-0.02, 1.02), lwd = 2, lty = 2, xpd = TRUE)
