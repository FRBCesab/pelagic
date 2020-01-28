plot(0, xlim = c(-1, 1), ylim = c(0, 5), axes = FALSE, bty = "n", ann = FALSE, type = "n")

text(0, 4.25, toupper(group), cex = .65, font = 2)

lines(c(-.8, .8), rep(3.75, 2), lwd = 1)

for (i in 1:length(categories)) {
  if (categories[i] == "NPA") {
    texte <- "Non protected"
  }
  if (categories[i] %in% c("TPA", "MPA")) {
    texte <- "Protected"
  }
  if (categories[i] == "NO TAKE") {
    texte <- "No take"
  }

  text(x = -.6, y = i, labels = texte, cex = .65, pos = 4, font = 2)
  rect(-.85, i-.15, -.525, i+.30, col = paste0(pa_colors[i], "aa"), border = pa_colors[i], lwd = 1)
}

box("plot", lwd = .5)
