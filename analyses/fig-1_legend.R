#' Add Figure Legend
#'
#' This script adds the figure legend (colors).
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


plot(0, xlim = c(-1, 1), ylim = c(0, 5), axes = FALSE, bty = "n", ann = FALSE, 
     type = "n")


## Legend Items ----

for (i in 1:length(categories)) {

  if (categories[i] == "Highly Restricted") new_cat <- "IUCN I"
  if (categories[i] == "Restricted") new_cat <- "Protected"
  if (categories[i] == "None") new_cat <- "Non protected"
  
  text(x = -.64, y = i, labels = new_cat, cex = 0.85, pos = 4, font = 2)
  
  rect(
    xleft   = -0.89, 
    ybottom = i - 0.15, 
    xright  = -0.565, 
    ytop    = i + 0.30, 
    col     = paste0(color_pas[[group]][categories[i]], "aa"), 
    border  = color_pas[[group]][categories[i]], 
    lwd     = 1
  )
}
