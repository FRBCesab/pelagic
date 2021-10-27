#' Bottom Variables Barplot
#'
#' This script plots variables correlations on ENFA Axis (bottom panel).
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


nnn <- nrow(vars_coords[[group]])   # Number of variables


## Plot 1 (Left) - Variables Labels ----

plot(0, type = "n", xlim = c(0, 1), ylim = c(-1, nnn + 2), ann  = FALSE, 
     bty  = "n", axes = FALSE)

abline(h = 0.5, lwd = 2, col = "white")

text(x = rep(1.05, nnn), y = 1:nnn, labels = vars_coords[[group]]$variable, pos = 2,
     cex = 0.9)

lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[3], 2), lwd = .5)
lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[4], 2), lwd = .5)
lines(x = rep(par()$usr[1], 2), y = c(par()$usr[3], par()$usr[4]), lwd = .5)


## Barplots ----

for (pca in c("PCA1", "PCA2")) {

  if (pca == "PCA1") par(mar = c(0.5, 0.0, 0.5, 0.0), xaxs = "i", yaxs = "i")
  if (pca == "PCA2") par(mar = c(0.5, 0.0, 0.5, 0.5), xaxs = "i", yaxs = "i")


  plot(
    x    = 0,
    type = "n",
    xlim = c(
      min(vars_coords[[group]][ , pca]) - 0.050,
      max(vars_coords[[group]][ , pca]) + 0.050
    ),
    ylim = c(-1, nnn + 2),
    ann  = FALSE,
    bty  = "n",
    axes = FALSE
  )
  
  abline(h = 0.5, lwd = 2, col = "white")

  where <- axTicks(side = 1)
  
  ## Hack ----
  
  # if (group == "marine" && pca == "PCA2" && variables_type != "Socioeconomic") where <- where[-1]
  # if (group == "terrestrial" && pca == "PCA2") where <- where[-length(where)]
  # 
  # 
  # if (variables_type == "Socioeconomic" && pca == "PCA1") where <- where[-1]
  # if (variables_type == "Socioeconomic" && pca == "PCA2") where <- where[-1]
  # 
  
  if (variables_type == "all") {
    if (group == "terrestrial" && pca == "PCA2") {
      where <- where[-length(where)]
    }
    if (group == "marine" && pca == "PCA2") {
      where <- where[-1]
    }
  }
  
  if (variables_type == "Socioeconomic") {
    if (group == "terrestrial" && pca == "PCA1") {
      where <- where[-1]
    }
    if (group == "terrestrial" && pca == "PCA2") {
      where <- where[-c(1, length(where))]
    }
    if (group == "marine" && pca == "PCA2") {
      where <- where[-length(where)]
    }
  }
  
  if (variables_type == "Environment") {
    if (group == "marine" && pca == "PCA2") {
      where <- where[-c(1, length(where))]
    }
  }
  
  # Add Axis ----
  
  for (i in where) {

    text(x = i, y = 0.5, labels = i, pos = 1, col = par_fg, cex = 0.65, 
         xpd = TRUE)
    # lines(x = rep(i, 2), y = c(1 - 0.50,   1 - 0.60), col = par_fg, lwd = 0.5)
    lines(x = rep(i, 2), y = c(1 - 0.33, nnn + 0.55), col = "#999999", 
          lwd = 0.5, lty = 3)
  }

  
  ## Add Data ----
  
  # couleur <- color_pas[[group]][length(color_pas[[group]])]

  for (i in 1:nnn) {
    
    couleur <- color_cat[vars_coords[[group]][i, "family"]]

    if (vars_coords[[group]][i, pca] < 0) {

      rect(
        xleft   = vars_coords[[group]][i, pca], 
        ybottom = i - 0.33, 
        xright  = 0, 
        ytop    = i + 0.33, 
        col     = paste0(couleur, ""), 
        border  = couleur
      )

    } else {

      rect(
        xleft   = 0, 
        ybottom = i - 0.33, 
        xright  = vars_coords[[group]][i, pca], 
        ytop    = i + 0.33, 
        col     = paste0(couleur, ""), 
        border  = couleur
      )
    }
  }

  ## Zero Axis ----
  
  lines(x = rep(0, 2), y = c(1 - 0.55, nnn + 0.55), lty = 1, col = par_fg)

  
  ## Add Title ----
  
  text(
    x      = (par()$usr[1] + par()$usr[2]) / 2,
    y      = nnn + 1.15,
    labels = paste0("ENFA axis ", gsub("PCA", "", pca)),
    font   = 2, 
    cex    = 0.80
  )

  
  ## Variable categories Separation ----
  
  abline(h = 6.5, lwd = 2, col = "white")

  
  ## Plot Box ----
  
  lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[3], 2), lwd = .5)
  lines(x = c(par()$usr[1], par()$usr[2]), y = rep(par()$usr[4], 2), lwd = .5)
}

lines(x = rep(par()$usr[2], 2), y = c(par()$usr[3], par()$usr[4]), lwd = 0.5)
