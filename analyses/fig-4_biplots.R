## Parameters ----

regions <- c("Land", "Sea")
llabels <- c("Terrestrial", "Marine")


## Coordinates systems ----

lon_lat   <- paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", 
                   "+towgs84=0,0,0")
mollweide <- paste("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", 
                   "+units=m +no_defs")
robinson  <- paste("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84", 
                   "+datum=WGS84 +units=m +no_defs")


## Breaks color palette ----

increment <- 0.1
bornes <- seq(0 + increment, 1, increment)

col_2 <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8',
           '#abd9e9','#74add1','#4575b4','#313695')
col_2 <- rev(col_2)
col_2 <- colorRampPalette(col_2[-c(2, 10)])
col_2 <- rev(col_2(length(bornes)))

col_1 <- colorRampPalette(c("black", "white"))(length(bornes))


## Graphical parameters ----

par(family = "Roboto", xaxs = "i", yaxs = "i", mgp = c(2, 0.3, 0), 
    mar = c(4, 4, 3, 3))


for (i in 1:length(regions)) {
  
  ## Load Data ----

  datas <- get(load(here::here("data", paste0("datZ", regions[i], ".RData"))))


  ## Load Raster ----

  ras   <- raster::raster(here::here("data", paste0("Cells", regions[i],
                                                    ".tif")))


  ## Add cells coordinates to Data ----

  cells <- which(ras[] %in% datas$"ID")
  xy    <- data.frame(ID = ras[][cells], raster::xyFromCell(ras, cells))

  dat <- merge(xy, datas, by = "ID", all = TRUE)


  ## Compute Top10 ----

  rank <- 0.9 * max(dat$RankZ)
  dat$"top10" <- FALSE
  dat[which(dat$RankZ >= rank), "top10"] <- TRUE


  ## Convert to probabilities ----

  # dat10 <- dat[dat$"top10" == TRUE, ]

  dat$"RankZ" <- dat$"RankZ" / max(dat$"RankZ")


  ## Convert to sf ----

  tab <- sf::st_as_sf(dat, coords = c("x", "y"), crs = mollweide)
  tab <- sf::st_transform(tab, robinson)


  ## Categorize ----

  cats <- data.frame(from = seq(0, 1 - increment, by = increment),
                     to   = seq(0 + increment, 1, by = increment))

  cats$"category" <- LETTERS[1:nrow(cats)]


  for (j in 1:nrow(cats)) {

    pos <- which(tab$"proba" >= cats[j, "from"] & tab$"proba" < cats[j, "to"])

    if (length(pos)) {

      tab[pos, "category"] <- cats[j, "category"]

    }
  }
  
  
  ## Plot ----
  
  plot(0, xlim = c(-0.01, 1.01), ylim = c(-0.01, 1.01), type = 'n', bty = 'n', 
       axes = FALSE, 
       xlab = "Ranking of unprotected areas for conservation benefits", 
       ylab = "Probability of protecting unprotected areas", cex.axis = 1.2, 
       font.lab = 2, cex.lab = 1.25)
  
  grid()
  
  
  ## Add points ----
  
  for (j in 1:(length(bornes) - 1)) {

    for (k in 1:length(bornes)) {

      pos <- which(tab$"RankZ" >= (bornes[j] - increment) &
                     tab$"RankZ" <= (bornes[j]) &
                     tab$"proba" >= (bornes[k] - increment) &
                     tab$"proba" <= (bornes[k]))

      if (length(pos)) {

        invisible(lapply(pos, function(x) {
          points(x = tab[x, "RankZ", drop = TRUE],
                 y = tab[x, "proba", drop = TRUE],
                 pch = 21, bg = paste0(col_2[k], "FF"), col = NA)

          points(x = tab[x, "RankZ", drop = TRUE],
                 y = tab[x, "proba", drop = TRUE],
                 pch = 21, bg = paste0(col_1[j], "88"), col = NA)
        }))
      }
    }
  }

  for (k in 1:length(bornes)) {

    pos <- which(tab$"RankZ" >= (bornes[length(bornes)] - increment) &
                   tab$"RankZ" <= (bornes[length(bornes)]) &
                   tab$"proba" >= (bornes[k] - increment) &
                   tab$"proba" <= (bornes[k]))

    if (length(pos)) {

      invisible(lapply(pos, function(x) {
        points(x = tab[x, "RankZ", drop = TRUE],
               y = tab[x, "proba", drop = TRUE],
               pch = 21, bg = paste0(col_2[k], "FF"), col = NA)
      }))
    }
  }
  

  ## Axes ----
  
  axis(1, c(par()$usr[1], par()$usr[2]), lwd = 3, lwd.ticks = 0, labels = FALSE, 
       col = "white")
  axis(2, c(par()$usr[3], par()$usr[4]), lwd = 3, lwd.ticks = 0, labels = FALSE, 
       col = "white", las = 1)
  axis(3, c(par()$usr[1], par()$usr[2]), lwd = 3, lwd.ticks = 0, labels = FALSE, 
       col = "white")
  axis(4, c(par()$usr[3], par()$usr[4]), lwd = 3, lwd.ticks = 0, labels = FALSE, 
       col = "white", las = 1)
  
  axis(1, c(par()$usr[1], par()$usr[2]), lwd = 1, lwd.ticks = 0, labels = FALSE)
  axis(2, c(par()$usr[3], par()$usr[4]), lwd = 1, lwd.ticks = 0, labels = FALSE, 
       las = 1)
  axis(3, c(par()$usr[1], par()$usr[2]), lwd = 1, lwd.ticks = 0, labels = FALSE)
  axis(4, c(par()$usr[3], par()$usr[4]), lwd = 1, lwd.ticks = 0, labels = FALSE, 
       las = 1)
  
  axis(1, at = seq(0, 1.0, 0.2), labels = paste0(seq(0, 100, 20), "%"), lwd = 0)
  axis(2, lwd = 0, las = 1)
  
  
  ## Top-left Box ----
  
  n <- length(which(dat$"RankZ" <= 0.1 & dat$"proba" >= 0.9))

  rect(0.0, 0.9, 0.1, 1.0, lwd = 6, border = "white", xpd = TRUE)
  rect(0.0, 0.9, 0.1, 1.0, lwd = 3, border = "black", xpd = TRUE)
  shadow_text(0.03, 0.8, paste0("Easy no gain\nn = ", n, ""), font = 2, 
              xpd = TRUE, col = "black", bg = "white", pos = 4, cex = 1.45, 
              radius = 0.15)
  
  
  ## Top-right Box ----
  
  n <- length(which(dat$"RankZ" >= 0.9 & dat$"proba" >= 0.9))
  
  rect(0.9, 0.9, 1.0, 1.0, lwd = 6, border = "white", xpd = TRUE)
  rect(0.9, 0.9, 1.0, 1.0, lwd = 3, border = "black", xpd = TRUE)
  shadow_text(0.97, 0.8, paste0("Easy gain\nn = ", n, ""), font = 2, 
              xpd = TRUE, col = "black", bg = "white", pos = 2, cex = 1.45, 
              radius = 0.15)
  
  
  ## Bottom-right Box ----
  
  n <- length(which(dat$"RankZ" >= 0.9 & dat$"proba" <= 0.1))
  
  rect(0.9, 0.0, 1.0, 0.1, lwd = 6, border = "white", xpd = TRUE)
  rect(0.9, 0.0, 1.0, 0.1, lwd = 3, border = "black", xpd = TRUE)
  shadow_text(0.97, 0.18, paste0("Hard gain\nn = ", n, ""), font = 2, 
              xpd = TRUE, col = "black", bg = "white", pos = 2, cex = 1.45, 
              radius = 0.15)

  
  ## Bottom-left Box ----
  
  n <- length(which(dat$"RankZ" <= 0.1 & dat$"proba" <= 0.1))

  rect(0.0, 0.0, 0.1, 0.1, lwd = 6, border = "white", xpd = TRUE)
  rect(0.0, 0.0, 0.1, 0.1, lwd = 3, border = "black", xpd = TRUE)
  shadow_text(0.03, 0.18, paste0("Hard no gain\nn = ", n, ""), font = 2, 
              xpd = TRUE, col = "black", bg = "white", pos = 4, cex = 1.45, 
              radius = 0.15)
  
  
  ## Plot Frame ----
  
  lines(x = rep(0.5, 2), y = c(-0.02, 1.02), lwd = 4, lty = 1, col = "white", 
        xpd = TRUE)
  lines(y = rep(0.5, 2), x = c(-0.02, 1.02), lwd = 4, lty = 1, col = "white", 
        xpd = TRUE)
  lines(x = rep(0.5, 2), y = c(-0.02, 1.02), lwd = 2, lty = 2, xpd = TRUE)
  lines(y = rep(0.5, 2), x = c(-0.02, 1.02), lwd = 2, lty = 2, xpd = TRUE)
  
  
  ## Subplot Title ----
  
  mtext(LETTERS[i], line = 0.75, side = 3, adj = -0.100, font = 2, cex = 1.2)
  mtext(llabels[i], line = 0.75, side = 3, adj = -0.025, font = 2, cex = 1.2)
}
