#' Figure S1
#'
#' This script produces the Figure S1.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr}
#'
#' @date 2021/10/27


## Figure Export Settings ----

ragg::agg_png(filename   = here::here("figures", "mouillot_etal_fig-S1.png"), 
              width      = 21.00, 
              height     = 14.00, 
              units      = "in", 
              res        = 600, 
              pointsize  = 18,
              background = "white")


## Create Layout ----

mat <- matrix(c(1, 1, 3, 2, 2, 4), nrow = 2, byrow = TRUE)
layout(mat, widths = rep(1, ncol(mat)), heights = rep(1, ncol(mat)))


## Parameters ----

llabels <- c("Terrestrial", "Marine")


## Coordinates systems ----

lon_lat   <- paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", 
                   "+towgs84=0,0,0")
mollweide <- paste("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 ", 
                   "+units=m +no_defs")
robinson  <- paste("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                   "+datum=WGS84 +units=m +no_defs")


## Load Rasters ----

# ras <- c(terra::rast(here::here("data", paste0("CellsLand.tif"))),
#          terra::rast(here::here("data", paste0("CellsSea.tif"))))
# 
# ras <- terra::project(ras, robinson)
# 
# for (i in 1:(terra::nlyr(ras))) {
#   na_cells <- which(is.na(ras[][ , i]))
#   pa_cells <- which(ras[][ , i] == -99)
#   ras[][pa_cells, i]  <- 1
#   ras[][-pa_cells, i] <- 0
#   ras[][na_cells, i]  <- NA
# }
# 
# terra::writeRaster(terra::subset(ras, 1), here::here("data", "raster_land_S1.tif"), overwrite=TRUE)
# terra::writeRaster(terra::subset(ras, 2), here::here("data", "raster_sea_S1.tif"), overwrite=TRUE)

ras <- c(terra::rast(here::here("data", paste0("raster_land_S1.tif"))),
         terra::rast(here::here("data", paste0("raster_sea_S1.tif"))))


## Map guides ----

frame <- map_frame(crs = robinson)
grats <- map_graticules(crs = robinson)
axes  <- map_axes(crs = robinson)


## Map guides (inset) ----

frame_inset <- map_frame(42, 52, -28,-10, crs = robinson)
grats_inset <- map_graticules(42, 52, -28,-10, 
                              parallels = seq(-26, -12, 4), 
                              meridians = seq(44, 50, 4), crs = robinson)
axes_inset  <- map_axes(42, 52, -28,-10, 
                        parallels = seq(-26, -12, 4), 
                        meridians = seq(44, 50, 4), crs = robinson)


## Import basemap Layers ----

world <- sf::st_read(here::here("data", "ne_50m_land", "ne_50m_land.shp"))
ocean <- sf::st_read(here::here("data", "ne_50m_ocean", "ne_50m_ocean.shp"))
coast <- sf::st_read(here::here("data", "ne_50m_coastline", "ne_50m_coastline.shp"))


## Layers projection ----

world  <- sf::st_transform(world, robinson)
ocean  <- sf::st_transform(ocean, robinson)
coast  <- sf::st_transform(coast, robinson)


## Crop layers with map extent ----

options(warn = -1)

world <- sf::st_intersection(world, frame)
ocean <- sf::st_intersection(ocean, frame)
coast <- sf::st_intersection(coast, frame)


## Crop layers with map extent (insets) ----

world_inset <- sf::st_intersection(world, frame_inset)
ocean_inset <- sf::st_intersection(ocean, frame_inset)
coast_inset <- sf::st_intersection(coast, frame_inset)


## Crop graticules with lands ----

grats <- sf::st_intersection(grats, ocean)
grats_inset <- sf::st_intersection(grats_inset, ocean_inset)

options(warn = 0)


## Crop raster (inset) ----

ras_inset <- terra::crop(ras, terra::vect(frame_inset))
ras_inset <- terra::mask(ras_inset, terra::vect(frame_inset))


## Colors ----

col_sea  <- "#e5f1f6"
col_grat <- "#bfdde9"

col_nopa <- c("#FFC44A", "#bfdde9")
col_pa   <- c("#8F261F", "#001768")


## Legend position ----

x_start <- -13800000
y_start <- -5500000
x_inc   <- 400000
y_inc   <- 300000


for (i in 1:length(llabels)) {
  
  par(mar = c(4, 2, 3, 2), family = "Roboto", new = FALSE)
  
  ## Base map ----
  
  plot(sf::st_geometry(frame), border = NA, col = NA)
  
  if (i == 1) {

    plot(sf::st_geometry(world), border = NA, col = "white", lwd = 0.1, 
         add = TRUE)
    
  } else {
    
    plot(sf::st_geometry(ocean), border = col_grat, col = "white", lwd = 0.2, 
         add = TRUE)
    plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)
  }

  
  ## Grid ----
  
  terra::plot(terra::subset(ras, i), add = TRUE, col = c(col_nopa[i], col_pa[i]), 
              breaks = c(0, 0.5, 1), legend = FALSE, axes = FALSE, bty = "n", 
              maxcell = dim(ras)[1] * dim(ras)[2])
  
  
  ## Masks ----
  
  if (i == 1) {

    plot(sf::st_geometry(ocean), border = col_grat, col = col_sea, lwd = 0.2,
         add = TRUE)
    plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)

  } else {

    plot(sf::st_geometry(world), border = col_sea, col = "#dddddd", lwd = 0.1,
         add = TRUE)
  }
  
  plot(sf::st_geometry(frame), border = "white", col = NA, lwd = 4, 
       add = TRUE, xpd = TRUE)
  plot(sf::st_geometry(frame), border = "black", col = NA, lwd = 1, 
       add = TRUE, xpd = TRUE)
  
  
  ## Add inset ----
  
  plot(sf::st_geometry(frame_inset), border = "white", col = NA, lwd = 4, 
       add = TRUE)
  plot(sf::st_geometry(frame_inset), border = "black", col = NA, lwd = 1, 
       add = TRUE)
  
  ## Axes ----
  
  text(axes[axes$"side" == 1, c("x", "y")], axes[axes$"side" == 1, "text"], 
       pos = 1, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes[axes$"side" == 2, c("x", "y")], axes[axes$"side" == 2, "text"], 
       pos = 2, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes[axes$"side" == 3, c("x", "y")], axes[axes$"side" == 3, "text"], 
       pos = 3, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes[axes$"side" == 4, c("x", "y")], axes[axes$"side" == 4, "text"], 
       pos = 4, xpd = TRUE, cex = 0.65, col = "#666666")
  
  
  ## Title ----
  
  mtext(llabels[i], line = 0.75, side = 3, adj = 0, font = 2, cex = 1.2)
  
  
  ## Legend ----
  
  rect(x_start, 
       y_start, 
       x_start + 2 * x_inc, 
       y_start + 2 * y_inc,
       border = "white", col = col_nopa[i])
  
  text(x = x_start + 2 * x_inc, 
       y = mean(c(y_start, y_start + 1.5 * y_inc)), 
       "Cells randomly selected", pos = 4, cex = 1, font = 2)
  
  rect(x_start, 
       y_start + 2 * y_inc, 
       x_start + 2 * x_inc, 
       y_start + 4 * y_inc,
       border = "white", col = col_pa[i])
  
  text(x = x_start + 2 * x_inc, 
       y = mean(c(y_start + 2 * y_inc, y_start + 3.5 * y_inc)), 
       "Cells within protected area", pos = 4, cex = 1, font = 2)
}


###
### Insets
### 


for (i in 1:length(llabels)) {
  
  par(mar = c(4, 2, 3, 2), family = "Roboto", new = FALSE)
  
  ## Base map ----
  
  plot(sf::st_geometry(frame_inset), border = NA, col = NA)
  
  if (i == 1) {

    plot(sf::st_geometry(world_inset), border = NA, col = "white", lwd = 0.1, 
         add = TRUE)
    
  } else {

    plot(sf::st_geometry(ocean_inset), border = col_grat, col = "white", lwd = 0.2, 
         add = TRUE)
    plot(sf::st_geometry(grats_inset), col = col_grat, lwd = 0.2, add = TRUE)
  }
  
  
  ## Grid ----
  
  terra::plot(terra::subset(ras_inset, i), add = TRUE, col = c(col_nopa[i], col_pa[i]),
              breaks = c(0, 0.5, 1), legend = FALSE, axes = FALSE, bty = "n",
              maxcell = dim(ras_inset)[1] * dim(ras_inset)[2])


  ## Masks ----
  
  if (i == 1) {

    plot(sf::st_geometry(ocean_inset), border = col_grat, col = col_sea, lwd = 0.2,
         add = TRUE)
    plot(sf::st_geometry(grats_inset), col = col_grat, lwd = 0.2, add = TRUE)

  } else {

    plot(sf::st_geometry(world_inset), border = col_sea, col = "#dddddd", lwd = 0.1,
         add = TRUE)
  }
  
  plot(sf::st_geometry(frame_inset), border = "white", col = NA, lwd = 4, 
       add = TRUE, xpd = TRUE)
  plot(sf::st_geometry(frame_inset), border = "black", col = NA, lwd = 1, 
       add = TRUE, xpd = TRUE)
  
  
  ## Axes ----
  
  text(axes_inset[axes_inset$"side" == 1, c("x", "y")], axes_inset[axes_inset$"side" == 1, "text"], 
       pos = 1, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes_inset[axes_inset$"side" == 2, c("x", "y")], axes_inset[axes_inset$"side" == 2, "text"], 
       pos = 2, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes_inset[axes_inset$"side" == 3, c("x", "y")], axes_inset[axes_inset$"side" == 3, "text"], 
       pos = 3, xpd = TRUE, cex = 0.65, col = "#666666")
  text(axes_inset[axes_inset$"side" == 4, c("x", "y")], axes_inset[axes_inset$"side" == 4, "text"], 
       pos = 4, xpd = TRUE, cex = 0.65, col = "#666666")
}


dev.off()
