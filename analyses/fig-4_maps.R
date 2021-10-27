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


## Prepare data ----

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


  ## Select Top10 ----

  dat <- dat[dat$"top10" == TRUE, ]


  ## Convert to sf ----

  tab <- sf::st_as_sf(dat, coords = c("x", "y"), crs = mollweide)
  tab <- sf::st_transform(tab, robinson)


  ## Categorize ----

  increment <- 0.1

  cats <- data.frame(from = seq(0, 1 - increment, by = increment),
                     to   = seq(0 + increment, 1, by = increment))

  cats$"category" <- LETTERS[1:nrow(cats)]

  sf_list <- list()

  for (j in 1:nrow(cats)) {

    pos <- which(tab$"proba" >= cats[j, "from"] & tab$"proba" < cats[j, "to"])

    if (length(pos)) {

      tab[pos, "category"] <- cats[j, "category"]


      ## Create Buffer + Dissolve + Crop ----

      sf_list[[j]] <- tab[pos, ]
      sf_list[[j]] <- sf::st_buffer(sf_list[[j]], dist = 50000)
      sf_list[[j]] <- sf::st_union(sf_list[[j]])
      sf_list[[j]] <- sf::st_as_sf(sf_list[[j]])
      sf_list[[j]]$"category" <- cats[j, "to"]
    }
  }

  sf_obj <- do.call(rbind, sf_list)
  sf_obj$"Region" <- llabels[i]

  if (i == 1) {
    SF_OBJ <- sf_obj
  } else {
    SF_OBJ <- rbind(SF_OBJ, sf_obj)
  }
}


## Map guides ----

frame <- map_frame(crs = robinson)
grats <- map_graticules(crs = robinson)
axes  <- map_axes(crs = robinson)


## Import basemap Layers ----

world <- sf::st_read(here::here("data", "ne_50m_land", "ne_50m_land.shp"))
ocean <- sf::st_read(here::here("data", "ne_50m_ocean", "ne_50m_ocean.shp"))
coast <- sf::st_read(here::here("data", "ne_50m_coastline", "ne_50m_coastline.shp"))


## Import WDPA ----

wdpa <- readRDS(here::here("data", "wdpa_polygons.rds"))

mar_ap <- wdpa[wdpa$"MARINE" == 2, ]
mar_ap <- mar_ap[mar_ap$"IUCN_CAT" %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"), ]
mar_ap <- mar_ap[mar_ap$"GIS_AREA" > 10000, ]

ter_ap <- wdpa[wdpa$"MARINE" == 0, ]
ter_ap <- ter_ap[ter_ap$"IUCN_CAT" %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"), ]
ter_ap <- ter_ap[ter_ap$"GIS_AREA" > 5000, ]


## Layers projection ----

world  <- sf::st_transform(world, robinson)
ocean  <- sf::st_transform(ocean, robinson)
coast  <- sf::st_transform(coast, robinson)
mar_ap <- sf::st_transform(mar_ap, robinson)
ter_ap <- sf::st_transform(ter_ap, robinson)


## Crop layers with map extent ----

options(warn = -1)

world <- sf::st_intersection(world, frame)
ocean <- sf::st_intersection(ocean, frame)
coast <- sf::st_intersection(coast, frame)
mar_ap <- sf::st_intersection(mar_ap, frame)


## Crop graticules with lands ----

grats <- sf::st_intersection(grats, ocean)
mar_ap <- sf::st_intersection(mar_ap, ocean)

options(warn = 0)


## Colors ----

col_sea  <- "#e5f1f6"
col_grat <- "#bfdde9"

couleurs <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8',
              '#abd9e9','#74add1','#4575b4','#313695')
couleurs <- rev(couleurs)
couleurs <- colorRampPalette(couleurs[-c(2, 10)])
couleurs <- rev(couleurs(10))


## Legend position ----

x_start <- -13000000
y_start <- -5000000
x_inc   <- 400000
y_inc   <- 300000



### 
### TERRESTRIAL MAP
### 



par(mar = c(4, 2, 3, 2), family = "Roboto")


## Base map ----

plot(sf::st_geometry(frame), border = NA, col = NA)
plot(sf::st_geometry(world), border = NA, col = "#dddddd", lwd = 0.1, 
     add = TRUE)


## Current AP ----

plot(sf::st_geometry(ter_ap), border = "#6ba249", col = NA, lwd = 0.4, 
     add = TRUE)


## Future AP (Top10) ----

data_ter <- SF_OBJ[SF_OBJ$Region == "Terrestrial", ]
for (i in 1:nrow(data_ter)) {
  plot(sf::st_geometry(data_ter[i, ]), col = couleurs[i], border = NA,
       add = TRUE)
}


## Masks ----

plot(sf::st_geometry(ocean), border = col_grat, col = col_sea, lwd = 0.2, 
     add = TRUE)
plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)
plot(sf::st_geometry(frame), border = "white", col = NA, lwd = 4, 
     add = TRUE, xpd = TRUE)
plot(sf::st_geometry(frame), border = "black", col = NA, lwd = 1, 
     add = TRUE, xpd = TRUE)


## Legend ----

text(axes[axes$"side" == 1, c("x", "y")], axes[axes$"side" == 1, "text"], 
     pos = 1, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 2, c("x", "y")], axes[axes$"side" == 2, "text"], 
     pos = 2, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 3, c("x", "y")], axes[axes$"side" == 3, "text"], 
     pos = 3, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 4, c("x", "y")], axes[axes$"side" == 4, "text"], 
     pos = 4, xpd = TRUE, cex = 0.65, col = "#666666")

rect(x_start, y_start - y_inc, x_start + x_inc * 10, y_start + y_inc,
     border = "white", col = "white", lwd = 2)

for (i in 1:length(couleurs)) {
  
  rect(x_start + x_inc * (i - 1), 
       y_start - y_inc, 
       x_start + x_inc * i, 
       y_start + y_inc,
       border = NA, col = couleurs[i])
}

text(x_start, y_start - y_inc / 1.5, "0.0", pos = 1, cex = 1)
text(x_start + x_inc * 5, y_start - y_inc / 1.5, "0.5", pos = 1, cex = 1)
text(x_start + x_inc * 10, y_start - y_inc / 1.5, "1.0", pos = 1, cex = 1)

text(x_start + x_inc * 5, y_start + y_inc / 1.5, 
     "Probability of protecting\nunprotected areas", pos = 3, cex = 1, font = 2)



### 
### MARINE MAP
### 



par(mar = c(4, 2, 3, 2), family = "Roboto")


## Base map ----

plot(sf::st_geometry(frame), border = NA, col = NA)
plot(sf::st_geometry(ocean), border = col_grat, col = col_sea, lwd = 0.2, 
     add = TRUE)
plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)


## Current AP ----

plot(sf::st_geometry(mar_ap), border = "#6ba249", col = NA, lwd = 0.4, 
     add = TRUE)


## Future AP (Top10) ----

data_mar <- SF_OBJ[SF_OBJ$Region == "Marine", ]
for (i in 1:nrow(data_mar)) {
  plot(sf::st_geometry(data_mar[i, ]), col = couleurs[i], border = NA,
       add = TRUE)
}


## Masks ----

plot(sf::st_geometry(world), border = col_sea, col = "#dddddd", lwd = 0.1, 
     add = TRUE)
plot(sf::st_geometry(frame), border = "white", col = NA, lwd = 4,
     add = TRUE, xpd = TRUE)
plot(sf::st_geometry(frame), border = "black", col = NA, lwd = 1, 
     add = TRUE, xpd = TRUE)


## Legend ----

text(axes[axes$"side" == 1, c("x", "y")], axes[axes$"side" == 1, "text"], 
     pos = 1, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 2, c("x", "y")], axes[axes$"side" == 2, "text"], 
     pos = 2, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 3, c("x", "y")], axes[axes$"side" == 3, "text"], 
     pos = 3, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 4, c("x", "y")], axes[axes$"side" == 4, "text"], 
     pos = 4, xpd = TRUE, cex = 0.65, col = "#666666")

rect(x_start, y_start - y_inc, x_start + x_inc * 10, y_start + y_inc,
     border = "white", col = "white", lwd = 2)

for (i in 1:length(couleurs)) {
  
  rect(x_start + x_inc * (i - 1), 
       y_start - y_inc, 
       x_start + x_inc * i, 
       y_start + y_inc,
       border = NA, col = couleurs[i])
}

text(x_start, y_start - y_inc / 1.5, "0.0", pos = 1, cex = 1)
text(x_start + x_inc * 5, y_start - y_inc / 1.5, "0.5", pos = 1, cex = 1)
text(x_start + x_inc * 10, y_start - y_inc / 1.5, "1.0", pos = 1, cex = 1)

text(x_start + x_inc * 5, y_start + y_inc / 1.5, 
     "Probability of protecting\nunprotected areas", pos = 3, cex = 1, font = 2)
