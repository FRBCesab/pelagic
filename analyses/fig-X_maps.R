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


## Map guides ----

frame <- map_frame(crs = robinson)
grats <- map_graticules(crs = robinson)
axes  <- map_axes(crs = robinson)


## Import basemap Layers ----

world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
world <- world[world$"admin" != "Antarctica", ]
ocean <- sf::st_read(here::here("data", "ne_50m_ocean", "ne_50m_ocean.shp"))
coast <- sf::st_read(here::here("data", "ne_50m_coastline", "ne_50m_coastline.shp"))
eez   <- sf::st_read(here::here("data", "World_EEZ_v11_20191118_gpkg", "eez_v11.gpkg"))
eez   <- eez[eez$"SOVEREIGN1" != "Antarctica", ]


## Layers projection ----

world  <- sf::st_transform(world, robinson)
ocean  <- sf::st_transform(ocean, robinson)
coast  <- sf::st_transform(coast, robinson)
eez    <- sf::st_transform(eez, robinson)


## Crop layers with map extent ----

options(warn = -1)

# world <- sf::st_intersection(world, frame)
ocean <- sf::st_intersection(ocean, frame)
coast <- sf::st_intersection(coast, frame)
# eez    <- sf::st_intersection(eez, frame)


## Crop graticules with lands ----

grats <- sf::st_intersection(grats, ocean)

options(warn = 0)


## Prepare data ----

i <- 1
  
## Load Data ----

datas <- get(load(here::here("data", paste0("datZ", regions[i], ".RData"))))
preds <- get(load(here::here("data", paste0("res_prob_", tolower(llabels[i]), ".Rdata"))))
preds <- preds[ , c("PA", "corr.rf.response")]
colnames(preds) <- c("ID", "PredictedProtection")
datas <- merge(datas, preds, by = "ID", all = FALSE)


## Load Raster ----

ras   <- raster::raster(here::here("data", paste0("Cells", regions[i],
                                                  ".tif")))


## Add cells coordinates to Data ----

cells <- which(ras[] %in% datas$"ID")
xy    <- data.frame(ID = ras[][cells], raster::xyFromCell(ras, cells))

dat <- merge(xy, datas, by = "ID", all = TRUE)


# ## Compute Top10 ----
# 
# rank <- 0.9 * max(dat$RankZ)
# dat$"top10" <- FALSE
# dat[which(dat$RankZ >= rank), "top10"] <- TRUE
# 
# 
# ## Select Top10 ----
# 
# dat <- dat[dat$"top10" == TRUE, ]


## Convert to sf ----

tab <- sf::st_as_sf(dat, coords = c("x", "y"), crs = mollweide)
tab <- sf::st_transform(tab, robinson)


## Intersect with country ----

x <- sf::st_intersects(tab, world, sparse = FALSE)
  
for (k in 1:nrow(world)) {
  pos <- which(x[ , k])
  if (length(pos))
    world[k, "mean_proba"] <- mean(tab[pos, "PredictedProtection", drop = TRUE])
}


## Categorize ----

increment <- 0.1

# cats <- data.frame(from = seq(0, 1 - increment, by = increment),
#                    to   = seq(0 + increment, 1, by = increment))

cats <- data.frame(from = c(0, 0.05, 0.1, 0.2, 0.4, 0.8),
                   to   = c(0.05, 0.1, 0.2, 0.4, 0.8, 1))

# cats$"couleur" <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090',
#                     '#81B8D7', '#659AC7', '#4D7AB7', '#38559C', '#24286f')

cats$"couleur" <- c('#a50026', '#d73027', '#f46d43', '#fdae61',
                    '#659AC7', '#24286f')

for (j in 1:nrow(cats)) {
  
  pos <- which(world$"mean_proba" >= cats[j, "from"] & world$"mean_proba" < cats[j, "to"])
  
  if (length(pos))
    world[pos, "couleur"] <- cats[j, "couleur"]
}

pos <- which(is.na(world$"couleur"))
if (length(pos) > 0)
  world[pos, "couleur"] <- "#dddddd"


## Colors ----

col_sea  <- "#e5f1f6"
col_grat <- "#bfdde9"


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
plot(sf::st_geometry(world), border = "white", col = world$"couleur", lwd = 0.4, 
     add = TRUE)
plot(sf::st_geometry(world), border = "#aaaaaa", col = NA, lwd = 0.2, 
     add = TRUE)


## Masks ----

plot(sf::st_geometry(ocean), border = col_grat, col = col_sea, lwd = 0.2, 
     add = TRUE)
plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)
plot(sf::st_geometry(frame), border = "white", col = NA, lwd = 4, 
     add = TRUE, xpd = TRUE)
plot(sf::st_geometry(frame), border = "black", col = NA, lwd = 1, 
     add = TRUE, xpd = TRUE)


## Axes ----

text(axes[axes$"side" == 1, c("x", "y")], axes[axes$"side" == 1, "text"], 
     pos = 1, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 2, c("x", "y")], axes[axes$"side" == 2, "text"], 
     pos = 2, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 3, c("x", "y")], axes[axes$"side" == 3, "text"], 
     pos = 3, xpd = TRUE, cex = 0.65, col = "#666666")
text(axes[axes$"side" == 4, c("x", "y")], axes[axes$"side" == 4, "text"], 
     pos = 4, xpd = TRUE, cex = 0.65, col = "#666666")


# ## Legend ----
# 
# rect(x_start, y_start - y_inc, x_start + x_inc * 10, y_start + y_inc,
#      border = "white", col = "white", lwd = 2)
# 
# for (i in 1:nrow(cats)) {
#   
#   rect(x_start + x_inc * (i - 1), 
#        y_start - y_inc, 
#        x_start + x_inc * i, 
#        y_start + y_inc,
#        border = NA, col = cats[i, "couleur"])
# }
# 
# text(x_start, y_start - y_inc / 1.5, "0.0", pos = 1, cex = 1)
# text(x_start + x_inc * 5, y_start - y_inc / 1.5, "0.5", pos = 1, cex = 1)
# text(x_start + x_inc * 10, y_start - y_inc / 1.5, "1.0", pos = 1, cex = 1)
# 
# text(x_start + x_inc * 5, y_start + y_inc / 1.5, 
#      "Mean probability of\nprotecting unprotected areas", pos = 3, cex = 1, font = 2)
# 
mtext("a", line = 0.75, side = 3, adj = 0, font = 2, cex = 1.2)


### 
### MARINE MAP
### 



## Prepare data ----

i <- 2

## Load Data ----

datas <- get(load(here::here("data", paste0("datZ", regions[i], ".RData"))))
preds <- get(load(here::here("data", paste0("res_prob_", tolower(llabels[i]), ".Rdata"))))
preds <- preds[ , c("PA", "corr.rf.response")]
colnames(preds) <- c("ID", "PredictedProtection")
datas <- merge(datas, preds, by = "ID", all = FALSE)


## Load Raster ----

ras   <- raster::raster(here::here("data", paste0("Cells", regions[i],
                                                  ".tif")))


## Add cells coordinates to Data ----

cells <- which(ras[] %in% datas$"ID")
xy    <- data.frame(ID = ras[][cells], raster::xyFromCell(ras, cells))

dat <- merge(xy, datas, by = "ID", all = TRUE)


# ## Compute Top10 ----
# 
# rank <- 0.9 * max(dat$RankZ)
# dat$"top10" <- FALSE
# dat[which(dat$RankZ >= rank), "top10"] <- TRUE
# 
# 
# ## Select Top10 ----
# 
# dat <- dat[dat$"top10" == TRUE, ]


## Convert to sf ----

tab <- sf::st_as_sf(dat, coords = c("x", "y"), crs = mollweide)
tab <- sf::st_transform(tab, robinson)


## Intersect with country ----

x <- sf::st_intersects(tab, eez, sparse = FALSE)

for (k in 1:nrow(eez)) {
  pos <- which(x[ , k])
  if (length(pos))
    eez[k, "mean_proba"] <- mean(tab[pos, "PredictedProtection", drop = TRUE])
}


## Categorize ----

increment <- 0.1

# cats <- data.frame(from = seq(0, 1 - increment, by = increment),
#                    to   = seq(0 + increment, 1, by = increment))

cats <- data.frame(from = c(0, 0.05, 0.1, 0.2, 0.4, 0.8),
                   to   = c(0.05, 0.1, 0.2, 0.4, 0.8, 1))

# cats$"couleur" <- c('#a50026', '#d73027', '#f46d43', '#fdae61', '#fee090',
#                     '#81B8D7', '#659AC7', '#4D7AB7', '#38559C', '#24286f')

cats$"couleur" <- c('#a50026', '#d73027', '#f46d43', '#fdae61',
                    '#659AC7', '#24286f')

for (j in 1:nrow(cats)) {
  
  pos <- which(eez$"mean_proba" >= cats[j, "from"] & eez$"mean_proba" < cats[j, "to"])
  
  if (length(pos))
    eez[pos, "couleur"] <- cats[j, "couleur"]
}

pos <- which(is.na(eez$"couleur"))
if (length(pos) > 0)
  eez[pos, "couleur"] <- "#dddddd"



par(mar = c(4, 2, 3, 2), family = "Roboto")


## Base map ----

plot(sf::st_geometry(frame), border = NA, col = "#dddddd")
plot(sf::st_geometry(ocean), border = col_grat, col = col_sea, lwd = 0.2, 
     add = TRUE)
plot(sf::st_geometry(grats), col = col_grat, lwd = 0.2, add = TRUE)


## Masks ----

# plot(sf::st_geometry(world), border = col_sea, col = "#dddddd", lwd = 0.1, 
#      add = TRUE)
plot(sf::st_geometry(eez), border = "white", col = eez$"couleur", lwd = 0.4, 
     add = TRUE)
plot(sf::st_geometry(eez), border = "#aaaaaa", col = NA, lwd = 0.2, 
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
# 
# rect(x_start, y_start - y_inc, x_start + x_inc * 10, y_start + y_inc,
#      border = "white", col = "white", lwd = 2)
# 
# for (i in 1:nrow(cats)) {
#   
#   rect(x_start + x_inc * (i - 1), 
#        y_start - y_inc, 
#        x_start + x_inc * i, 
#        y_start + y_inc,
#        border = NA, col = cats[i, "couleur"])
# }
# 
# text(x_start, y_start - y_inc / 1.5, "0.0", pos = 1, cex = 1)
# text(x_start + x_inc * 5, y_start - y_inc / 1.5, "0.5", pos = 1, cex = 1)
# text(x_start + x_inc * 10, y_start - y_inc / 1.5, "1.0", pos = 1, cex = 1)
# 
# text(x_start + x_inc * 5, y_start + y_inc / 1.5, 
#      "Mean probability of\nprotecting unprotected areas", pos = 3, cex = 1, font = 2)

mtext("b", line = 0.75, side = 3, adj = 0, font = 2, cex = 1.2)



## Legend ----

par(mar = rep(0, 4), family = "Roboto")

plot(0, xlim = c(-0.75, 1.75), ylim = c(-1, 4), type = "n", ann = FALSE, bty = "n")


rect(0, 1, 1, 2, border = "white", col = "white", lwd = 2)


for (i in 1:nrow(cats)) {

  rect(cats[i, "from"],
       1,
       cats[i, "to"],
       2,
       border = NA, col = cats[i, "couleur"])
  
  if (i != 2) {
  
    text(cats[i, "from"], 1, format(cats[ , "from"])[i], pos = 1, cex = 1.15)
  }
}

text(cats[i, "to"], 1, format(cats[ , "to"])[i], pos = 1, cex = 1.15)

text(0.5, 2, 
     "Mean probability of protecting unprotected areas", pos = 3, cex = 1.5, font = 2)
    
# 
# text(x_start, y_start - y_inc / 1.5, "0.0", pos = 1, cex = 1)
# text(x_start + x_inc * 5, y_start - y_inc / 1.5, "0.5", pos = 1, cex = 1)
# text(x_start + x_inc * 10, y_start - y_inc / 1.5, "1.0", pos = 1, cex = 1)
# 
# 

