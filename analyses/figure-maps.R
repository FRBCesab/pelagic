regions <- c("Land", "Sea")
llabels <- c("Terrestrial", "Marine")

proj4 <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


## Import World PA ----

# shp <- list.files(path = here::here("data", "wdpa"), recursive = TRUE, 
#                   pattern = "polygons\\.shp$", full.names = TRUE)
# 
# shp <- lapply(shp, function(x) sf::st_read(x))
# shp <- do.call(rbind.data.frame, shp)
# shp <- sf::st_transform(shp, proj4)

shp <- readRDS(here::here("data", "wdpa_polygons.rds"))


## Import Basemap Layer ----

world_ori <- rnaturalearth::ne_countries(scale = "medium", type = "countries", 
                                         returnclass = "sf")
world <- sf::st_transform(world_ori, proj4)
world <- world[world$admin != "Antarctica", ]


## Map Border ----

ext <- raster::extent(-180, 180, -90, 90)
ext <- as(ext, 'SpatialPolygons')  
sp::proj4string(ext) <- raster::projection(world_ori)


for (i in 1:length(regions)) {
  
  
  ## Load Data ----
  
  datas <- get(load(here::here("data", paste0("datZ", regions[i], ".RData"))))
  
  
  ## Load Raster ----
  
  ras   <- raster::raster(here::here("data", paste0("Cells", regions[i], 
                                                    ".tif")))
  
  
  ## Add cells coordinates to Data ----
  
  cells <- which(ras[] %in% datas$ID)
  xy    <- data.frame(ID = ras[][cells], raster::xyFromCell(ras, cells))
  
  dat <- merge(xy, datas, by = "ID", all = TRUE)
  
  
  ## Subset Top5 ----
  
  dat <- dat[dat$top5 == TRUE, ]
  
  
  ## Convert to sf ----
  
  tab <- sf::st_as_sf(dat, coords = c("x", "y"), crs = proj4)
  
  
  
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


## Maps ----

couleurs <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8',
              '#abd9e9','#74add1','#4575b4','#313695')
couleurs <- rev(couleurs)
couleurs <- colorRampPalette(couleurs[-c(2, 10)])
couleurs <- rev(couleurs(10))




## Marine map ----

couleurs <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8',
              '#abd9e9','#74add1','#4575b4','#313695')
# reds <- c('#d73027','#f46d43','#fdae61', '#fee090')
# blues <- c('#e0f3f8','#abd9e9','#74add1','#4575b4','#313695')
# blues <- colorRampPalette(blues)(6)
# couleurs <- c(reds, blues)

# plot(1:length(couleurs), rep(1, length(couleurs)), pch = 19, cex = 4, col = couleurs)

mar_ap <- shp[shp$"MARINE" == 2, ]
mar_ap <- mar_ap[mar_ap$"IUCN_CAT" %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"), ]
mar_ap <- mar_ap[mar_ap$"GIS_AREA" > 10000, ]

sf_obj <- SF_OBJ[SF_OBJ$"Region" == "Marine", ]

mar_plot <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(data = mar_ap, fill = NA, color = "#486d31", size = 0.4) +
  
  ggplot2::geom_sf(data = world, fill = "#dedede", color = "white", size = 0.1) +
  
  ggplot2::geom_sf(data = sf_obj, ggplot2::aes(fill = category), 
                   color = NA, size = 0.1) +
  
  ggplot2::scale_fill_gradientn(colours = couleurs) +
  
  ggplot2::coord_sf(crs = proj4) +
  
  ggplot2::theme_bw() +
  
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = ggplot2::element_blank(),
                 strip.text.y = ggplot2::element_blank())

ggplot2::ggsave(mar_plot, filename = here::here("figures", "marine-map.pdf"),
                width = 12, height = 7, units = "in")


# 
# ggplot2::ggsave(filename = here::here("figures", "figure_maps.png"),
#                 width = 12, height = 12, dpi = 600, units = "in", pointsize = 14)
# 


## Terrestrial map ----

couleurs <- c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8',
              '#abd9e9','#74add1','#4575b4','#313695')
couleurs <- rev(couleurs)
couleurs <- colorRampPalette(couleurs[-c(2, 10)])
couleurs <- rev(couleurs(10))

# plot(1:length(couleurs), rep(1, length(couleurs)), pch = 19, cex = 4, col = couleurs)

ter_ap <- shp[shp$"MARINE" == 0, ]
ter_ap <- ter_ap[ter_ap$"IUCN_CAT" %in% c("Ia", "Ib", "II", "III", "IV", "V", "VI"), ]
ter_ap <- ter_ap[ter_ap$"GIS_AREA" > 5000, ]

sf_obj <- SF_OBJ[SF_OBJ$"Region" == "Terrestrial", ]

ter_plot <- ggplot2::ggplot() +
  
  ggplot2::geom_sf(data = world, fill = "#dedede", color = "white", 
                   size = 0.1) +
  
  ggplot2::geom_sf(data = ter_ap, fill = NA, color = "#486d31", size = 0.4) +
  
  ggplot2::geom_sf(data = sf_obj, ggplot2::aes(fill = category), 
                   color = NA, size = 0.1) +
  
  ggplot2::scale_fill_gradientn(colours = couleurs) +
  
  ggplot2::coord_sf(crs = proj4) +
  
  ggplot2::theme_bw() +
  
  ggplot2::theme(legend.position = "bottom", 
                 legend.title = ggplot2::element_blank(),
                 strip.text.y = ggplot2::element_blank())

ggplot2::ggsave(ter_plot, filename = here::here("figures", "terrestrial-map.pdf"),
                width = 12, height = 7, units = "in")
