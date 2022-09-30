old <- get(load("data/cov_imp_ATP.RData"))
new <- get(load("data/IUCN.I.VI.ATP.RData"))
dat <- get(load("data/IUCN_I_VI_ATP_clean.RData"))

new <- new[ , -c(1, 6:7, 12, 16)]

colnames(new) <- c("long", "lat", "iucn_category", "reserve", "conflicts", "voice",
                   "natural_resources", "ngo", "altitude", "dist_to_ocean",
                   "island", "annual_precipitation", "mean_temperature",
                   "freshwater", "gdp", "accessibility", "ndvi", "human_footprint")

pas <- read.csv("data/pa_cats.csv")

new <- merge(new, pas, by = "iucn_category", all = TRUE)

lonlat <- new[ , c("long","lat", "iucn_category", "reserve")]
new <- new[ , -c(2:4)]

datas_ter <- new
save(datas_ter, file = "data/IUCN_I_VI_ATP_clean.RData")
save(lonlat, file = "data/IUCN_I_VI_ATP_coords.RData")



################################################################################



old <- get(load("data/cov_imp_AMP.RData"))
new <- get(load("data/IUCN.I.VI.AMP.RData"))
dat <- get(load("data/IUCN_I_VI_AMP_clean.RData"))

new <- new[ , -c(1, 8)]

colnames(new) <- c("iucn_category", "reserve", "long", "lat", "conflicts", "voice", "ngo",
                   "marine_ecosystem_dependency", "bathymetry", "dist_to_coast",
                   "accessibility", "island", "dist_to_seamounts", "sst", "chloro_a",
                   "salinity", "gdp", "human_footprint")

pas <- read.csv("data/pa_cats.csv")

new <- merge(new, pas, by = "iucn_category", all = TRUE)

lonlat <- new[ , c("long","lat", "iucn_category", "reserve")]
new <- new[ , -c(2:4)]

datas_mar <- new
save(datas_mar, file = "data/IUCN_I_VI_AMP_clean.RData")
save(lonlat, file = "data/IUCN_I_VI_AMP_coords.RData")
