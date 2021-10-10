#' Create a map frame
#'
#' @param xmin a numeric (min longitude of the frame)
#' @param xmax a numeric (max longitude of the frame)
#' @param ymin a numeric (min latitude of the frame)
#' @param ymax a numeric (max latitude of the frame)
#' @param crs a character of length 1 specifying the Coordinate Reference 
#'   System to project the frame. If `NA` (default) the frame is defined in
#'   the WGS84 standard.
#'
#' @return An `sf` POLYGON.
#' 
#' @export
#'
#' @examples
#' robinson <- paste("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84", 
#'                   "+datum=WGS84 +units=m +no_defs")
#'                   
#' land <- rnaturalearth::ne_countries(scale = "small", type = "countries", 
#'                                     returnclass = "sf")
#'                                     
#' land_rob <- sf::st_transform(land, robinson)
#' 
#' frame     <- map_frame(ymin = -90)
#' frame_rob <- map_frame(crs = robinson, ymin = -90)
#' 
#' par(mar = rep(0, 4), mfrow = c(2, 1))
#' plot(sf::st_geometry(frame))
#' plot(sf::st_geometry(land), add = TRUE)
#' plot(sf::st_geometry(frame_rob))
#' plot(sf::st_geometry(land_rob), add = TRUE)

map_frame <- function(xmin = -180, xmax = 180, ymin = -60, ymax = 90, 
                      crs = NA) {
  
  lon_lat  <- paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", 
                    "+towgs84=0,0,0")
  frame <- c(
    unlist(lapply(seq(xmin, xmax,  0.5), function(lon) c(lon, ymax))),
    unlist(lapply(seq(ymax, ymin, -0.1), function(lat) c(xmax, lat))),
    unlist(lapply(seq(xmax, xmin, -0.5), function(lon) c(lon, ymin))),
    unlist(lapply(seq(ymin, ymax,  0.1), function(lat) c(xmin, lat)))
  )

  frame <- matrix(frame, ncol = 2, byrow = TRUE)
  frame <- sf::st_polygon(list(frame))
  frame <- sf::st_sfc(frame, crs = lon_lat)
  
  if (!is.na(crs)) {
    frame <- sf::st_transform(frame, crs)
  }
  
  frame
}



#' Create map graticules
#'
#' @param xmin a numeric (min longitude of the map extent)
#' @param xmax a numeric (max longitude of the map extent)
#' @param ymin a numeric (min latitude of the map extent)
#' @param ymax a numeric (max latitude of the map extent)
#' @param meridians a vector of longitudes at which meridians will be created
#' @param parallels a vector of latitudes at which parallels will be created
#' @param crs a character of length 1 specifying the Coordinate Reference 
#'   System to project graticules. If `NA` (default) graticules are defined in
#'   the WGS84 standard.
#'
#' @return An `sf` MULTILINESTRING.
#'
#' @export
#'
#' @examples
#' robinson <- paste("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84", 
#'                   "+datum=WGS84 +units=m +no_defs")
#' 
#' frame <- map_frame(ymin = -90)
#' frame_rob <- map_frame(crs = robinson, ymin = -90)
#' 
#' grats     <- map_graticules(ymin = -90, parallels = seq( -80,  80, 20))
#' grats_rob <- map_graticules(crs = robinson, ymin = -90, 
#'                             parallels = seq( -80,  80, 20))
#' 
#' par(mar = rep(0, 4), mfrow = c(2, 1))
#' plot(sf::st_geometry(frame))
#' plot(sf::st_geometry(grats), add = TRUE)
#' 
#' plot(sf::st_geometry(frame_rob))
#' plot(sf::st_geometry(grats_rob), add = TRUE)

map_graticules <- function(xmin = -180, xmax = 180, ymin = -60, ymax = 90, 
                           meridians = seq(-160, 160, 40), 
                           parallels = seq( -60,  90, 20), crs = NA) {
  
  lon_lat  <- paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", 
                    "+towgs84=0,0,0")
  
  graticules <- c(
    lapply(meridians, function(lon) cbind(lon, seq(ymin, ymax, 0.1))),
    lapply(parallels, function(lat) cbind(seq(xmin, xmax, 0.5), lat)))
  
  graticules <- sf::st_multilinestring(graticules)
  graticules <- sf::st_sfc(graticules, crs = lon_lat)
  
  if (!is.na(crs)) {
    graticules <- sf::st_transform(graticules, crs)
  }
  
  graticules
}



map_axes <- function(xmin = -180, xmax = 180, ymin = -60, ymax = 90, 
                     meridians = seq(-160, 160, 40), 
                     parallels = seq( -60,  90, 20), crs = NA) {
  
  lon_lat  <- paste("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84", 
                    "+towgs84=0,0,0")
  
  axis_1 <- data.frame("x"    = meridians,
                       "y"    = ymin,
                       "side" = 1,
                       "text" = ifelse(meridians == 0, 
                                       paste0(meridians, "\u00B0"),
                                       ifelse(meridians < 0, 
                                              paste0(-1 * meridians, "\u00B0E"),
                                              paste0(meridians, "\u00B0O"))))
  
  axis_2 <- data.frame("x"    = xmin,
                       "y"    = parallels,
                       "side" = 2,
                       "text" = ifelse(parallels == 0, 
                                       paste0(parallels, "\u00B0"),
                                       ifelse(parallels < 0, 
                                              paste0(-1 * parallels, "\u00B0S"),
                                              paste0(parallels, "\u00B0N"))))
  
  axis_3 <- data.frame("x"    = meridians,
                       "y"    = ymax,
                       "side" = 3,
                       "text" = ifelse(meridians == 0, 
                                       paste0(meridians, "\u00B0"),
                                       ifelse(meridians < 0, 
                                              paste0(-1 * meridians, "\u00B0E"),
                                              paste0(meridians, "\u00B0O"))))
  
  axis_4 <- data.frame("x"    = xmax,
                       "y"    = parallels,
                       "side" = 4,
                       "text" = ifelse(parallels == 0, 
                                       paste0(parallels, "\u00B0"),
                                       ifelse(parallels < 0, 
                                              paste0(-1 * parallels, "\u00B0S"),
                                              paste0(parallels, "\u00B0N"))))
  
  coords <- rbind(axis_1, axis_2, axis_3, axis_4)
  coords <- sf::st_as_sf(coords, coords = 1:2, crs = lon_lat)
  
  if (!is.na(crs)) {
    coords <- sf::st_transform(coords, crs)
  }
  
  coords <- data.frame(sf::st_coordinates(coords), sf::st_drop_geometry(coords))
  colnames(coords) <- tolower(colnames(coords))
  
  coords
}
