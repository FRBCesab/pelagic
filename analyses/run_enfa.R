#' Perform a PCA Followed by a ENFA
#'
#' This script performs a PCA followed by a ENFA.
#'
#' @author Nicolas CASAJUS, \email{nicolas.casajus@@fondationbiodiversite.fr},
#'         Nicolas LOISEAU, \email{nicolas.loiseau1@@gmail.com}
#'
#' @date 2020/06/30


for (group in groupes) {

  ## Transform Data ----
  
  if (group == "marine") {
    
    var_to_log <- c("conflicts", "ngo", "dist_to_coast", "travel_time", 
                    "dist_to_seamounts", "chloro_a")
  } else {
    
    var_to_log <- c("conflicts", "ngo", "dist_to_ocean", "pop_density", 
                    "annual_precipitation")
  }
  
  datas[[group]][ , var_to_log] <- log10(datas[[group]][ , var_to_log] + 1)
  
  
  ## Create PA Categories ----

  datas[[group]]$"pa_category" <- ifelse(
    datas[[group]]$"iucn_category" %in% highly_restricted,
    "Highly Restricted",
    ifelse(
      datas[[group]]$"iucn_category" == "NONE",
      "None",
      "Restricted"
    )
  )
  
  datas[[group]]$"pa_category" <- factor(datas[[group]]$"pa_category", 
                                         levels  = categories, 
                                         ordered = TRUE)
  
  ## Select Columns ----

  columns <- vars_list[[group]]$"code"
  
  if (variables_type == "Socioeconomic") {
    columns <- columns[which(columns %in% vars_list[[group]][vars_list[[group]]$"family" == "Socioeconomic", "code", drop = TRUE])]
  }
  
  if (variables_type == "Environment") {
    columns <- columns[which(columns %in% vars_list[[group]][vars_list[[group]]$"family" == "Environment", "code", drop = TRUE])]
  }
  
  columns <- c(columns, "pa_category")
  datas[[group]] <- datas[[group]][ , columns]
  
  
  ## Hack ----

  

  ## Run PCA & ENFA ----

  pca_list[[group]]  <- ade4::dudi.pca(
    df     = datas[[group]][ , -ncol(datas[[group]])], 
    scannf = FALSE, 
    nf     = 2
  )
  
  enfa_list[[group]] <- adehabitatHS::enfa(
    dudi   = pca_list[[group]], 
    pr     = as.numeric(datas[[group]]$"pa_category"), 
    scannf = FALSE
  )


  ## Get Cells Coordinates ----

  datas[[group]][ , "PCA1"] <- enfa_list[[group]]$li[ , 1]
  datas[[group]][ , "PCA2"] <- enfa_list[[group]]$li[ , 2]


  ## Get Variables Coordinates ----

  coords <- data.frame(
    variable  = rownames(enfa_list[[group]]$co),
    enfa_list[[group]]$co,
    row.names = NULL
  )
  
  colnames(coords)[-1] <- c("PCA1", "PCA2")


  ## Add Variables Labels ----

  coords <- merge(
    x    = vars_list[[group]],
    y    = coords,
    by.x = "code",
    by.y = "variable",
    all  = FALSE
  )
  
  
  ## Order Variables (by Category then by x-Axis Coordinates) ----
  
  socio <- coords[coords$"family" == "Socioeconomic", ]
  envir <- coords[coords$"family" != "Socioeconomic", ]

  socio <- socio[order(abs(socio$"PCA1"), decreasing = FALSE), ]
  envir <- envir[order(abs(envir$"PCA1"), decreasing = FALSE), ]

  vars_coords[[group]] <- rbind(envir, socio)
  
}
