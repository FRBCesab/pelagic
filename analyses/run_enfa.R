

for (group in groupes) {



#'  -------------------------------------------------------------------------   @TransformData

  datas[[group]][ , "NGO"]        <- log10(datas[[group]][ , "NGO"] + 1)

  if (group == "marine") {

    datas[[group]][ , "catPA"]     <- factor(datas[[group]][ , "catPA"], levels = c('NPA','MPA','NO TAKE'), ordered = TRUE)
    datas[[group]][ , "Distance"]  <- log10(datas[[group]][ , "Distance"] + 1)
    datas[[group]][ , "MeanBathy"] <- log10(-1 * (datas[[group]][ , "MeanBathy"]) + 1)

  } else {

    datas[[group]][ , "catPA"]    <- factor(datas[[group]][ , "catMPA"], levels = c('NPA','TPA','NO TAKE'), ordered = TRUE)
    datas[[group]][ , "Altitude"] <- log10(datas[[group]][ , "Altitude"] + 1)
    datas[[group]][ , "NatRes"]   <-
      datas[[group]][ , "NatRes"] /
      max(datas[[group]][ , "NatRes"]) +
      datas[[group]][ , "AgricultureDep"]
  }



#'  -------------------------------------------------------------------------   @SelectData


  if (group == "marine") {

    variables <- c(
      "HDI", "FisheriesDep", "ChloMean", "SSTMean", "TravelTime", "MeanBathy",
      "ConflictScore", "DistanceToMainland", "Voice", "Distance", "Island",
      "NGO", "catPA"
    )

    # variables <- c(
    #   "HDI", "FisheriesDep", "TravelTime",
    #   "ConflictScore", "Voice", "NGO", "catPA"
    # )

    # variables <- c(
    #   "ChloMean", "SSTMean", "MeanBathy",
    #   "DistanceToMainland", "Distance", "Island", "catPA"
    # )

  } else {

    variables <- c(
      "HDI", "PopDensity", "Precipitation", "MeanTemperature", "NatRes",
      "Altitude", "ConflictScore", "DistanceToOcean", "Voice", "FireActivity",
      "Island", "NGO", "catPA"
    )

    # variables <- c(
    #   "HDI", "PopDensity", "NatRes",
    #   "ConflictScore", "Voice", "NGO", "catPA"
    # )

    # variables <- c(
    #   "Precipitation", "MeanTemperature", "Altitude",
    #   "DistanceToOcean", "FireActivity", "Island", "catPA"
    # )
  }

  datas[[group]] <- datas[[group]][ , variables]



#'  -------------------------------------------------------------------------   @RunPCAENFA


  pca_list[[group]]  <- ade4::dudi.pca(datas[[group]][ , -ncol(datas[[group]])], scannf = FALSE, nf = 2)
  enfa_list[[group]] <- adehabitatHS::enfa(pca_list[[group]], c(datas[[group]][ , "catPA"]), scannf = FALSE)



#'  -------------------------------------------------------------------------   @GetIndsCoords


  datas[[group]][ , "PCA1"] <- enfa_list[[group]]$li[ , 1]
  datas[[group]][ , "PCA2"] <- enfa_list[[group]]$li[ , 2]



#'  -------------------------------------------------------------------------   @GetVarsCoords


  coords <- data.frame(
    variable = rownames(enfa_list[[group]]$co),
    enfa_list[[group]]$co,
    row.names = NULL
  )
  colnames(coords)[-1] <- c("PCA1", "PCA2")



#'  -------------------------------------------------------------------------   @AddVarsLabels


  coords <- merge(
    x    = vars_list[[group]],
    y    = coords,
    by.x = "code",
    by.y = "variable",
    all  = FALSE
  )
  socio      <- coords[coords[ , "family"] == "Socioeconomic", ]
  envir      <- coords[coords[ , "family"] != "Socioeconomic", ]

  socio      <- socio[order(abs(socio[ , "PCA1"]), decreasing = FALSE), ]
  envir      <- envir[order(abs(envir[ , "PCA1"]), decreasing = FALSE), ]

  vars_coords[[group]] <- rbind(envir, socio)
}
