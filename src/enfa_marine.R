#---Marine Protected Areas

load(file.path("data", "dataIUCNAMP_All.RData"))

vars <- readr::read_csv(file.path("data", "list_variables_marine.csv"))

dataIUCNAMP_All$catPA      <- as.factor(dataIUCNAMP_All$catPA)
dataIUCNAMP_All$NGO        <- log10(dataIUCNAMP_All$NGO+1)
dataIUCNAMP_All$DistanceKm <- log10(dataIUCNAMP_All$DistanceSeaMount+1)
dataIUCNAMP_All$MeanBathy  <- log10(-dataIUCNAMP_All$MeanBathy+1)

variables <- c(
  "HDI", "FisheriesDep", "ChloMean", "SSTMean", "TravelTime", "MeanBathy",
  "ConflictScore", "DistanceToMainland", "Voice", "DistanceKm", "Island",
  "NGO", "catPA"
)

dataIUCNAMP_All_selected <- dataIUCNAMP_All[ , variables]

pc <- ade4::dudi.pca(dataIUCNAMP_All_selected[ , -13], scannf = FALSE, nf = 2)

# The data are then ready to be used in ENFA to define 'ecologically more meaningful' axes:
en <- adehabitatHS::enfa(pc, c(dataIUCNAMP_All_selected$catPA), scan = FALSE)

dataIUCNAMP_All_selected$Axis_1 <- en$li[ , 1]
dataIUCNAMP_All_selected$Axis_2 <- en$li[ , 2]

var_coords <- data.frame(variable = rownames(en$co), en$co, row.names = NULL)
colnames(var_coords)[-1] <- c("PCA1", "PCA2")

vars <- merge(vars, var_coords, by.x = "code", by.y = "variable", all = TRUE)
vars0 <- vars[vars$family == "Socioeconomic", ]
vars1 <- vars[vars$family != "Socioeconomic", ]

vars0 <- vars0[order(abs(vars0$PCA1), decreasing = FALSE), ]
vars1 <- vars1[order(abs(vars1$PCA1), decreasing = FALSE), ]

vars <- rbind(vars1, vars0)
