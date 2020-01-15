load("dataIUCNAMP_All.RData")

dataIUCNAMP_All$catPA<-as.factor(dataIUCNAMP_All$catPA)
dataIUCNAMP_All$NGO<-log10(dataIUCNAMP_All$NGO+1)
dataIUCNAMP_All$DistanceKm<-log10(dataIUCNAMP_All$DistanceSeaMount+1)
dataIUCNAMP_All$MeanBathy<-log10(-dataIUCNAMP_All$MeanBathy+1)
dataIUCNAMP_All_selected <-  dataIUCNAMP_All[,c( "HDI","FisheriesDep","ChloMean","SSTMean","TravelTime",
                                                 "MeanBathy","ConflictScore","DistanceToMainland","Voice","DistanceKm","Island","NGO","catPA")]
 
pc <- dudi.pca(dataIUCNAMP_All_selected[,-13], scannf=FALSE, nf = 2)

# The data are then ready to be used in ENFA to define 'ecologically more meaningful' axes:
en <- adehabitatHS::enfa(pc, c(dataIUCNAMP_All_selected$catPA), scan=FALSE)

dataIUCNAMP_All_selected$Axis_1 <- en$li[,1]
dataIUCNAMP_All_selected$Axis_2 <- en$li[,2]
