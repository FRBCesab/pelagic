


#---Terrestrial Protected Areas
load("dataIUCNATP_All.RData")

dataIUCNATP_All$NGO<-log10(dataIUCNATP_All$NGO+1)
dataIUCNATP_All$Altitude<-log10(dataIUCNATP_All$Altitude+1)
natresources=dataIUCNATP_All$Naturalresourcesrents/max(dataIUCNATP_All$Naturalresourcesrents)+ dataIUCNATP_All$AgricultureDep
dataIUCNATP_All=cbind(dataIUCNATP_All,natresources)
dataIUCNATP_All$catPA<-as.factor(dataIUCNATP_All$catPA)
dataIUCNATP_All_selected <-  dataIUCNATP_All[,c("HDI","PopDensity","Precipitation","MeanTemperature",
                                                "natresources","Altitude","ConflictScore",
                                                "DistanceToOcean","Voice","FireActivity","Island","NGO","catPA")]


pc <- ade4::dudi.pca(dataIUCNATP_All_selected[,-13], scannf=FALSE, nf = 2)

# The data are then ready to be used in ENFA to define 'ecologically more meaningful' axes:
en <- adehabitatHS::enfa(pc, c(dataIUCNATP_All$catMPA), scan=FALSE)
