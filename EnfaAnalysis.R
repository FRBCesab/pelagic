#---Marine Protected Areas

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


pc <- dudi.pca(dataIUCNATP_All_selected[,-13], scannf=FALSE, nf = 2)

# The data are then ready to be used in ENFA to define 'ecologically more meaningful' axes:
en <- adehabitatHS::enfa(pc, c(dataIUCNATP_All$catMPA), scan=FALSE)

dataIUCNATP_All_selected$Axis_1 <- en$li[,1]
dataIUCNATP_All_selected$Axis_2 <- en$li[,2]



spa<-subset(dataIUCNATP_All_selected,dataIUCNATP_All_selected$catPA=="NO TAKE")
rpa<-subset(dataIUCNATP_All_selected,dataIUCNATP_All_selected$catPA=="TPA")
npa<-subset(dataIUCNATP_All_selected,dataIUCNATP_All_selected$catPA=="NPA")

col.npa <- colorRampPalette(c("#999999", "#aaaaaa", "#eeeeee"))
col.rpa <- colorRampPalette(c("chartreuse4", "chartreuse2"))
col.spa <- colorRampPalette(c("olivedrab4", "olivedrab1"))

spa.z <- kde2d(spa$Axis_1 , spa$Axis_2, n=50)
rpa.z <- kde2d(rpa$Axis_1 , rpa$Axis_2, n=50)
npa.z<- kde2d(npa$Axis_1 , npa$Axis_2, n=50)

plot(dataIUCNATP_All_selected$Axis_1 ,dataIUCNATP_All_selected$Axis_2, xlab="X label", ylab="Y label", pch=19, cex=.4,type="n")
filled.contour(npa.z, drawlabels=FALSE, nlevels=k, col=col.npa(k), add=TRUE)
contour(rpa.z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
contour(spa.z, drawlabels=FALSE, nlevels=k, col=my.cols, add=TRUE)
