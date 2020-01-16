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
