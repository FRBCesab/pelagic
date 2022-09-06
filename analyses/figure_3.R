setwd("C:/Users/lmannocc/Documents/LAURA/Marie Curie/PA_project")

library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(caret)
library(matrixStats)
library(grid)

##### Chargement des données

#  data = readxl::read_xlsx("DominanceVar.xlsx") %>%
#    data.frame

#load("data/BRT/BRT_MPA_IUCN_I_II.Rdata")  
#load("data/BRT/BRT_MPA_IUCN_I_IV.Rdata")  
#load("data/BRT/BRT_TPA_IUCN_I_II.Rdata")
#load("data/BRT/BRT_TPA_IUCN_I_IV.Rdata")


#ImpAMPI_II = summary(BRT_MPA_IUCN_I_II)
#ImpATPI_II =  summary(BRT_TPA_IUCN_I_II)
#ImpAMPI_IV =  summary(BRT_MPA_IUCN_I_IV)
#ImpATPI_IV =  summary(BRT_TPA_IUCN_I_IV)

load("data/score.AMP.All.RData")
ImpAMP_All <- score.cv %>%
  slice(-c(1:4))
VariablesAMP_All = rownames(ImpAMP_All)
ImpAMP_All <- (ImpAMP_All/max(ImpAMP_All)) %>%
  mutate(score.mean=rowMeans(.),score.sd = rowSds(as.matrix(.)))
load("data/score.AMP.I.RData")
ImpAMP_I <- score.cv %>%
  slice(-c(1:4))
VariablesAMP_I = rownames(ImpAMP_I)
ImpAMP_I <- (ImpAMP_I/max(ImpAMP_I)) %>%
  mutate(score.mean=rowMeans(.),score.sd = rowSds(as.matrix(.)))
load("data/score.ATP.All.RData")
ImpATP_All <- score.cv %>%
  slice(-c(1:4))
VariablesATP_All = rownames(ImpATP_All)
ImpATP_All <- (ImpATP_All/max(ImpATP_All)) %>%
  mutate(score.mean=rowMeans(.),score.sd = rowSds(as.matrix(.)))
load("data/score.ATP.I.RData")
ImpATP_I <- score.cv %>%
  slice(-c(1:4))
VariablesATP_I = rownames(ImpATP_I)
ImpATP_I <- (ImpATP_I/max(ImpATP_I)) %>%
  mutate(score.mean=rowMeans(.),score.sd = rowSds(as.matrix(.)))


###############AMP#############################
#### Mise en forme des données
data.A <- ImpAMP_All %>%
  select(c(score.mean,score.sd)) %>%
  data.frame(VariablesAMP_All)

colnames(data.A) = c("AMPI_VI.mean","AMPI_VI.sd","Variables")
data.A = data.frame(data.A)
#data$Variables = rownames(data)
data.A$Factors = c("Socioeconomic","Socioeconomic","Socioeconomic","Environmental","Environmental","Socioeconomic","Socioeconomic","Environmental","Environmental","Environmental","Environmental","Socioeconomic")
data.A = data.A[order(data.A$Factors),]
data.A$Variables = c("Chl. A","Mean SST","Mean Bathy","Dist. to seamount","Dist. to mainland","Island", "HDI","Conflict Score","Fish. Dep.","Travel T.","Voice","NGO")

data.B = ImpAMP_I %>%
  select(c(score.mean,score.sd)) %>%
  data.frame(VariablesAMP_I)
colnames(data.B) = c("AMPI.mean","AMPI.sd","Variables")
data.B = data.frame(data.B)
#data$Variables = rownames(data)
data.B$Factors = c("Socioeconomic","Socioeconomic","Socioeconomic","Environmental","Environmental","Socioeconomic","Socioeconomic","Environmental","Environmental","Environmental","Environmental","Socioeconomic")
data.B = data.B[order(data.B$Factors),]
data.B$Variables = c("Chl. A","Mean SST","Mean Bathy","Dist. to seamount","Dist. to mainland","Island", "HDI","Conflict Score","Fish. Dep.","Travel T.","Voice","NGO")

data = merge(data.A,data.B[,c(1,2,3)],by="Variables",sort=FALSE)

###############ATP#############################
#### Mise en forme des données
data2.A <- ImpATP_All %>%
  select(c(score.mean,score.sd)) %>%
  data.frame(VariablesATP_All)

colnames(data2.A) = c("ATPI_VI.mean","ATPI_VI.sd","Variables")
data2.A = data.frame(data2.A)
#data$Variables = rownames(data)
data2.A$Factors = c("Socioeconomic","Socioeconomic","Socioeconomic","Environmental","Environmental","Socioeconomic","Socioeconomic","Environmental","Environmental","Environmental","Environmental","Socioeconomic")
data2.A = data2.A[order(data2.A$Factors),]
data2.A$Variables =c("Prec.","Mean Temp.","Altitude", "Fresh water",  "Dist. to Ocean",  "Island", "HDI",  "Conflict Score", "Natural Res.", "Pop. Dens.","Voice","NGO")


data2.B <- ImpATP_I %>%
  select(c(score.mean,score.sd)) %>%
  data.frame(VariablesATP_I)

colnames(data2.B) = c("ATPI.mean","ATPI.sd","Variables")
data2.B = data.frame(data2.B)
#data$Variables = rownames(data)
data2.B$Factors = c("Socioeconomic","Socioeconomic","Socioeconomic","Environmental","Environmental","Socioeconomic","Socioeconomic","Environmental","Environmental","Environmental","Environmental","Socioeconomic")
data2.B = data2.B[order(data2.B$Factors),]
data2.B$Variables =c("Prec.","Mean Temp.","Altitude", "Fresh water",  "Dist. to Ocean",  "Island", "HDI",  "Conflict Score", "Natural Res.", "Pop. Dens.","Voice","NGO")



data2 = merge(data2.A,data2.B[,c(1,2,3)],by="Variables",sort=FALSE)




# Set a number of 'empty bar' to add at the end of each group
empty_bar=0
#to_add = data.frame( matrix(NA, empty_bar*length(unique(data$Factors)), ncol(data)) )
to_add = data.frame( matrix(NA, empty_bar, ncol(data)) )
colnames(to_add) = colnames(data)
to_add$Factors=rep("Human Footprint", each=empty_bar)
data=rbind(data, to_add)
#data=data %>% arrange(Factors)
data$id=seq(1, nrow(data))

# Set a number of 'empty bar' to add at the end of each group
empty_bar=0
#to_add = data.frame( matrix(NA, empty_bar*length(unique(data$Factors)), ncol(data)) )
to_add = data.frame( matrix(NA, empty_bar, ncol(data2)) )
colnames(to_add) = colnames(data2)
to_add$Factors=rep("Human Footprint", each=empty_bar)
data2=rbind(data2, to_add)
#data=data %>% arrange(Factors)
data2$id=seq(1, nrow(data2))



base_data=data %>% 
  group_by(Factors) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

base_data2=data2 %>% 
  group_by(Factors) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

#base_data[5,3] = 16
#base_data[5,4] = 15.5

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# prepare a data frame for grid (scales)
grid_data2 = base_data2
grid_data2$end = grid_data2$end[ c( nrow(grid_data2), 1:nrow(grid_data2)-1)] + 1
grid_data2$start = grid_data2$start - 1
grid_data2=grid_data2[-1,]

segment_data = matrix(NA,19,4)
segment_data  = as.data.frame(segment_data)
colnames(segment_data) = c("xstart","ystart","xend","yend")
segment_data[,1] = c(rep(0.5,6),seq(0.5,12.5,1))
segment_data[,2] = c(seq(0,1,0.2),rep(0,13))
segment_data[,3] = c(rep(12.5,6),seq(0.5,12.5,1))
segment_data[,4] = c(seq(0,1,0.2),rep(1,13))

segment_data2 = matrix(NA,19,4)
segment_data2  = as.data.frame(segment_data2)
colnames(segment_data2) = c("xstart","ystart","xend","yend")
segment_data2[,1] = c(rep(0.5,6),seq(0.5,12.5,1))
segment_data2[,2] = c(seq(0,1,0.2),rep(0,13))
segment_data2[,3] = c(rep(12.5,6),seq(0.5,12.5,1))
segment_data2[,4] = c(seq(0,1,0.2),rep(1,13))


# Get the name and the y position of each label
label_data=data
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse( angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# Get the name and the y position of each label
label_data2=data2
number_of_bar=nrow(label_data2)
angle= 90 - 360 * (label_data2$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data2$hjust<-ifelse( angle < -90, 1, 0)
label_data2$angle<-ifelse(angle < -90, angle+180, angle)



#cols = c("#053061","#4393c3","#80cdc1","#fee090","#d6604d","#67001f")
#cols = c("#619CFF","#00BFC4","#00BA38","#B79F00","#F564E3","#F8766D")

cols = c("#35978f","#bf812d")

label_data$Factors <- factor(label_data$Factors,levels=c("Environmental","Socioeconomic"),order=TRUE)
label_data2$Factors <- factor(label_data2$Factors,levels=c("Environmental","Socioeconomic"),order=TRUE)
#base_data$Factors <- factor(base_data$Factors,levels=c("Climate","Geography","Habitat diversity","Multiculturalisme","Governance capacity","Human Footprint"),order=TRUE)


databar <- data %>%
  select(c("AMPI_VI.mean","AMPI.mean","Factors")) %>%
  group_by(Factors) %>%
  summarise_all(sum) %>%
  arrange(desc(Factors))
databar$AMPI_VI.mean = databar$AMPI_VI.mean/sum(databar$AMPI_VI.mean)
databar$AMPI.mean = databar$AMPI.mean/sum(databar$AMPI.mean)
databar2 <- data2 %>%
  select(c("ATPI_VI.mean","ATPI.mean","Factors")) %>%
  group_by(Factors) %>%
  summarise_all(sum) %>%
  arrange(desc(Factors))
databar2$ATPI_VI.mean = databar2$ATPI_VI.mean/sum(databar2$ATPI_VI.mean)
databar2$ATPI.mean = databar2$ATPI.mean/sum(databar2$ATPI.mean)
#####################################################
p = ggplot(data, aes(x=as.factor(id), y=AMPI_VI.mean, fill=Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=AMPI_VI.mean, fill=Factors), stat="identity", alpha=0)  +
  scale_fill_manual(values=cols) +
  
  geom_segment(data=segment_data, aes(x = xstart, y = ystart, xend = xend, yend = yend), colour = "grey", alpha=0.5, size=0.09, inherit.aes = FALSE ) +
  geom_bar(aes(x=as.factor(id), y=AMPI_VI.mean, fill=Factors), stat="identity", alpha=0.6) +
  geom_linerange(aes(ymin=AMPI_VI.mean-AMPI_VI.sd, ymax=AMPI_VI.mean+AMPI_VI.sd),colour="gray47") +
  # Add text showing the value of each 100/75/50/25 lines
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  #geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(13,6), y = c(0, 0.2,0.4,0.6,0.8,1), label = c("0","0.2","0.4","0.6","0.8","1") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  #geom_bar(aes(x=as.factor(id), y=TPA, fill=c("blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue")), stat="identity", alpha=0) +
  ylim(-0.5,1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1.5,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=AMPI_VI.mean+0.04, label=Variables, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.9, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start-0.1, y = -0.05, xend = end+0.1, yend = -0.05), colour = cols, alpha=0.5, size=0.7 , inherit.aes = FALSE, )  +
  #geom_text(data=base_data, aes(x = title, y =-0.4, label=Factors), hjust=c(-0.1,0.2,0.4,0.5,0.5,0.5), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE) +
  #ggtitle("TPA")+
  theme(plot.title = element_text(vjust=-40))+
  theme(legend.text = element_text(colour="grey", size=10, 
                                   face="bold"),legend.position="none")
  #annotate("text", x = 0, y = -0.18, label = "MPA\nIUCN I-VI" , color="gray47", size=4 , angle=0, fontface="bold") +


#####################################################
p1 = ggplot(data, aes(x=as.factor(id), y=AMPI.mean, fill=Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=AMPI.mean, fill=Factors), stat="identity", alpha=0) +
  scale_fill_manual(values=cols) +
  
  geom_segment(data=segment_data, aes(x = xstart, y = ystart, xend = xend, yend = yend), colour = "grey", alpha=0.5, size=0.09, inherit.aes = FALSE ) +
  geom_bar(aes(x=as.factor(id), y=AMPI.mean, fill=Factors), stat="identity", alpha=0.6) +
  geom_linerange(aes(ymin=AMPI.mean-AMPI.sd, ymax=AMPI.mean+AMPI.sd), col="gray47") +
  # Add text showing the value of each 100/75/50/25 lines
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  #geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(13,6), y = c(0, 0.2,0.4,0.6,0.8,1), label = c("0","0.2","0.4","0.6","0.8","1") , color="grey", size=3 , angle=0, fontface="bold", hjust=1)  +
  
  #geom_bar(aes(x=as.factor(id), y=TPA, fill=c("blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue")), stat="identity", alpha=0) +
  ylim(-0.5,1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1.5,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=AMPI.mean+0.04,label=Variables, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.9, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start-0.1, y = -0.05, xend = end+0.1, yend = -0.05), colour = cols, alpha=0.5, size=0.7 , inherit.aes = FALSE, )  +
  #geom_text(data=base_data, aes(x = title, y =-0.4, label=Factors), hjust=c(-0.1,0.2,0.4,0.5,0.5,0.5), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE) +
  #ggtitle("TPA")+
  theme(plot.title = element_text(vjust=-40))+
  theme(legend.text = element_text(colour="grey", size=10, 
                                   face="bold"),legend.position="none")
  #annotate("text", x = 0, y = -0.18, label = "MPA\nIUCN I" , color="gray47", size=4 , angle=0, fontface="bold") +


#####################################################
p2 = ggplot(data2, aes(x=as.factor(id), y=ATPI_VI.mean, fill=Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=ATPI_VI.mean, fill=Factors), stat="identity", alpha=0) +
  scale_fill_manual(values=cols) +
  
  geom_segment(data=segment_data2, aes(x = xstart, y = ystart, xend = xend, yend = yend), colour = "grey", alpha=0.5, size=0.09, inherit.aes = FALSE ) +
  geom_bar(aes(x=as.factor(id), y=ATPI_VI.mean, fill=Factors), stat="identity", alpha=0.6) +
  geom_linerange(aes(ymin=ATPI_VI.mean-ATPI_VI.sd, ymax=ATPI_VI.mean+ATPI_VI.sd), col="gray47") +
  # Add text showing the value of each 100/75/50/25 lines
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  #geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(13,6), y = c(0, 0.2,0.4,0.6,0.8,1), label = c("0","0.2","0.4","0.6","0.8","1") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  #geom_bar(aes(x=as.factor(id), y=TPA, fill=c("blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue")), stat="identity", alpha=0) +
  ylim(-0.5,1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1.5,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data2, aes(x=id, y=ATPI_VI.mean+0.04, label=Variables, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.9, angle= label_data2$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data2, aes(x = start-0.1, y = -0.05, xend = end+0.1, yend = -0.05), colour = cols, alpha=0.5, size=0.7 , inherit.aes = FALSE)  +
  #geom_text(data=base_data, aes(x = title, y =-0.4, label=Factors), hjust=c(-0.1,0.2,0.4,0.5,0.5,0.5), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE) +
  #ggtitle("TPA")+
  theme(plot.title = element_text(vjust=-40))+
  theme(legend.text = element_text(colour="grey", size=10, 
                                   face="bold"),legend.position="none")
  #annotate("text", x = 0, y = -0.18, label = "TPA\nIUCN I-VI" , color="gray47", size=4 , angle=0, fontface="bold") +


###################################################

p3 = ggplot(data2, aes(x=as.factor(id), y=ATPI.mean, fill=Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=ATPI.mean, fill=Factors), stat="identity", alpha=0) +
  scale_fill_manual(values=cols) +
  
  geom_segment(data=segment_data2, aes(x = xstart, y = ystart, xend = xend, yend = yend), colour = "grey", alpha=0.5, size=0.09, inherit.aes = FALSE ) +
  geom_bar(aes(x=as.factor(id), y=ATPI.mean, fill=Factors), stat="identity", alpha=0.6) +
  geom_linerange(aes(ymin=ATPI.mean-ATPI.sd, ymax=ATPI.mean+ATPI.sd), col="gray47") +
  # Add text showing the value of each 100/75/50/25 lines
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  #geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0.5, xend = start, yend = 0.5), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  #geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(13,6), y = c(0, 0.2,0.4,0.6,0.8,1), label = c("0","0.2","0.4","0.6","0.8","1") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  #geom_bar(aes(x=as.factor(id), y=TPA, fill=c("blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue","red","blue")), stat="identity", alpha=0) +
  ylim(-0.5,1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1.5,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data2, aes(x=id, y=ATPI.mean+0.04, label=Variables, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.9, angle= label_data2$angle, inherit.aes = FALSE ) +

  # Add base line information
  geom_segment(data=base_data2, aes(x = start-0.1, y = -0.05, xend = end+0.1, yend = -0.05), colour = cols, alpha=0.5, size=1.5 , inherit.aes = FALSE, )  +
  #geom_text(data=base_data, aes(x = title, y =-0.4, label=Factors), hjust=c(-0.1,0.2,0.4,0.5,0.5,0.5), colour = "black", alpha=0.8, size=2.5, fontface="bold", inherit.aes = FALSE) +
  #ggtitle("TPA")+
  theme(plot.title = element_text(vjust=-40))+
  theme(legend.text = element_text(colour="grey", size=10, 
                                   face="bold"),legend.position="none")+
  #annotate("text", x = 0, y = -0.18, label = "TPA\nIUCN I" , color="gray47", size=4 , angle=0, fontface="bold") +
  theme(legend.text = element_text(colour="gray47", size=12, 
                                   face="bold"),legend.position="bottom",legend.title=element_text(colour="gray49",size=12, face="bold")) +
  guides(fill = guide_legend(byrow=TRUE)) 

#####################################################



# 2. Save the legend
#+++++++++++++++++++++++
legend <- get_legend(p3)
# 3. Remove the legend from the box plot
#+++++++++++++++++++++++
p3 <- p3 + theme(legend.position="none") 


### Create barplot

g = ggplot() +
  geom_bar(data = databar,mapping=aes(x=c(1,2),y=AMPI_VI.mean,fill=Factors),stat="identity", alpha=0.6) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=cols) +
  scale_x_continuous("", labels=c("",""),breaks = c(1,2)) +
  scale_y_continuous("",labels=c(0,0.2,0.4,0.6),breaks = c(0,0.2,0.4,0.6),limits = c(0,0.7))+
  theme(legend.position="none") +
  theme(axis.line.x = element_line(size = 0.4,color = "gray47"),axis.line.y = element_line(size = 0.4,color = "gray47"),axis.ticks.x = element_line(size=0.4,color = "gray47"),axis.ticks.y = element_line(size=0.4,color = "gray47"),axis.text=element_text(size=8,color = "gray47"))


g1 = ggplot() +
  geom_bar(data = databar,mapping=aes(x=c(1,2),y=AMPI.mean,fill=Factors),stat="identity", alpha=0.6) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=cols) +
  scale_x_continuous("", labels=c("",""),breaks = c(1,2)) +
  scale_y_continuous("",labels=c(0,0.2,0.4,0.6),breaks = c(0,0.2,0.4,0.6),limits = c(0,0.7))+
  theme(legend.position="none") +
  theme(axis.line.x = element_line(size = 0.4,color = "gray47"),axis.line.y = element_line(size = 0.4,color = "gray47"),axis.ticks.x = element_line(size=0.4,color = "gray47"),axis.ticks.y = element_line(size=0.4,color = "gray47"),axis.text=element_text(size=8,color = "gray47"))


g2 = ggplot() +
  geom_bar(data = databar2,mapping=aes(x=c(1,2),y=ATPI_VI.mean,fill=Factors),stat="identity", alpha=0.6) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=cols) +
  scale_x_continuous("", labels=c("",""),breaks = c(1,2)) +
  scale_y_continuous("",labels=c(0,0.2,0.4,0.6),breaks = c(0,0.2,0.4,0.6),limits = c(0,0.7))+
  theme(legend.position="none") +
  theme(axis.line.x = element_line(size = 0.4,color = "gray47"),axis.line.y = element_line(size = 0.4,color = "gray47"),axis.ticks.x = element_line(size=0.4,color = "gray47"),axis.ticks.y = element_line(size=0.4,color = "gray47"),axis.text=element_text(size=8,color = "gray47"))

g3 = ggplot() +
  geom_bar(data = databar2,mapping=aes(x=c(1,2),y=ATPI.mean,fill=Factors),stat="identity", alpha=0.6) +
  theme_minimal()+
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values=cols) +
  scale_x_continuous("", labels=c("",""),breaks = c(1,2)) +
  scale_y_continuous("",labels=c(0,0.2,0.4,0.6),breaks = c(0,0.2,0.4,0.6),limits = c(0,0.7))+
  theme(legend.position="none") +
  theme(axis.line.x = element_line(size = 0.4,color = "gray47"),axis.line.y = element_line(size = 0.4,color = "gray47"),axis.ticks.x = element_line(size=0.4,color = "gray47"),axis.ticks.y = element_line(size=0.4,color = "gray47"),axis.text=element_text(size=8,color = "gray47"))


### Insert barplot within circular plot
gg = ggdraw() +
  draw_plot(p, 0, 0, 1, 1, scale =0.8) +
  draw_plot(g, 0.4, 0.36, 0.2, 0.26)

gg1 = ggdraw() +
  draw_plot(p1, 0, 0, 1, 1, scale =0.8) +
  draw_plot(g, 0.4, 0.36, 0.2, 0.26)

gg2 = ggdraw() +
  draw_plot(p2, 0, 0, 1, 1, scale =0.8) +
  draw_plot(g, 0.39, 0.36, 0.22, 0.26)

gg3 = ggdraw() +
  draw_plot(p3, 0, 0, 1, 1, scale =0.8) +
  draw_plot(g, 0.39, 0.36, 0.22, 0.26)


#### Save pdf 
par(mar =c (0,0,0,0))
pdf("outputs/figure2.pdf", width = 8, height = 7)
grid.arrange(gg2 + theme(plot.margin = unit(c(1,0,0,1), "cm")), gg  + theme(plot.margin = unit(c(1,0,0,0), "cm")), 
             gg3 + theme(plot.margin = unit(c(0,0,0,1), "cm")), gg1, legend, ncol=2, nrow = 3, 
             layout_matrix = rbind(c(1,2), c(3,4),c(5,5)),
             widths = c(2.5, 2.5), heights = c(2.5,2.5, 0.4))
grid.text("a", x = unit(0.05, "npc"),
          y = unit(.92, "npc"),gp = gpar(fontsize=13))
grid.text("Terrestrial Protected Areas", x = unit(0.28, "npc"),
          y = unit(.98, "npc"),gp = gpar(fontsize=13))
grid.text("b", x = unit(0.53, "npc"),
          y = unit(.92, "npc"),gp = gpar(fontsize=13))
grid.text("Marine Protected Areas", x = unit(0.75, "npc"),
          y = unit(.98, "npc"),gp = gpar(fontsize=13))
grid.text("c", x = unit(0.05, "npc"),
          y = unit(.5, "npc"),gp = gpar(fontsize=13))
grid.text("All", x = unit(0.035, "npc"),
          y = unit(.8, "npc"),gp = gpar(fontsize=13))
grid.text("IUCN", x = unit(0.035, "npc"),
          y = unit(.75, "npc"),gp = gpar(fontsize=13))
grid.text("(I-VI)", x = unit(0.035, "npc"),
          y = unit(.7, "npc"),gp = gpar(fontsize=13))
grid.text("d", x = unit(0.53, "npc"),
          y = unit(.5, "npc"),gp = gpar(fontsize=13))
grid.text("IUCN I", x = unit(0.035, "npc"),
          y = unit(.32, "npc"),gp = gpar(fontsize=13))
grid.text("only", x = unit(0.035, "npc"),
          y = unit(.27, "npc"),gp = gpar(fontsize=13))
dev.off() 

