library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(caret)
library(matrixStats)


load("data/importance_permutation_marine.Rdata")
load("data/importance_permutation_marine_strict.Rdata")
load("data/importance_permutation_terrestrial.Rdata")
load("data/importance_permutation_terrestrial_strict.Rdata")


###############MPA#############################
#### Data formatting
data.A <- importance_permutation_marine %>%
  drop_na(rowname) %>%
  mutate(mod = rep(1:10, each = 14)) %>%
  group_by(mod) %>%
  mutate(value.std = (importance.mod. - min(importance.mod.)) / (max(importance.mod.) -
                                                                   min(importance.mod.))) %>%
  ungroup() %>%
  group_by(rowname) %>%
  mutate(AMP_All.mean = mean(value.std),
         AMP_All.sd = sd(value.std)) %>%
  slice(1) %>%
  select(rowname, AMP_All.mean, AMP_All.sd) %>%
  data.frame() %>%
  rename("Variables" = "rowname") %>%
  mutate(Factors = case_when(
    Variables %in% c(
      "Island",
      "SST",
      "logChla",
      "logDistToCoast",
      "logDistToSeamounts",
      "logdepth",
      "logsalinity"
    ) ~ "Environmental",
    TRUE ~ "Socioeconomic"
  )) %>%
  mutate(Variables = recode(Variables, "logMarineEcosystemDependency" = "Fish. Dep.")) %>%
  mutate(Variables = recode(Variables, "SST" = "Temp.")) %>%
  mutate(Variables = recode(Variables, "hf" = "H. Footprint")) %>%
  mutate(Variables = recode(Variables, "logChla" = "Chloro. a")) %>%
  mutate(Variables = recode(Variables, "logDistToCoast" = "Dist to Mainland")) %>%
  mutate(Variables = recode(Variables, "logDistToSeamounts" = "Dist to Seamounts")) %>%
  mutate(Variables = recode(Variables, "logHDI" = "HDI")) %>%
  mutate(Variables = recode(Variables, "logNGO" = "NGO")) %>%
  mutate(Variables = recode(Variables, "logdepth" = "Bathymetry")) %>%
  mutate(Variables = recode(Variables, "loggdp" = "GDP")) %>%
  mutate(Variables = recode(Variables, "logsalinity" = "Salinity")) %>%
  mutate(Variables = recode(Variables, "logtravel_time" = "Access.")) %>%
  mutate(Variables = recode(Variables, "logconflicts" = "Conflicts")) %>%
  arrange(Factors)



data.B <- importance_permutation_marine_strict %>%
  drop_na(rowname) %>%
  mutate(mod = rep(1:10, each = 14)) %>%
  group_by(mod) %>%
  mutate(value.std = (importance.mod. - min(importance.mod.)) / (max(importance.mod.) -
                                                                   min(importance.mod.))) %>%
  ungroup() %>%
  group_by(rowname) %>%
  mutate(AMP_I.mean = mean(value.std),
         AMP_I.sd = sd(value.std)) %>%
  slice(1) %>%
  select(rowname, AMP_I.mean, AMP_I.sd) %>%
  data.frame() %>%
  rename("Variables" = "rowname") %>%
  mutate(Factors = case_when(
    Variables %in% c(
      "Island",
      "SST",
      "logChla",
      "logDistToCoast",
      "logDistToSeamounts",
      "logdepth",
      "logsalinity"
    ) ~ "Environmental",
    TRUE ~ "Socioeconomic"
  )) %>%
  mutate(Variables = recode(Variables, "logMarineEcosystemDependency" = "Fish. Dep.")) %>%
  mutate(Variables = recode(Variables, "SST" = "Temp.")) %>%
  mutate(Variables = recode(Variables, "hf" = "H. Footprint")) %>%
  mutate(Variables = recode(Variables, "logChla" = "Chloro. a")) %>%
  mutate(Variables = recode(Variables, "logDistToCoast" = "Dist to Mainland")) %>%
  mutate(Variables = recode(Variables, "logDistToSeamounts" = "Dist to Seamounts")) %>%
  mutate(Variables = recode(Variables, "logHDI" = "HDI")) %>%
  mutate(Variables = recode(Variables, "logNGO" = "NGO")) %>%
  mutate(Variables = recode(Variables, "logdepth" = "Bathymetry")) %>%
  mutate(Variables = recode(Variables, "loggdp" = "GDP")) %>%
  mutate(Variables = recode(Variables, "logsalinity" = "Salinity")) %>%
  mutate(Variables = recode(Variables, "logtravel_time" = "Access.")) %>%
  mutate(Variables = recode(Variables, "logconflicts" = "Conflicts")) %>%
  arrange(Factors)

data = merge(data.A, data.B[, c(1, 2, 3)], by = "Variables", sort = FALSE)


###############TPA#############################
#### Data formatting

data2.A <- importance_permutation_terrestrial %>%
  drop_na(rowname) %>%
  mutate(mod = rep(1:10, each = 14)) %>%
  group_by(mod) %>%
  mutate(value.std = (importance.mod. - min(importance.mod.)) / (max(importance.mod.) -
                                                                   min(importance.mod.))) %>%
  ungroup() %>%
  group_by(rowname) %>%
  mutate(ATP_All.mean = mean(value.std),
         ATP_All.sd = sd(value.std)) %>%
  slice(1) %>%
  select(rowname, ATP_All.mean, ATP_All.sd) %>%
  data.frame() %>%
  rename("Variables" = "rowname") %>%
  mutate(Factors = case_when(
    Variables %in% c(
      "Island",
      "MeanTemperature",
      "NDVI",
      "logDistToOcean",
      "logAnnualPrecipitation",
      "logAltitude",
      "logFreshWater"
    ) ~ "Environmental",
    TRUE ~ "Socioeconomic"
  )) %>%
  mutate(Variables = recode(Variables, "logNaturalRessources" = "Natural Res.")) %>%
  mutate(Variables = recode(Variables, "MeanTemperature" = "Temp.")) %>%
  mutate(Variables = recode(Variables, "loghf" = "H. Footprint")) %>%
  mutate(Variables = recode(Variables, "logDistToOcean" = "Dist to Ocean")) %>%
  mutate(Variables = recode(Variables, "logAnnualPrecipitation" = "Precipitation")) %>%
  mutate(Variables = recode(Variables, "logHDI" = "HDI")) %>%
  mutate(Variables = recode(Variables, "logNGO" = "NGO")) %>%
  mutate(Variables = recode(Variables, "logAltitude" = "Altitude")) %>%
  mutate(Variables = recode(Variables, "logGDP" = "GDP")) %>%
  mutate(Variables = recode(Variables, "logFreshWater" = "Freshwater")) %>%
  mutate(Variables = recode(Variables, "logtt" = "Access.")) %>%
  mutate(Variables = recode(Variables, "logconflicts" = "Conflicts")) %>%
  arrange(Factors)



data2.B <-  importance_permutation_terrestrial_strict %>%
  drop_na(rowname) %>%
  mutate(mod = rep(1:10, each = 14)) %>%
  group_by(mod) %>%
  mutate(value.std = (importance.mod. - min(importance.mod.)) / (max(importance.mod.) -
                                                                   min(importance.mod.))) %>%
  ungroup() %>%
  group_by(rowname) %>%
  mutate(ATP_I.mean = mean(value.std),
         ATP_I.sd = sd(value.std)) %>%
  slice(1) %>%
  select(rowname, ATP_I.mean, ATP_I.sd) %>%
  data.frame() %>%
  rename("Variables" = "rowname") %>%
  mutate(Factors = case_when(
    Variables %in% c(
      "Island",
      "MeanTemperature",
      "NDVI",
      "logDistToOcean",
      "logAnnualPrecipitation",
      "logAltitude",
      "logFreshWater"
    ) ~ "Environmental",
    TRUE ~ "Socioeconomic"
  )) %>%
  mutate(Variables = recode(Variables, "logNaturalRessources" = "Natural Res.")) %>%
  mutate(Variables = recode(Variables, "MeanTemperature" = "Temp.")) %>%
  mutate(Variables = recode(Variables, "loghf" = "H. Footprint")) %>%
  mutate(Variables = recode(Variables, "logDistToOcean" = "Dist to Ocean")) %>%
  mutate(Variables = recode(Variables, "logAnnualPrecipitation" = "Precipitation")) %>%
  mutate(Variables = recode(Variables, "logHDI" = "HDI")) %>%
  mutate(Variables = recode(Variables, "logNGO" = "NGO")) %>%
  mutate(Variables = recode(Variables, "logAltitude" = "Altitude")) %>%
  mutate(Variables = recode(Variables, "logGDP" = "GDP")) %>%
  mutate(Variables = recode(Variables, "logFreshWater" = "Freshwater")) %>%
  mutate(Variables = recode(Variables, "logtt" = "Access.")) %>%
  mutate(Variables = recode(Variables, "logconflicts" = "Conflicts")) %>%
  arrange(Factors)


data2 = merge(data2.A, data2.B[, c(1, 2, 3)], by = "Variables", sort = FALSE)



# Set a number of 'empty bar' to add at the end of each group
empty_bar = 0
to_add = data.frame(matrix(NA, empty_bar, ncol(data)))
colnames(to_add) = colnames(data)
to_add$Factors = rep("Human Footprint", each = empty_bar)
data = rbind(data, to_add)
data$id = c(4, 3, 1, 2, 6, 5, 7, 8, 9, 10, 14, 13, 11, 12)

# Set a number of 'empty bar' to add at the end of each group
empty_bar = 0
to_add = data.frame(matrix(NA, empty_bar, ncol(data2)))
colnames(to_add) = colnames(data2)
to_add$Factors = rep("Human Footprint", each = empty_bar)
data2 = rbind(data2, to_add)
data2$id = c(4, 1, 6, 2, 5, 7, 3, 9, 10, 14, 8, 13, 11, 12)


base_data = data %>%
  group_by(Factors) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

base_data2 = data2 %>%
  group_by(Factors) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

grid_data = base_data
grid_data$end = grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start = grid_data$start - 1
grid_data = grid_data[-1, ]

# prepare a data frame for grid (scales)
grid_data2 = base_data2
grid_data2$end = grid_data2$end[c(nrow(grid_data2), 1:nrow(grid_data2) -
                                    1)] + 1
grid_data2$start = grid_data2$start - 1
grid_data2 = grid_data2[-1, ]

segment_data = matrix(NA, 21, 4)
segment_data  = as.data.frame(segment_data)
colnames(segment_data) = c("xstart", "ystart", "xend", "yend")
segment_data[, 1] = c(rep(0.5, 6), seq(0.5, 14.5, 1))
segment_data[, 2] = c(seq(0, 1, 0.2), rep(0, 15))
segment_data[, 3] = c(rep(14.5, 6), seq(0.5, 14.5, 1))
segment_data[, 4] = c(seq(0, 1, 0.2), rep(1, 15))

segment_data2 = matrix(NA, 21, 4)
segment_data2  = as.data.frame(segment_data2)
colnames(segment_data2) = c("xstart", "ystart", "xend", "yend")
segment_data2[, 1] = c(rep(0.5, 6), seq(0.5, 14.5, 1))
segment_data2[, 2] = c(seq(0, 1, 0.2), rep(0, 15))
segment_data2[, 3] = c(rep(14.5, 6), seq(0.5, 14.5, 1))
segment_data2[, 4] = c(seq(0, 1, 0.2), rep(1, 15))


# Get the name and the y position of each label
label_data = data
number_of_bar = nrow(label_data)
angle = 90 - 360 * (label_data$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# Get the name and the y position of each label
label_data2 = data2
number_of_bar = nrow(label_data2)
angle = 90 - 360 * (label_data2$id - 0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data2$hjust <- ifelse(angle < -90, 1, 0)
label_data2$angle <- ifelse(angle < -90, angle + 180, angle)


cols = c("#008B8B", "#ee8761")

#####################################################
p = ggplot(data, aes(x = as.factor(id), y = AMP_All.mean, fill = Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x = as.factor(id), y = AMP_All.mean, fill = Factors),
           stat = "identity",
           alpha = 0)  +
  scale_fill_manual(values = cols) +
  
  geom_segment(
    data = segment_data,
    aes(
      x = xstart,
      y = ystart,
      xend = xend,
      yend = yend
    ),
    colour = "grey",
    alpha = 0.7,
    size = 0.09,
    inherit.aes = FALSE
  ) +
  geom_bar(aes(x = as.factor(id), y = AMP_All.mean, fill = Factors),
           stat = "identity",
           alpha = 0.6) +
  geom_linerange(aes(ymin = AMP_All.mean - AMP_All.sd, ymax = AMP_All.mean +
                       AMP_All.sd),
                 colour = "gray47") +
  
  # Add text 
  annotate(
    "text",
    x = rep(15, 6),
    y = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    label = c("0", "0.2", "0.4", "0.6", "0.8", "1") ,
    color = "grey",
    size = 2.5 ,
    angle = 0,
    fontface = "bold",
    hjust = 1
  ) +
  ylim(-0.5, 1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_polar() +
  geom_text(
    data = label_data,
    aes(
      x = id,
      y = AMP_All.mean + 0.04,
      label = Variables,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 3.5,
    angle = label_data$angle,
    inherit.aes = FALSE
  ) +
  
  # Add base line information
  geom_segment(
    data = base_data,
    aes(
      x = start - 0.1,
      y = -0.05,
      xend = end + 0.1,
      yend = -0.05
    ),
    colour = cols,
    alpha = 0.5,
    size = 0.7 ,
    inherit.aes = FALSE,
    
  )  +
  theme(plot.title = element_text(vjust = -40)) +
  theme(
    legend.text = element_text(
      colour = "grey",
      size = 10,
      face = "bold"
    ),
    legend.position = "none"
  )#+


#####################################################
p1 = ggplot(data, aes(x = as.factor(id), y = AMP_I.mean, fill = Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x = as.factor(id), y = AMP_I.mean, fill = Factors),
           stat = "identity",
           alpha = 0) +
  scale_fill_manual(values = cols) +
  
  geom_segment(
    data = segment_data,
    aes(
      x = xstart,
      y = ystart,
      xend = xend,
      yend = yend
    ),
    colour = "grey",
    alpha = 0.7,
    size = 0.09,
    inherit.aes = FALSE
  ) +
  geom_bar(aes(x = as.factor(id), y = AMP_I.mean, fill = Factors),
           stat = "identity",
           alpha = 0.6) +
  geom_linerange(aes(ymin = AMP_I.mean - AMP_I.sd, ymax = AMP_I.mean + AMP_I.sd),
                 col = "gray47") +
  
  # Add text 
  annotate(
    "text",
    x = rep(15, 6),
    y = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    label = c("0", "0.2", "0.4", "0.6", "0.8", "1") ,
    color = "grey",
    size = 2.5 ,
    angle = 0,
    fontface = "bold",
    hjust = 1
  )  +
  ylim(-0.5, 1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_polar() +
  geom_text(
    data = label_data,
    aes(
      x = id,
      y = AMP_I.mean + 0.04,
      label = Variables,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 3.5,
    angle = label_data$angle,
    inherit.aes = FALSE
  ) +
  
  # Add base line information
  geom_segment(
    data = base_data,
    aes(
      x = start - 0.1,
      y = -0.05,
      xend = end + 0.1,
      yend = -0.05
    ),
    colour = cols,
    alpha = 0.5,
    size = 0.7 ,
    inherit.aes = FALSE,
    
  )  +
  theme(plot.title = element_text(vjust = -40)) +
  theme(
    legend.text = element_text(
      colour = "grey",
      size = 10,
      face = "bold"
    ),
    legend.position = "none"
  )#

#####################################################
p2 = ggplot(data2, aes(x = as.factor(id), y = ATP_All.mean, fill = Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x = as.factor(id), y = ATP_All.mean, fill = Factors),
           stat = "identity",
           alpha = 0) +
  scale_fill_manual(values = cols) +
  
  geom_segment(
    data = segment_data2,
    aes(
      x = xstart,
      y = ystart,
      xend = xend,
      yend = yend
    ),
    colour = "grey",
    alpha = 0.5,
    size = 0.09,
    inherit.aes = FALSE
  ) +
  geom_bar(aes(x = as.factor(id), y = ATP_All.mean, fill = Factors),
           stat = "identity",
           alpha = 0.6) +
  geom_linerange(aes(ymin = ATP_All.mean - ATP_All.sd, ymax = ATP_All.mean +
                       ATP_All.sd),
                 col = "gray47") +
  # Add text 
  annotate(
    "text",
    x = rep(15, 6),
    y = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    label = c("0", "0.2", "0.4", "0.6", "0.8", "1") ,
    color = "grey",
    size = 2.5 ,
    angle = 0,
    fontface = "bold",
    hjust = 1
  ) +
  
  ylim(-0.5, 1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_polar() +
  geom_text(
    data = label_data2,
    aes(
      x = id,
      y = ATP_All.mean + 0.04,
      label = Variables,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 3.5,
    angle = label_data2$angle,
    inherit.aes = FALSE
  ) +
  
  # Add base line information
  geom_segment(
    data = base_data2,
    aes(
      x = start - 0.1,
      y = -0.05,
      xend = end + 0.1,
      yend = -0.05
    ),
    colour = cols,
    alpha = 0.5,
    size = 0.7 ,
    inherit.aes = FALSE
  )  +
  theme(plot.title = element_text(vjust = -40)) +
  theme(
    legend.text = element_text(
      colour = "grey",
      size = 10,
      face = "bold"
    ),
    legend.position = "none"
  )


###################################################

p3 = ggplot(data2, aes(x = as.factor(id), y = ATP_I.mean, fill = Factors)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x = as.factor(id), y = ATP_I.mean, fill = Factors),
           stat = "identity",
           alpha = 0) +
  scale_fill_manual(values = cols) +
  
  geom_segment(
    data = segment_data2,
    aes(
      x = xstart,
      y = ystart,
      xend = xend,
      yend = yend
    ),
    colour = "grey",
    alpha = 0.5,
    size = 0.09,
    inherit.aes = FALSE
  ) +
  geom_bar(aes(x = as.factor(id), y = ATP_I.mean, fill = Factors),
           stat = "identity",
           alpha = 0.6) +
  geom_linerange(aes(ymin = ATP_I.mean - ATP_I.sd, ymax = ATP_I.mean + ATP_I.sd),
                 col = "gray47") +

  
  # Add text 
  annotate(
    "text",
    x = rep(15, 6),
    y = c(0, 0.2, 0.4, 0.6, 0.8, 1),
    label = c("0", "0.2", "0.4", "0.6", "0.8", "1") ,
    color = "grey",
    size = 2.5 ,
    angle = 0,
    fontface = "bold",
    hjust = 1
  ) +
  
  ylim(-0.5, 1.1) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  ) +
  coord_polar() +
  geom_text(
    data = label_data2,
    aes(
      x = id,
      y = ATP_I.mean + 0.04,
      label = Variables,
      hjust = hjust
    ),
    color = "black",
    fontface = "bold",
    alpha = 0.6,
    size = 3.5,
    angle = label_data2$angle,
    inherit.aes = FALSE
  ) +
  
  # Add base line information
  geom_segment(
    data = base_data2,
    aes(
      x = start - 0.1,
      y = -0.05,
      xend = end + 0.1,
      yend = -0.05
    ),
    colour = cols,
    alpha = 0.5,
    size = 0.7 ,
    inherit.aes = FALSE,
    
  )  +
  theme(plot.title = element_text(vjust = -40)) +
  theme(
    legend.text = element_text(
      colour = "grey",
      size = 10,
      face = "bold"
    ),
    legend.position = "none"
  ) +
  theme(
    legend.text = element_text(
      colour = "gray47",
      size = 10,
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      colour = "gray49",
      size = 12,
      face = "bold"
    )
  ) +
  theme(legend.spacing.y = unit(3, 'lines')) +
  guides(fill = guide_legend(byrow = TRUE))


#####################################################



# 2. Save the legend
#+++++++++++++++++++++++
legend <- get_legend(p3)

# 3. Remove the legend from the box plot
#+++++++++++++++++++++++
p3 <- p3 + theme(legend.position = "none")


databar <- data %>%
  select(c("AMP_All.mean", "AMP_I.mean", "Factors")) %>%
  group_by(Factors) %>%
  summarise_all(mean) %>%
  arrange(desc(Factors))

databar2 <- data2 %>%
  select(c("ATP_All.mean", "ATP_I.mean", "Factors")) %>%
  group_by(Factors) %>%
  summarise_all(mean) %>%
  arrange(desc(Factors))


### Create barplot

g = ggplot() +
  geom_bar(
    data = databar,
    mapping = aes(x = c(1, 2), y = AMP_All.mean, fill = Factors),
    stat = "identity",
    alpha = 0.6
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous("", labels = c("", ""), breaks = c(1, 2)) +
  scale_y_continuous(
    "",
    labels = c(0, 0.2, 0.4, 0.6),
    breaks = c(0, 0.2, 0.4, 0.6),
    limits = c(0, 0.7)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.line.x = element_line(size = 0.25, color = "gray47"),
    axis.line.y = element_line(size = 0.25, color = "gray47"),
    axis.ticks.x = element_line(size = 0.25, color = "gray47"),
    axis.ticks.y = element_line(size = 0.25, color = "gray47"),
    axis.text = element_text(size = 6, color = "gray47")
  )


g1 = ggplot() +
  geom_bar(
    data = databar,
    mapping = aes(x = c(1, 2), y = AMP_I.mean, fill = Factors),
    stat = "identity",
    alpha = 0.6
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous("", labels = c("", ""), breaks = c(1, 2)) +
  scale_y_continuous(
    "",
    labels = c(0, 0.2, 0.4, 0.6),
    breaks = c(0, 0.2, 0.4, 0.6),
    limits = c(0, 0.7)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.line.x = element_line(size = 0.25, color = "gray47"),
    axis.line.y = element_line(size = 0.25, color = "gray47"),
    axis.ticks.x = element_line(size = 0.25, color = "gray47"),
    axis.ticks.y = element_line(size = 0.25, color = "gray47"),
    axis.text = element_text(size = 6, color = "gray47")
  )


g2 = ggplot() +
  geom_bar(
    data = databar2,
    mapping = aes(x = c(1, 2), y = ATP_All.mean, fill = Factors),
    stat = "identity",
    alpha = 0.6
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous("", labels = c("", ""), breaks = c(1, 2)) +
  scale_y_continuous(
    "",
    labels = c(0, 0.2, 0.4, 0.6),
    breaks = c(0, 0.2, 0.4, 0.6),
    limits = c(0, 0.7)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.line.x = element_line(size = 0.25, color = "gray47"),
    axis.line.y = element_line(size = 0.25, color = "gray47"),
    axis.ticks.x = element_line(size = 0.25, color = "gray47"),
    axis.ticks.y = element_line(size = 0.25, color = "gray47"),
    axis.text = element_text(size = 6, color = "gray47")
  )

g3 = ggplot() +
  geom_bar(
    data = databar2,
    mapping = aes(x = c(1, 2), y = ATP_I.mean, fill = Factors),
    stat = "identity",
    alpha = 0.6
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = cols) +
  scale_x_continuous("", labels = c("", ""), breaks = c(1, 2)) +
  scale_y_continuous(
    "",
    labels = c(0, 0.2, 0.4, 0.6),
    breaks = c(0, 0.2, 0.4, 0.6),
    limits = c(0, 0.7)
  ) +
  theme(legend.position = "none") +
  theme(
    axis.line.x = element_line(size = 0.25, color = "gray47"),
    axis.line.y = element_line(size = 0.25, color = "gray47"),
    axis.ticks.x = element_line(size = 0.25, color = "gray47"),
    axis.ticks.y = element_line(size = 0.25, color = "gray47"),
    axis.text = element_text(size = 6, color = "gray47")
  )

### Insert barplot within circular plot
gg = ggdraw() +
  draw_plot(p, 0, 0, 1, 1) +
  draw_plot(g, 0.404, 0.38, 0.19, 0.19) +
  draw_plot_label("b", 0, 1)

gg1 = ggdraw() +
  draw_plot(p1, 0, 0, 1, 1) +
  draw_plot(g1, 0.404, 0.38, 0.19, 0.19) +
  draw_plot_label("d", 0, 1)

gg2 = ggdraw() +
  draw_plot(p2, 0, 0, 1, 1) +
  draw_plot(g2, 0.404, 0.38, 0.19, 0.199) +
  draw_plot_label("a", 0, 1)

gg3 = ggdraw() +
  draw_plot(p3, 0, 0, 1, 1) +
  draw_plot(g3, 0.404, 0.38, 0.19, 0.19) +
  draw_plot_label("c", 0, 1)

t <- grid::textGrob("All IUCN \n (I - VI)")
t1 <- grid::textGrob("IUCN I \n only")
t2 <- grid::textGrob("Terrestrial Protected Areas")
t3 <- grid::textGrob("Marine Protected Areas")
t_empty <- grid::textGrob("")
grid.arrange(
  t_empty,
  t2,
  t3,
  t,
  gg2,
  gg,
  t1,
  gg3,
  gg1,
  t_empty,
  legend,
  ncol = 9,
  nrow = 6,
  layout_matrix = rbind(
    c(1, 2, 2, 2, 2, 3, 3, 3, 3),
    c(4, 5, 5, 5, 5, 6, 6, 6, 6),
    c(4, 5, 5, 5, 5, 6, 6, 6, 6),
    c(4, 5, 5, 5, 5, 6, 6, 6, 6),
    c(7, 8, 8, 8, 8, 9, 9, 9, 9),
    c(7, 8, 8, 8, 8, 9, 9, 9, 9),
    c(7, 8, 8, 8, 8, 9, 9, 9, 9),
    c(10, 11, 11, 11, 11, 11, 11, 11, 11)
  )
)

