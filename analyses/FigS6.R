library(ranger)
library(ggplot2)
library(pdp)
library(cowplot)


###############################
# Data and models
###############################

#####################
#TPA
####################

load("data/IUCN.I.VI.ATP.RData")


Reserve <- c(rep(TRUE, 258725), rep(FALSE, 258725))

IUCN.I.VI.ATP <- cbind(IUCN.I.VI.ATP, Reserve)

IUCN.I.VI.ATP$Island <- as.factor(IUCN.I.VI.ATP$Island)

IUCN.I.VI.ATP$Reserve <- as.factor(IUCN.I.VI.ATP$Reserve)


# log transform


logNGO <- log10(IUCN.I.VI.ATP$NGO + 1)

logconflicts <- log10(IUCN.I.VI.ATP$conflicts + 1)

logDistToOcean <- log10(IUCN.I.VI.ATP$DistToOcean + 1)

logtt <- log10(IUCN.I.VI.ATP$tt + 1)

logHF <- log10(IUCN.I.VI.ATP$hf + 1)

logAltitude <- log10(IUCN.I.VI.ATP$Altitude + 1)

logNaturalResources <- log10(IUCN.I.VI.ATP$NaturalResources + 1)

logAnnualPrecipitation <- log10(IUCN.I.VI.ATP$AnnualPrecipitation + 1)

logGDP <- log10(IUCN.I.VI.ATP$GDP + 1)


ATP_All <-
  cbind(
    IUCN.I.VI.ATP,
    logNGO,
    logconflicts,
    logDistToOcean,
    logtt,
    logHF,
    logAltitude,
    logNaturalResources,
    logAnnualPrecipitation,
    logGDP
  )


modATP <-
  ranger(
    as.factor(Reserve) ~ logNGO + logconflicts + logDistToOcean +
      logtt + logHF + logAltitude + FreshWater + logAnnualPrecipitation + logGDP +
      HDI +  logNaturalResources + Island + MeanTemperature + NDVI,
    data = ATP_All,
    keep.inbag = TRUE,
    probability = TRUE
  )


#####################
#TPA IUCN only
####################

ATP_I <-
  ATP_All[ATP_All$IUCN_CAT == "Ia" |
            ATP_All$IUCN_CAT == "Ib" | ATP_All$IUCN_CAT == "NONE", ]

modATPI <-
  ranger(
    as.factor(Reserve) ~ logNGO + logconflicts + logDistToOcean +
      logtt + logHF + logAltitude + FreshWater + logAnnualPrecipitation + logGDP +
      HDI +  logNaturalResources + Island + MeanTemperature + NDVI,
    data = ATP_I,
    keep.inbag = TRUE,
    probability = TRUE
  )


#####################
#MPA
####################


load("data/IUCN.I.VI.AMP.RData")

IUCN.I.VI.AMP$Island <- as.factor(IUCN.I.VI.AMP$Island)

IUCN.I.VI.AMP$Reserve <- as.factor(IUCN.I.VI.AMP$Reserve)

summary(IUCN.I.VI.AMP)


# log transform


logNGO <- log10(IUCN.I.VI.AMP$NGO + 1)

logconflicts <- log10(IUCN.I.VI.AMP$conflicts + 1)

logDistToCoast <- log10(IUCN.I.VI.AMP$DistToCoast + 1)

logtravel_time <- log10(IUCN.I.VI.AMP$travel_time + 1)

logDistToSeamounts <- log10(IUCN.I.VI.AMP$DistToSeamounts + 1)

logChla <- log10(IUCN.I.VI.AMP$Chla + 1)

loghf <- log10((IUCN.I.VI.AMP$hf * 5) + 1)

loggdp <- log10(IUCN.I.VI.AMP$gdp + 1)

logdepth <- log10(-IUCN.I.VI.AMP$Bathymetry + 1)

AMP_All <-
  cbind(
    IUCN.I.VI.AMP,
    logNGO,
    logconflicts,
    logDistToCoast,
    logtravel_time,
    logDistToSeamounts,
    logChla,
    loghf,
    loggdp,
    logdepth
  )

modAMP <-
  ranger(
    as.factor(Reserve) ~ loggdp + loghf + logconflicts + MarineEcosystemDependency +
      logChla +
      SST + logtravel_time + HDI + logDistToSeamounts + logdepth +
      salinity +
      logDistToCoast + Island + logNGO,
    data = AMP_All,
    keep.inbag = TRUE,
    probability = TRUE
  )

#####################
#MPA IUCN only
####################

AMP_I <-
  AMP_All[AMP_All$IUCN_CAT == "Ia" |
            AMP_All$IUCN_CAT == "Ib" | AMP_All$IUCN_CAT == "NONE", ]

modAMPI <-
  ranger(
    as.factor(Reserve) ~ loggdp + loghf + logconflicts + MarineEcosystemDependency +
      logChla +
      SST + logtravel_time + HDI + logDistToSeamounts + logdepth +
      salinity +
      logDistToCoast + Island + logNGO,
    data = AMP_I,
    keep.inbag = TRUE,
    probability = TRUE
  )




###############################
# Partial plot
###############################

#####################
#GDP
####################

par.mod1_AMP_GDP <-
  pdp::partial(modAMP,
               pred.var = "loggdp",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_GDP <-
  pdp::partial(modAMPI,
               pred.var = "loggdp",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_GDP <-
  pdp::partial(modATP,
               pred.var = "logGDP",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_GDP <-
  pdp::partial(modATPI,
               pred.var = "logGDP",
               prob = TRUE,
               which.class = "TRUE")


pGDP_m <-
  ggplot() + geom_line(data = par.mod1_AMP_GDP,
                       mapping = aes(x = loggdp, y = yhat),
                       alpha = 0) + geom_smooth(
                         data = par.mod1_AMP_GDP,
                         mapping = aes(x = loggdp, y = yhat),
                         span = 0.4,
                         color = '#034e7b',
                         alpha = 0.8,
                         se = FALSE
                       ) +
  geom_line(data = par.mod1_AMPI_GDP,
            mapping = aes(x = loggdp, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_AMPI_GDP,
              mapping = aes(x = loggdp, y = yhat),
              span = 0.4,
              color = '#74a9cf',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "GDP")

pGDP_t <-
  ggplot() + 
  geom_line(data = par.mod1_ATP_GDP,
            mapping = aes(x = logGDP, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_GDP,
              mapping = aes(x = logGDP, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_GDP,
            mapping = aes(x = logGDP, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_GDP,
              mapping = aes(x = logGDP, y = yhat),
              span = 0.4,
              color = '#fec44f',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "") + scale_x_continuous(name = "GDP")

#####################
#chloro
####################

par.mod1_AMP_chloro <-
  pdp::partial(modAMP,
               pred.var = "logChla",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_chloro <-
  pdp::partial(modAMPI,
               pred.var = "logChla",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_chloro <-
  pdp::partial(modATP,
               pred.var = "NDVI",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_chloro <-
  pdp::partial(modATPI,
               pred.var = "NDVI",
               prob = TRUE,
               which.class = "TRUE")


pChloro_m <-
  ggplot() + geom_line(
    data = par.mod1_AMP_chloro,
    mapping = aes(x = logChla, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_chloro,
    mapping = aes(x = logChla, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_chloro,
    mapping = aes(x = logChla, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_chloro,
    mapping = aes(x = logChla, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "Chlorophyll-a")

pChloro_t <-
  ggplot() + 
  geom_line(data = par.mod1_ATP_chloro,
            mapping = aes(x = NDVI, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_chloro,
              mapping = aes(x = NDVI, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_chloro,
            mapping = aes(x = NDVI, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_chloro,
              mapping = aes(x = NDVI, y = yhat),
              span = 0.4,
              color = '#fec44f',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "") + scale_x_continuous(name = "NDVI")


#####################
#precipitation / salinity
####################

par.mod1_AMP_sal <-
  pdp::partial(modAMP,
               pred.var = "salinity",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_sal <-
  pdp::partial(modAMPI,
               pred.var = "salinity",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_prec <-
  pdp::partial(modATP,
               pred.var = "logAnnualPrecipitation",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_prec <-
  pdp::partial(modATPI,
               pred.var = "logAnnualPrecipitation",
               prob = TRUE,
               which.class = "TRUE")


pSal_m <-
  ggplot() + geom_line(
    data = par.mod1_AMP_sal,
    mapping = aes(x = salinity, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_sal,
    mapping = aes(x = salinity, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_sal,
    mapping = aes(x = salinity, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_sal,
    mapping = aes(x = salinity, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "Salinity")

pPrec_t <-
  ggplot() + 
  geom_line(
    data = par.mod1_ATP_prec,
    mapping = aes(x = logAnnualPrecipitation, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_prec,
    mapping = aes(x = logAnnualPrecipitation, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_prec,
    mapping = aes(x = logAnnualPrecipitation, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_prec,
    mapping = aes(x = logAnnualPrecipitation, y = yhat),
    span = 0.4,
    color = '#fec44f',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "") + scale_x_continuous(name = "Precipitation (log mm)")


#####################
#freshwater / seamounts
####################

par.mod1_AMP_seamounts <-
  pdp::partial(modAMP,
               pred.var = "logDistToSeamounts",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_seamounts <-
  pdp::partial(modAMPI,
               pred.var = "logDistToSeamounts",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_fwater <-
  pdp::partial(modATP,
               pred.var = "FreshWater",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_fwater <-
  pdp::partial(modATPI,
               pred.var = "FreshWater",
               prob = TRUE,
               which.class = "TRUE")


pSeamounts_m <-
  ggplot() + geom_line(
    data = par.mod1_AMP_seamounts,
    mapping = aes(x = logDistToSeamounts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_seamounts,
    mapping = aes(x = logDistToSeamounts, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_seamounts,
    mapping = aes(x = logDistToSeamounts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_seamounts,
    mapping = aes(x = logDistToSeamounts, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "Distance to Seamounts (log m)")

pFwater_t <-
  ggplot() + 
  geom_line(
    data = par.mod1_ATP_fwater,
    mapping = aes(x = FreshWater, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_fwater,
    mapping = aes(x = FreshWater, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_fwater,
    mapping = aes(x = FreshWater, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_fwater,
    mapping = aes(x = FreshWater, y = yhat),
    span = 0.4,
    color = '#fec44f',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "") + scale_x_continuous(name = "Fresh water coverage (%)")

#####################
#island
####################

par.mod1_AMP_Island <-
  pdp::partial(modAMP,
               pred.var = "Island",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_Island <-
  pdp::partial(modAMPI,
               pred.var = "Island",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_Island <-
  pdp::partial(modATP,
               pred.var = "Island",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_Island <-
  pdp::partial(modATPI,
               pred.var = "Island",
               prob = TRUE,
               which.class = "TRUE")


pIsland_m <-
  ggplot() + geom_point(
    data = par.mod1_AMP_Island,
    mapping = aes(x = Island, y = yhat),
    color = '#034e7b',
    alpha = 0.8 ,
    shape = 18,
    size = 5
  ) + 
  geom_point(
    data = par.mod1_AMPI_Island,
    mapping = aes(x = Island, y = yhat),
    alpha = 0.8,
    color = '#74a9cf',
    shape = 18 ,
    size = 5
  ) + 
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "Protection probability") + scale_x_discrete(name = "Island")

pIsland_t <-
  ggplot() + 
  geom_point(
    data = par.mod1_ATP_Island,
    mapping = aes(x = Island, y = yhat),
    alpha = 0.8,
    color = "#8c2d04",
    size = 5,
    shape = 18
  ) + 
  geom_point(
    data = par.mod1_ATPI_Island,
    mapping = aes(x = Island, y = yhat),
    alpha = 0.8,
    color = "#fec44f",
    size = 5,
    shape = 18
  ) + 
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(name = "") + scale_x_discrete(name = "Island")

#####################
#Plot
####################
partial_plot_append <-
  cowplot::plot_grid(
    pGDP_m,
    pGDP_t,
    pChloro_m,
    pChloro_t,
    pSal_m,
    pPrec_t,
    pSeamounts_m,
    pFwater_t,
    pIsland_m,
    pIsland_t,
    ncol = 2,
    labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
  )
