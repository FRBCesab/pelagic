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

logAnnualPrecipitation <-
  log10(IUCN.I.VI.ATP$AnnualPrecipitation + 1)

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
            ATP_All$IUCN_CAT == "Ib" | ATP_All$IUCN_CAT == "NONE",]

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
            AMP_All$IUCN_CAT == "Ib" | AMP_All$IUCN_CAT == "NONE",]

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
# HDI
####################

par.mod1_AMP_HDI <-
  pdp::partial(modAMP,
               pred.var = "HDI",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_HDI <-
  pdp::partial(modAMPI,
               pred.var = "HDI",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_HDI <-
  pdp::partial(modATP,
               pred.var = "HDI",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_HDI <-
  pdp::partial(modATPI,
               pred.var = "HDI",
               prob = TRUE,
               which.class = "TRUE")


pHDI <-
  ggplot() + geom_line(data = par.mod1_AMP_HDI,
                       mapping = aes(x = HDI, y = yhat),
                       alpha = 0) + geom_smooth(
                         data = par.mod1_AMP_HDI,
                         mapping = aes(x = HDI, y = yhat),
                         span = 0.4,
                         color = '#034e7b',
                         alpha = 0.8,
                         se = FALSE
                       ) +
  geom_line(data = par.mod1_AMPI_HDI,
            mapping = aes(x = HDI, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_AMPI_HDI,
              mapping = aes(x = HDI, y = yhat),
              span = 0.4,
              color = '#74a9cf',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  geom_line(data = par.mod1_ATP_HDI,
            mapping = aes(x = HDI, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_HDI,
              mapping = aes(x = HDI, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_HDI,
            mapping = aes(x = HDI, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_HDI,
              mapping = aes(x = HDI, y = yhat),
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
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "HDI")

#####################
# HF
####################
par.mod1_AMP_hf <-
  pdp::partial(modAMP,
               pred.var = "loghf",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_hf <-
  pdp::partial(modAMPI,
               pred.var = "loghf",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_hf <-
  pdp::partial(modATP,
               pred.var = "logHF",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_hf <-
  pdp::partial(modATPI,
               pred.var = "logHF",
               prob = TRUE,
               which.class = "TRUE")


pHF <-
  ggplot() + geom_line(data = par.mod1_AMP_hf,
                       mapping = aes(x = loghf, y = yhat),
                       alpha = 0) + geom_smooth(
                         data = par.mod1_AMP_hf,
                         mapping = aes(x = loghf, y = yhat),
                         span = 0.4,
                         color = '#034e7b',
                         alpha = 0.8,
                         se = FALSE
                       ) +
  geom_line(data = par.mod1_AMPI_hf,
            mapping = aes(x = loghf, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_AMPI_hf,
              mapping = aes(x = loghf, y = yhat),
              span = 0.4,
              color = '#74a9cf',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  geom_line(data = par.mod1_ATP_hf,
            mapping = aes(x = logHF, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_hf,
              mapping = aes(x = logHF, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_hf,
            mapping = aes(x = logHF, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_hf,
              mapping = aes(x = logHF, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Human footprint (log)")

#####################
# Conflicts
####################
par.mod1_AMP_conflits <-
  pdp::partial(modAMP,
               pred.var = "logconflicts",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_conflits <-
  pdp::partial(modAMPI,
               pred.var = "logconflicts",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_conflits <-
  pdp::partial(modATP,
               pred.var = "logconflicts",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_conflits <-
  pdp::partial(modATPI,
               pred.var = "logconflicts",
               prob = TRUE,
               which.class = "TRUE")



pConflicts <-
  ggplot() + geom_line(
    data = par.mod1_AMP_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  geom_line(
    data = par.mod1_ATP_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_conflits,
    mapping = aes(x = logconflicts, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_conflits,
    mapping = aes(x = logconflicts, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Conflicts (log number)")


#####################
# Elevation and Depth
####################
par.mod1_AMP_bath <-
  pdp::partial(modAMP,
               pred.var = "logdepth",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_bath <-
  pdp::partial(modAMPI,
               pred.var = "logdepth",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_bath <-
  pdp::partial(modATP,
               pred.var = "logAltitude",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_bath <-
  pdp::partial(modATPI,
               pred.var = "logAltitude",
               prob = TRUE,
               which.class = "TRUE")


pBath <-
  ggplot() + geom_line(
    data = par.mod1_AMP_bath,
    mapping = aes(x = logdepth, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_bath,
    mapping = aes(x = logdepth, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_bath,
    mapping = aes(x = logdepth, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_bath,
    mapping = aes(x = logdepth, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  geom_line(
    data = par.mod1_ATP_bath,
    mapping = aes(x = logAltitude, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_bath,
    mapping = aes(x = logAltitude, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_bath,
    mapping = aes(x = logAltitude, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_bath,
    mapping = aes(x = logAltitude, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Elevation or Depth (log m)")


#####################
# Distance to coast
####################

par.mod1_AMP_dist <-
  pdp::partial(modAMP,
               pred.var = "logDistToCoast",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_dist <-
  pdp::partial(modAMPI,
               pred.var = "logDistToCoast",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_dist <-
  pdp::partial(modATP,
               pred.var = "logDistToOcean",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_dist <-
  pdp::partial(modATPI,
               pred.var = "logDistToOcean",
               prob = TRUE,
               which.class = "TRUE")

#Distance to the coastline and Distance to the mainland
pDist <-
  ggplot() + geom_line(
    data = par.mod1_AMP_dist,
    mapping = aes(x = logDistToCoast, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_dist,
    mapping = aes(x = logDistToCoast, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_dist,
    mapping = aes(x = logDistToCoast, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_dist,
    mapping = aes(x = logDistToCoast, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  geom_line(
    data = par.mod1_ATP_dist,
    mapping = aes(x = logDistToOcean, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_dist,
    mapping = aes(x = logDistToOcean, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_dist,
    mapping = aes(x = logDistToOcean, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_dist,
    mapping = aes(x = logDistToOcean, y = yhat),
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
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "Distance to the coast (log km)")



#####################
# Temperature
####################

par.mod1_AMP <-
  pdp::partial(modAMP,
               pred.var = "SST",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI <-
  pdp::partial(modAMPI,
               pred.var = "SST",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP <-
  pdp::partial(modATP,
               pred.var = "MeanTemperature",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI <-
  pdp::partial(modATPI,
               pred.var = "MeanTemperature",
               prob = TRUE,
               which.class = "TRUE")


pTemp <-
  ggplot() + geom_line(data = par.mod1_AMP,
                       mapping = aes(x = SST, y = yhat),
                       alpha = 0) + geom_smooth(
                         data = par.mod1_AMP,
                         mapping = aes(x = SST, y = yhat),
                         span = 0.4,
                         color = '#034e7b',
                         alpha = 0.8,
                         se = FALSE
                       ) +
  geom_line(data = par.mod1_AMPI,
            mapping = aes(x = SST, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_AMPI,
              mapping = aes(x = SST, y = yhat),
              span = 0.4,
              color = '#74a9cf',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  geom_line(
    data = par.mod1_ATP,
    mapping = aes(x = MeanTemperature, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP,
    mapping = aes(x = MeanTemperature, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI,
    mapping = aes(x = MeanTemperature, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI,
    mapping = aes(x = MeanTemperature, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Annual temperature")


#####################
# Travel time
####################

par.mod1_AMP_tt <-
  pdp::partial(modAMP,
               pred.var = "logtravel_time",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_tt <-
  pdp::partial(modAMPI,
               pred.var = "logtravel_time",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_tt <-
  pdp::partial(modATP,
               pred.var = "logtt",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_tt <-
  pdp::partial(modATPI,
               pred.var = "logtt",
               prob = TRUE,
               which.class = "TRUE")


pPressure <-
  ggplot() + geom_line(
    data = par.mod1_AMP_tt,
    mapping = aes(x = logtravel_time, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_tt,
    mapping = aes(x = logtravel_time, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_tt,
    mapping = aes(x = logtravel_time, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_tt,
    mapping = aes(x = logtravel_time, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  geom_line(data = par.mod1_ATP_tt,
            mapping = aes(x = logtt, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_tt,
              mapping = aes(x = logtt, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_tt,
            mapping = aes(x = logtt, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_tt,
              mapping = aes(x = logtt, y = yhat),
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
  scale_y_continuous(name = "Protection probability") + scale_x_continuous(name = "Travel time (log minutes)")


#####################
# NGO
####################
par.mod1_AMP_NGO <-
  pdp::partial(modAMP,
               pred.var = "logNGO",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_NGO <-
  pdp::partial(modAMPI,
               pred.var = "logNGO",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_NGO <-
  pdp::partial(modATP,
               pred.var = "logNGO",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_NGO <-
  pdp::partial(modATPI,
               pred.var = "logNGO",
               prob = TRUE,
               which.class = "TRUE")


pNGO <-
  ggplot() + geom_line(data = par.mod1_AMP_NGO,
                       mapping = aes(x = logNGO, y = yhat),
                       alpha = 0) + geom_smooth(
                         data = par.mod1_AMP_NGO,
                         mapping = aes(x = logNGO, y = yhat),
                         span = 0.4,
                         color = '#034e7b',
                         alpha = 0.8,
                         se = FALSE
                       ) +
  geom_line(data = par.mod1_AMPI_NGO,
            mapping = aes(x = logNGO, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_AMPI_NGO,
              mapping = aes(x = logNGO, y = yhat),
              span = 0.4,
              color = '#74a9cf',
              alpha = 0.8,
              se = FALSE,
              linetype = "twodash"
            ) +
  geom_line(data = par.mod1_ATP_NGO,
            mapping = aes(x = logNGO, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATP_NGO,
              mapping = aes(x = logNGO, y = yhat),
              span = 0.4,
              color = '#8c2d04',
              alpha = 0.8,
              se = FALSE
            ) +
  geom_line(data = par.mod1_ATPI_NGO,
            mapping = aes(x = logNGO, y = yhat),
            alpha = 0) + geom_smooth(
              data = par.mod1_ATPI_NGO,
              mapping = aes(x = logNGO, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Non-governmental organizations (log number)")


#####################
# Resources dependency
####################

par.mod1_AMP_dep <-
  pdp::partial(modAMP,
               pred.var = "MarineEcosystemDependency",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_AMPI_dep <-
  pdp::partial(modAMPI,
               pred.var = "MarineEcosystemDependency",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATP_dep <-
  pdp::partial(modATP,
               pred.var = "logNaturalResources",
               prob = TRUE,
               which.class = "TRUE")
par.mod1_ATPI_dep <-
  pdp::partial(modATPI,
               pred.var = "logNaturalResources",
               prob = TRUE,
               which.class = "TRUE")


pResource <-
  ggplot() + geom_line(
    data = par.mod1_AMP_dep,
    mapping = aes(x = MarineEcosystemDependency, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMP_dep,
    mapping = aes(x = MarineEcosystemDependency, y = yhat),
    span = 0.4,
    color = '#034e7b',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_AMPI_dep,
    mapping = aes(x = MarineEcosystemDependency, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_AMPI_dep,
    mapping = aes(x = MarineEcosystemDependency, y = yhat),
    span = 0.4,
    color = '#74a9cf',
    alpha = 0.8,
    se = FALSE,
    linetype = "twodash"
  ) +
  geom_line(
    data = par.mod1_ATP_dep,
    mapping = aes(x = logNaturalResources, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATP_dep,
    mapping = aes(x = logNaturalResources, y = yhat),
    span = 0.4,
    color = '#8c2d04',
    alpha = 0.8,
    se = FALSE
  ) +
  geom_line(
    data = par.mod1_ATPI_dep,
    mapping = aes(x = logNaturalResources, y = yhat),
    alpha = 0
  ) + geom_smooth(
    data = par.mod1_ATPI_dep,
    mapping = aes(x = logNaturalResources, y = yhat),
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
  scale_y_continuous(name = " ") + scale_x_continuous(name = "Resources dependency (log)")


#####################
# plot
####################
partial_plot_all <-
  cowplot::plot_grid(
    pHDI,
    pConflicts,
    pHF,
    pPressure,
    pNGO,
    pResource,
    pDist,
    pBath,
    pTemp,
    ncol = 3,
    labels = c("a", "b", "c", "d", "e", "f", "g", "h", "i")
  )
