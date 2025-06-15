## listing all the final models that were used:
library(tidyverse)
library(lme4)
library(car)
library(MASS)
#load data frames for models
# germinationData <- read_csv("~/Documents/GitHub/Florida/Eryngium Manuscript/cleanData/germinationData.csv")
load("cleanData/germinationData.RData")
load("dataFrames/combinedData.RData")
load("dataFrames/flwrFig1Data.RData")
fig1Data <- read_csv("cleanData/roadData.csv")
scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45)

flwrFig1Data$heads21_scaled <- scale(flwrFig1Data$heads21)
flwrFig1Data$patchDistance_scaled <- scale(flwrFig1Data$patchDistance)
flwrFig1Data$roadDistance_scaled <- scale(flwrFig1Data$roadDistance)
flwrFig1Data$meanElevation_scaled <- scale(flwrFig1Data$meanElevation)

fig1Data$roadDistanceSq <- (fig1Data$roadDistance)*(fig1Data$roadDistance)

#Road Model:
glmeNegBinomial <- glmer.nb(seedAbundance~ roadDistance + patchDistance + +meanElevation_scaled+poly(roadDistance,2)+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,])

#scrub v road model
control_params <- glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 500000))
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

#germination model
germinationModel <- glm(formula = cbind(total.germ, seeds-total.germ)~habitat, family = binomial, data=germinationData)

#Rosemary model (just scrub)
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6))
scrubData$nearFar <- factor(scrubData$nearFar, levels = c("N", "F"))
scrubData$seedAbundance_adjusted <- scrubData$seedAbundance + 1
# nearFarModel <- glmer.nb(seedAbundance ~ nearFar + (1 | bald), offset = log(massActual), 
#                          data = scrubData, control = control, verbose = FALSE)
nearFarModelAdjusted <- glmer.nb(seedAbundance_adjusted~nearFar+(1|bald), offset=log(massActual), data=scrubData, verbose=FALSE)

#flowering heads and seeds
controls <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10e6))
flwrElevationTest <- glmer.nb(seedAbundance ~ heads21_scaled + patchDistance_scaled + meanElevation_scaled+ roadDistance_scaled + (1 | transectNum), data = flwrFig1Data[flwrFig1Data$patchDistance<=130,], control = controls)

#seeds and seedlings
seedlingsTest <- glmer.nb(seedlings22~ seedAbundance + roadDistance+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=150,], control=controls)