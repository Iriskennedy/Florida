library(tidyverse)
library(lme4)
library(car)

#load data frames for models
# germinationData <- read_csv("~/Documents/GitHub/Florida/Eryngium Manuscript/cleanData/germinationData.csv")
load("cleanData/germinationData.RData")
load("dataFrames/combinedData.RData")
load("dataFrames/flwrFig1Data.RData")
fig1Data <- read_csv("cleanData/roadData.csv")
scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45)

fig1Data$roadDistanceSq <- (fig1Data$roadDistance)*(fig1Data$roadDistance)

#MODELS:

#Road Model:
# glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + poly(roadDistance,2) +(1|transectNum), data=fig1Data)

glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + roadDistanceSq +(1|transectNum), data=fig1Data) #altered polynomial tern so it wouldn't give NAs in summary

# Rescale predictor variables
flwrFig1Data$heads21_scaled <- scale(flwrFig1Data$heads21)
flwrFig1Data$patchDistance_scaled <- scale(flwrFig1Data$patchDistance)
flwrFig1Data$roadDistance_scaled <- scale(flwrFig1Data$roadDistance)
flwrFig1Data$meanElevation_scaled <- scale(flwrFig1Data$meanElevation)

test2 <- glmer.nb(seedAbundance~ roadDistance + patchDistance +meanElevation_scaled+poly(roadDistance,2)+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,]) #has been selected to have the lowest AIC
summary(test2)

#note, with one variable: suppressor effect, "It might not explain a significant portion of the variability in seedAbundance alone, resulting in a non-significant effect in test3." X1 has a weak correlation with Y when considered alone.
# X2 might account for some noise or irrelevant variance.
# When X2 is included in the model, it suppresses this noise, allowing the true relationship between X1 and Y to emerge, making X1 significant.
test3 <- glmer.nb(seedAbundance~ meanElevation_scaled+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,])
summary(test3)

#Scrub v Road model
control_params <- glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 500000))
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

#germination model
germinationModel <- glm(formula = cbind(total.germ, seeds-total.germ)~habitat, family = binomial, data=germinationData)

#Rosemary model (just scrub)
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6))

nearFarModel <- glmer.nb(seedAbundance ~ nearFar + (1 | bald), offset = log(massActual), 
                         data = scrubData, control = control, verbose = FALSE)