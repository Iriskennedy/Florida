#elevation gradient models
library(lme4)
library(car)
library(tidyverse)
library(readxl)
elevationData <- read_excel("Raw Data/Er elevation data for plots.xlsx")#subsetting so it is just the mean elevation column
elevationData<- elevationData%>%
dplyr::  select(`Mean Elevation`, quand.num)%>%
  rename(meanElevation= "Mean Elevation", patchDistance=quand.num)

# there is a significant effect as we find fewer seeds further down in elevation. We could investigate this further in the demographic analyses.
# 
# For example, the analyses could look like this:
# seed abundance ~ flowering heads * patch + flowering heads * road
# seedlings ~ seeds * patch + seeds * road
# 
# The interaction terms, particularly the ones with patch (i.e. elevation) get at the question of whether seed banking differs along this gradient. A significant flowering heads x patch interaction could mean that further down in elevation seeds are being produced, but not necessarily banking (could be rotting, for example). Or, perhaps the seeds are there, but the seedlings aren't able to germinate in those lower elevations. Non-significant interactions would mean that it is a straightforward relationship betwen flowering heads and seed abundance, or seeds and seedlings.

load("dataFrames/flwrFig1Data.RData")

#merging in elevation data
elevationData$patchDistance <- as.numeric(elevationData$patchDistance)
flwrFig1Data <- merge(flwrFig1Data, elevationData, by.x = "patchDistance", by.y = "patchDistance", all.x = TRUE)
flwrElevationTest <- glmer.nb(seedAbundance~heads21*meanElevation + heads21*roadDistance+(1|transectNum), data=mergedData)

save(flwrFig1Data, file = "cleanData/flwrFig1Data.RData")

flwrElevation <- glmer.nb(seedAbundance~heads21*patchDistance + heads21*roadDistance+(1|transectNum), data=flwrFig1Data)

# Rescale predictor variables
flwrFig1Data$heads21_scaled <- scale(flwrFig1Data$heads21)
flwrFig1Data$patchDistance_scaled <- scale(flwrFig1Data$patchDistance)
flwrFig1Data$roadDistance_scaled <- scale(flwrFig1Data$roadDistance)
flwrFig1Data$meanElevation_scaled <- scale(flwrFig1Data$meanElevation)

# Control settings with increased iterations and different optimizer
controls <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10e6))

# Fit the model with rescaled variables and custom control settings
flwrElevation <- glmer.nb(seedAbundance ~ heads21_scaled + patchDistance_scaled  + roadDistance_scaled + (1 | transectNum), 
                          data = flwrFig1Data, control = controls)

summary(flwrElevation)
flwrModel2 <- glmer.nb(seedAbundance ~ heads21 + patchDistance + roadDistance + (1 | uid), data = flwrFig1Data, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(flwrModel2)
#troubleshoot
#because theres so many zeroes at transect 30 and up, consider cutting off at transect 30 ish for purpose of analysis
m2<-  glmer.nb(seedAbundance ~ heads21_scaled+ meanElevation_scaled+ roadDistance_scaled+ meanElevation_scaled*heads21_scaled+ heads21_scaled*roadDistance_scaled+(1 | transectNum), data = flwrFig1Data[flwrFig1Data$transectNum<=30,], control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))

summary(m2)
Anova(m2)
#AIC 372
m3<- glmer.nb(seedAbundance ~ heads21_scaled+(1 | transectNum), data = flwrFig1Data[flwrFig1Data$transectNum<=30,], control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))

summary(m3)
Anova(m3)

m4<- glmer.nb(seedAbundance ~ heads21+(1 | transectNum), data = flwrFig1Data[flwrFig1Data$transectNum<=30,], control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1000000)))

## heads model with everything
#AIC 340
flwrElevationTest <- glmer.nb(seedAbundance ~ heads21_scaled + patchDistance_scaled + meanElevation_scaled+ roadDistance_scaled + (1 | transectNum), 
                              data = flwrFig1Data[flwrFig1Data$patchDistance<=130,], control = controls)

summary(flwrElevationTest)

#flowering heads SIMPLE
flwrElevationTest <- glmer.nb(seedAbundance ~ heads21_scaled + (1 | transectNum), 
                              data = flwrFig1Data[flwrFig1Data$patchDistance<=130,], control = controls)
summary(flwrElevationTest)

#seedlings and seeds
lgSeedlings <- lmer(log(seedlings22+1)~ scale(patchDistance) + poly(roadDistance,2) + poly(seedAbundance, 2)+ log(heads21+1)+ (1|transectNum), data = flwrFig1Data)

seedlingsTest <- glmer.nb(seedlings22~ seedAbundance + roadDistance+ patchDistance_scaled+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=150,], control=controls)
summary(seedlingsTest) #AIC 311.6 without patch distance scaled, 312 with patch distance

#SIMPLE
seedlingsSimple <- glmer.nb(seedlings22~ seedAbundance +(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=150,], control=controls)
summary(seedlingsSimple) #AIC 310.6

#seedlings patchDistance

m5 <- lmer(log(seedlings22+1)~ scale(patchDistance) + poly(roadDistance,2) + poly(seedAbundance, 2)+ log(heads21+1)+ (1|transectNum), data = flwrFig1Data)

summary(lgSeedlings)
Anova(lgSeedlings)

#road and seeds, when I add in heads it freaks out
# had to reduce patchdistance included because of all the zeroes towards the west
#bald1 just one model with seed abundance, do distance from road, one thats seeds and flowering heads
#run one analysis for seed abundance, one for seedlings, do model reduction based on AIC, report the best models, keep figure 4 but don't include lines of best fit if they're not significant. Put everything in there and take them out as they're not working. 
glmeNegBinomial<- glmer.nb(seedAbundance~ roadDistance +meanElevation_scaled+ poly(roadDistance,2) +(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=130,])

#when I add in patchDistance it makes elevation significant

summary(glmeNegBinomial)
Anova(glmeNegBinomial)

#testing by slowly adding in predictors one by 1, AIC went down as each was added
test1<- glmer.nb(seedAbundance~ roadDistance + patchDistance + poly(roadDistance,2)+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,]) #AIC 418
summary(test1)

test2 <- glmer.nb(seedAbundance~ roadDistance + patchDistance + +meanElevation_scaled+poly(roadDistance,2)+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,]) #AIC 412
summary(test2)
Anova(test2)
