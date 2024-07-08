#elevation gradient models
library(lme4)
library(car)
library(tidyverse)

# there is a significant effect as we find fewer seeds further down in elevation. We could investigate this further in the demographic analyses.
# 
# For example, the analyses could look like this:
# seed abundance ~ flowering heads * patch + flowering heads * road
# seedlings ~ seeds * patch + seeds * road
# 
# The interaction terms, particularly the ones with patch (i.e. elevation) get at the question of whether seed banking differs along this gradient. A significant flowering heads x patch interaction could mean that further down in elevation seeds are being produced, but not necessarily banking (could be rotting, for example). Or, perhaps the seeds are there, but the seedlings aren't able to germinate in those lower elevations. Non-significant interactions would mean that it is a straightforward relationship betwen flowering heads and seed abundance, or seeds and seedlings.

load("dataFrames/flwrFig1Data.RData")

flwrElevation <- glmer.nb(seedAbundance~heads21*patchDistance + heads21*roadDistance+(1|transectNum), data=flwrFig1Data)

# Rescale predictor variables
flwrFig1Data$heads21_scaled <- scale(flwrFig1Data$heads21)
flwrFig1Data$patchDistance_scaled <- scale(flwrFig1Data$patchDistance)
flwrFig1Data$roadDistance_scaled <- scale(flwrFig1Data$roadDistance)

# Control settings with increased iterations and different optimizer
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e6))

# Fit the model with rescaled variables and custom control settings
flwrElevation <- glmer.nb(seedAbundance ~ heads21_scaled * patchDistance_scaled + heads21_scaled * roadDistance_scaled + (1 | transectNum), 
                          data = flwrFig1Data, control = control)
summary(flwrElevation)
Anova(flwrElevation)

