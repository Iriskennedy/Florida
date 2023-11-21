## Stats Analysis for Bald 1 (saved as statsBald1)
## creating GLM for bald 1 roadside data
#started jan 18

library(tidyverse)
library(dplyr)
library(car)

fig1Data <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\bald1RoadsideRaw.csv")

#create object to hold the glm: bald1Poisson, factor(roadDistance) makes it into a factor not integer
## list y~x, data, family

bald1Poisson <- glm(seedAbundance~factor(roadDistance), data=fig1Data, family="poisson")

##quadratic might fit better than linear because there is a max then a dropoff, I(roadDistance^2) is the quadratic term squaring x (roadDistance is the predictor here)
bald1Poisson <- glm(seedAbundance~factor(roadDistance) + I(roadDistance^2), data=fig1Data, family="poisson")

#do summary(bald1poisson) to check/see what the model found
summary(bald1Poisson)
Anova(bald1Poisson)

##use boxplot to just explore/visualize data
boxplotPoisson <- boxplot(log(fig1Data$seedAbundance+1)~fig1Data$roadDistance)

##post hoc test

library(emmeans) ##emmeans gives you the back transformed means that you should use

emmeans(bald1Poisson, list(pairwise ~ roadDistance), adjust="tukey",type="response")

library(lme4) ##lme is linear mixed effects,  (1|school:class) would 
#specify a random effect (the parentheses) for the interaction of school and class (the colon).
glmePoisson<- glmer(seedAbundance~roadDistance + I(roadDistance^2) +(1|transectNum), data=fig1Data, family="poisson")

summary(glmePoisson)
Anova(glmePoisson)

##negative binomial, order does matter for this think going from broad to small patchdistance is more broad, roaddistance is smaller
glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + I(roadDistance^2) +(1|transectNum), data=fig1Data)

summary(glmeNegBinomial) ## gives lower BIC than poisson model which means it may be better
Anova(glmeNegBinomial)
#in summary the Estimate column is the slopes

##make new dataframe called negBinNew with road distance and patch distance as sequences

negBinNew <- expand.grid(roadDistance=seq(1,4, by=.1), patchDistance=seq(0,180, by=5))
sort.list(negBinNew$patchDistance)
negBinNew$predictedSeeds <- predict(glmeNegBinomial, newdata=negBinNew, re.form=~0, type=c("response"))

plot(negBinNew$patchDistance, negBinNew$predictedSeeds, type="l")


## this basically worked except I can't get a bunch of patchDistance values
negBinNew <- data.frame(roadDistance=seq(1,4, by=.1), patchDistance=17.5)
negBinNew$predictedSeeds <- predict(glmeNegBinomial, newdata=negBinNew, re.form=~0, type=c("response"))
##plot just the actual model
ggplot(negBinNew, aes(roadDistance, predictedSeeds))+
  geom_line()

##I plotted the model on top of the actual data and it looks interesting...
ggplot(negBinNew, aes(roadDistance, predictedSeeds))+
  geom_line()+
  geom_point(data=fig1Data, mapping=aes(x=roadDistance, y=seedAbundance))+
  ggtitle("Seed Abundance From Scrub to Road")+ xlab("Distance Towards Road from Scrubline (m)")+ylab("Seed Abundance")

##now try patchDistance
##this seems to have worked the best so far
negBinNew <- expand.grid(roadDistance=seq(1,4, by=.1), patchDistance=seq(0,180, by=5))
sort.list(negBinNew$patchDistance)
negBinNew$predictedSeeds <- predict(glmeNegBinomial, newdata=negBinNew, re.form=~0, type=c("response"))

plot(negBinNew$patchDistance, negBinNew$predictedSeeds, type="l")


negBinPatch <- expand.grid(roadDistance=seq(1,4, by=.1), patchDistance=seq(0,180, by=5))
negBinPatch$predictedSeeds <- predict(glmeNegBinomial, newdata=negBinNew, re.form=~0, type=c("response"))
order(negBinPatch)

ggplot(negBinPatch, aes(patchDistance, predictedSeeds))+
  geom_line()

plot(negBinNew$patchDistance, negBinNew$predictedSeeds, type="l")

library(ggplot2)
ggplot()
plot(negBinNew$roadDistance, negBinNew$predictedSeeds, type="l")

#expand.grid is alternative to data frame to put every combo of if it is chaos sort the data
##need to ignore random effect, re.form=~0 says dont care about random effect

#create plot to view model?
library(ggplot2)
library(ggpredict)
library(ggiraphExtra)
library(sjPlot)
library(tidyverse)
library(GGally)
library(broom.helpers)
library(broom.mixed)
ggcoef_model(glmeNegBinomial)

sjp.lmer(glmeNegBinomial)
ggPredict(glmeNegBinomial,se=TRUE,interactive=TRUE,digits=3)

logLik(glmeNegBinomial)
plot_model(glmeNegBinomial)

ggPredict(glmeNegBinomial, terms="roadDistance", condition=c(seedAbundance=0))

negBinPlot <- ggplot(fig1Data, aes(x = patchDistance, y = seedAbundance) ) +
  geom_point() +
 geom_smooth(method = "glm", alpha = .15)

negBin.RoadDistPlot <- ggplot(fig1Data, aes(x = roadDistance, y = seedAbundance) ) +
  geom_point() +
  geom_smooth(method = "glm", alpha = .15)

#poissonPlot <- ggplot(data=bald1Poisson, mapping = aes(x=roadDistance, y=seedAbundance))+
 # geom_point(size=2)

predict(glmeNegBinomial)


