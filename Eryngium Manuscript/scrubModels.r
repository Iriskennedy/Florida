##making negative binomials for scrub data
# started jan 24, 2022
## Ec presence absence, look at when Ec is present nearfar, stem counts
#note presence = code 1, absence= 0 in presentAbsent variable column, scrubData has this
library(lme4)
library(tidyverse)
library(dplyr)
library(car)

scrubData <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\rosemaryBaldsPresentAbsent.csv")

##presence of eryngium cover analysis
glmePresentAbsent<- glmer.nb(seedAbundance~presentAbsent+(1|bald), data=scrubData)

summary(glmePresentAbsent)
Anova(glmePresentAbsent)

# analysis of near/far from rosemary
scrubData$nearFar <- factor(scrubData$nearFar, levels = c("N", "F"))
scrubData$seedAbundance_adjusted <- scrubData$seedAbundance + 0.0001

nearFarModel <- glmer.nb(seedAbundance~nearFar+(1|bald), offset=log(massActual), data=scrubData, verbose=TRUE)
summary(nearFarModel)

#I think the warning is because there is just not enough data, I'm gonna cut this figure

##presence absence graph ##changed it to mean, now it is messed up may have to create a new data frame... the one in the powerpoint is just the count with p vaue from glmm

x<-ggplot(scrubData, aes(x=as.factor(presentAbsent), y=seedAbundance, fill=as.factor(presentAbsent)))+
  geom_bar(stat="identity")+
  ggtitle("Effect of Eryngium Presence on Seed Bank")+ xlab("Absence (0) Presence (1)")+ylab("Seed Abundance")+scale_fill_manual(values=c("darkgrey", "darkgreen"))+labs(fill= "Present/Absence")+scale_x_discrete(labels=c("Absence", "Presence"))+theme(text=element_text(size=16))+theme(axis.title.x = element_blank())

## now for only when Ec is present named data set vegPresent
scrubData%>% 
  filter(scrubData$presentAbsent == "1")->vegPresent

glmeVegPresent<- glmer.nb(seedAbundance~vegCover + nearFar+(1|bald), data=vegPresent)
# gives result boundary (singular) fit: see help('isSingular') : Evaluates whether a fitted mixed model is (almost / near) singular, i.e., the parameters are on the boundary of the feasible parameter space: variances of one or more linear combinations of effects are (close to) zero.
summary(glmeVegPresent)
Anova(glmeVegPresent)

##vegcover vs seed abundance
ggplot(scrubData, aes(x=vegCover, y=seedAbundance))+
  geom_point(size=2)+
  geom_smooth(method=lm)
