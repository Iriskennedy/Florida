##2/28
## create models to look at relationships between seeds and plants and seeds and flowers, making demography data fit roadside
##REMEMBER TO CLEAN OUT MESSED UP TRANSECT FROM MY BALD 1 DATA
rm(list=ls()) #clears global environment
library(tidyverse)
library(dplyr) #NOTE- some dplyr functions may be masked, fix with dplyr::
library(car)
library(readxl)
library(lme4)

demographyData <-read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\ecABSdemog.csv")
demographyData <- demographyData[demographyData$bald==1,]
fig1Data <- read_csv("cleanData/roadData.csv")#this is the bald1data

#create a unique id (uid) for each record so that it can be merged with demography data

#adds column of NAs for road dist, this is how we;re creating variables
demographyData$roadDistance <- NA

#cutoff variable allows you to set cutoffs around each of the road distances and "capture" those plants, can mess with the values
cutoff <- .25
demographyData$roadDistance <- NA

#dfine zones one at a time, 1s refer to my roadDistance 1 (1m), do 4 times change 1 to 2,3,4
demographyData$roadDistance[demographyData$y.cor>=(1-cutoff)&demographyData$y.cor<(1+cutoff)]<- 1

demographyData$roadDistance[demographyData$y.cor>=(2-cutoff)&demographyData$y.cor<(2+cutoff)]<- 2
demographyData$roadDistance[demographyData$y.cor>=(3-cutoff)&demographyData$y.cor<(3+cutoff)]<- 3
demographyData$roadDistance[demographyData$y.cor>=(4-cutoff)&demographyData$y.cor<(4+cutoff)]<- 4

#initialize the variable as full of NA values
demographyData$alive22 <- NA
# %in% tells to just look at these values (1,3,5) could do triple OR statement, I am only going to do this for 1,3 so it can be, basically %in% checks if something is equal to multiple things
demographyData$alive22[demographyData$s22 %in% c(1,3, 5)] <- 1 #assigns 1 to all living plants
#demographyData$seedling22[demographyData$s22 %in% c(5)] <- 1 #making seedling subset

#all things tht were alive are now 1s (1=alive, 3= new adult, 5=seedling)
#can make similar ones for seedlings (==5)

#could make one for flowering, 
demographyData$flwr21 <- NA
demographyData$flwr21<- demographyData$h22>=1 #this creates a column of TRUE FALSE

demographyData$seedlings22 <- NA
demographyData$seedlings22[demographyData$s22 %in% c(5)] <- 1

#aggregating two lists, variable, what you want to aggregate by
#making new colum called alive 22 based off demography data 22, can add # flowers etc response variables
#why sum, 1,3,5 are categories made 1s to count as alive

##mKING s22 seedling survival column
demographyData$seedlingSurvival22[!is.na(demographyData$s21)&demographyData$s21==5 & demographyData$s22==1]<- 1

demographyData$seedlingSurvival22[!is.na(demographyData$s21)&demographyData$s21==5 & demographyData$s22==0] <- 0

##make total seddling survival, 1 means seedling survived 0 means it did not

demographyData$seedlingSurvival21[!is.na(demographyData$s20)&demographyData$s20==5 & demographyData$s21==1]<- 1

demographyData$seedlingSurvival21[!is.na(demographyData$s20)&demographyData$s20==5 & demographyData$s21==0] <- 0

##2020
demographyData$seedlingSurvival20[!is.na(demographyData$s19)&demographyData$s19==5 & demographyData$s20==1]<- 1

demographyData$seedlingSurvival20[!is.na(demographyData$s19)&demographyData$s21==5 & demographyData$s20==0] <- 0
##rowSums
##need it to id rows with all NAs and skip over those rows, we don't want NAs, could make a column and mark with 1s or 0s (tellw which colum to run on)
demographyData$seedlingSurvival20to22 <- NA
demographyData$seedlingSurvival20to22 <- rowSums(demographyData[,c("seedlingSurvival20","seedlingSurvival21", "seedlingSurvival22")][!is.na(demographyData$seedlingSurvival20) | !is.na(demographyData$seedlingSurvival21) | !is.na(demographyData$seedlingSurvival22)], na.rm=TRUE)

temp <- aggregate(list(alive22=demographyData$alive22, seedlings22=demographyData$seedlings22, heads21=demographyData$h21), list(patchDistance=demographyData$patch, roadDistance=demographyData$roadDistance), sum, na.rm=TRUE)
## note negative patch
##added in heads21 to look at the flowering heads in 2021, so temp is the one to work with to make flowering graph?

##merges in mean seedling survival, temp3 is mean
temp3 <- aggregate(list(seedlingSurvival22=demographyData$seedlingSurvival22), list(patchDistance=demographyData$patch, roadDistance=demographyData$roadDistance), mean, na.rm=TRUE)

##merge temp and temp3
temp4 <-merge(temp, temp3, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE)

temp2 <- merge(fig1Data, temp4, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE)

#SKIP HERE TO MAKE THE HEADS VS SEED ABUNDANCE FIGURE
flwrFig1Data <-merge(fig1Data, temp, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE) #merges fig1 data and temp which has eryngium demography data including flowring heads, should I remove NAs in flowering heads?

#check dim, same rows +2 colums, double check NA is never any plant vs 0 there was and it is dead, change NAs to zeros
##note there are still NAs in heads but I think they should stay that way

#chanigng NA to zero, ##opposite of is.na is !is.na()
temp2$alive22[is.na(temp2$alive)]<-0
##^tells that anything that is an NA value gets a zero
temp2$seedlings22[is.na(temp2$seedlings22)] <-0

# flwrFig1Data$heads21[is.na(flwrFig1Data$heads21)] <- 0 #changing NAs in heads21 to zeros, not sure if I should
# flwrFig1Data$heads21[is.na(flwrFig1Data$seedlings22)] <- 0

library(ggplot2)

#this graph is for seed abundance vs number flowering heads
bestfit<- lm(flwrFig1Data$seedAbundance~flwrFig1Data$heads21)
flwrAbundance <- ggplot(flwrFig1Data, aes(x=flwrFig1Data$heads21, y=flwrFig1Data$seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Flowering Heads in 2021")+xlab("Number of Flowering Heads")+ylab("Seed Abundance")

save_directory <- "graphs" #saving graph object
saveRDS(object = flwrAbundance, file.path(save_directory, "flwrAbundance.rds"))

##doing this for all alive plants
bestfitAlive<- lm(temp2$seedAbundance~temp2$alive22)
abundanceAlive <- ggplot(temp2, aes(x=alive22, y=seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Plants")+xlab("Number of Plants")+ylab("Seed Abundance")

Save_directory <- "graphs" #saving graph object
saveRDS(object = abundanceAlive, file.path(save_directory, "abundanceAlive.rds"))

cor.test(temp2$alive22, temp2$seedAbundance)
#correlation of 0.349945 between living plants and seeds

##making graph of seedlings vs seed abundance
#poly(seedAbundance,2) helps get rid of colinearity issue
bestfit<- glm(seedlings22~poly(seedAbundance,2), data = flwrFig1Data, family=poisson())
bestfitNo<- glm(seedlings22~seedAbundance, data = flwrFig1Data, family=poisson())
bestfitAdded<- glm(seedlings22~poly(seedAbundance,2)+patchDistance+roadDistance, data = flwrFig1Data, family=poisson())
summary(bestfitAdded)#comopare with or without polynomial, then do anova to choose

#this creates the confidence intervals
bestfit_predict <- predict(bestfit, newdata =flwrFig1Data,  type="link", se = T)
bestfit_predict <- predict(bestfitAdded, newdata =flwrFig1Data,  type="link", se = T)

flwrFig1Data$fit <- exp(bestfit_predict$fit)
flwrFig1Data$lci <- exp(bestfit_predict$fit - 1.96*bestfit_predict$se.fit)
flwrFig1Data$uci <- exp(bestfit_predict$fit + 1.96*bestfit_predict$se.fit)


abundanceSeedlings <- ggplot(flwrFig1Data, aes(x=seedAbundance, y=seedlings22))+
  geom_point()+
  # geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5) +
   geom_line(aes(y = fit) )+
   geom_smooth(method = "glm", se=FALSE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Seedlings in 2022")+xlab("Number of Seeds")+ylab("Seedlings")

abundanceSeedlings

#USE THIS: making test predicted data:
## Fit the models
bestfit <- glm(seedlings22 ~ poly(seedAbundance, 2), data = flwrFig1Data, family = poisson())
flwrFig1Data$patchDistanceF <- as.factor(flwrFig1Data$patchDistance)
bestfitAdded <- glm(seedlings22 ~ poly(seedAbundance, 2) + patchDistanceF +roadDistance, data = flwrFig1Data, family = poisson())

summary(bestfitAdded)
## Predict fitted values and confidence intervals
flwrFig1Data$fit <- exp(predict(bestfit, newdata = flwrFig1Data, type = "link"))
flwrFig1Data$fitAdded <- exp(predict(bestfitAdded, newdata = flwrFig1Data, type = "link")) #filter newdata to only equal places where seedlings are not NA, predict is breaking because there are NAs in seedlings 22

#can try: 
flwrFig1Data_complete <- na.omit(flwrFig1Data)


#
newDF <- expand.grid(patchDistance=seq(0,180,5), roadDistance=1:4, seedAbundance=seq(0,30,1))#makes all combo
newDF$fit <- predict(bestfitAdded, newdata=newDF,re.form=~0)#this tells it to ignore random effect in predictions

reducedModel <- glmer(seedlings22 ~ (1|patchDistance) +roadDistance, data = flwrFig1Data, family = poisson())
newDF <- expand.grid( roadDistance=seq(1,4, .01))
newDF$fit <- predict(reducedModel, newdata=newDF,re.form=~0)

## Create the plot, THIS ONE WORKS- LOOKS BEST WITHOUT ADDITIVE, maybe just use this for visualizing but for the actual stats include the added patchDistance etc terms bc lower AIC
abundanceSeedlings <- ggplot(flwrFig1Data, aes(x = seedAbundance, y = seedlings22)) +
  geom_point() +
 # geom_smooth(method = "glm", se = FALSE, color = "black", formula = y ~ x) +
  geom_line(aes(y = fit), color = "black") +  # Fitted values from bestfit
  geom_ribbon(aes(ymin = fit - 1.96 * bestfit_predict$se.fit, ymax = fit + 1.96 * bestfit_predict$se.fit), 
              alpha = 0.2, fill = "grey50") +  # 95% confidence interval
  #geom_line(aes(y = fitAdded), color = "red") +  # Fitted values from bestfitAdded
  #geom_line(aes(y=newDF$fit)) #fix this
  ggtitle("Seed Abundance vs Number of Seedlings in 2022") +
  xlab("Number of Seeds") + ylab("Seedlings")

save_directory <- "graphs"
saveRDS(object = abundanceSeedlings, file.path(save_directory, "abundanceSeedlings.rds"))

  plot(newDF$roadDistance, newDF$fit, type="l") #tells for each road distance it increasews
  
## Show the plot
print(abundanceSeedlings)


#make test data frame with even 1-170 sequence in patch distance and then make columns with same names
testData <- seq()
bestfit_predict <- predict(bestfitAdded, newdata =flwrFig1Data,  type="link", se = T)

flwrFig1Data$fit <- exp(bestfit_predict$fit)
flwrFig1Data$lci <- exp(bestfit_predict$fit - 1.96*bestfit_predict$se.fit)
flwrFig1Data$uci <- exp(bestfit_predict$fit + 1.96*bestfit_predict$se.fit)

abundanceSeedlings2 <- ggplot(flwrFig1Data, aes(x=patchDistance, y=seedlings22))+
  geom_point()+
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5) +
  geom_line(aes(y = fit) )
##this is showing no significant relationship

ggsave("Seeds vs Plants", plot=abundanceAlive, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)



cor.test(temp2$seedAbundance, temp2$alive22) #yields .246

cor.test(flwrFig1Data$seedAbundance, flwrFig1Data$heads21) #yields .3176

cor.test(flwrFig1Data$seedAbundance, flwrFig1Data$seedlings22) #yields .0774

##Residual standard error: 10.84 on 77 degrees of freedom
#(68 observations deleted due to missingness)
#Multiple R-squared:  0.06065,	Adjusted R-squared:  0.04845 
#F-statistic: 4.972 on 1 and 77 DF,  p-value: 0.02868

cor.test(temp2$seedAbundance, temp2$seedlings22)
boxplot(temp2$seedlings22~ temp2$roadDistance)

##finding mean and se of seedlings

standard_error <- function(x) sd(x)/sqrt(length(x))

df.seedlingSummary <- temp2 %>%
  group_by(roadDistance) %>%
  summarise(
    se = standard_error(seedlings22),
    seedlings22 = mean(seedlings22)
  )

df.seedlingSummary <- temp2 %>%
  group_by(roadDistance) %>%
  summarise(
    se = sd(seedlings22, na.rm = TRUE) / sqrt(sum(!is.na(seedlings22))),
    seedlings22 = mean(seedlings22, na.rm = TRUE)
  )

df.seedlingSummary


seedlingsRoad <- ggplot(df.seedlingSummary, aes(x=roadDistance, y=seedlings22))+
  geom_col(fill='lightgrey', colour = "black")+
  ggtitle("Mean Seedling Abundance Along Scrub to Road Gradient")+xlab("Distance from Scrub to Road (m)")+ylab("Mean Number of Seedlings")+
  geom_errorbar( aes(ymin = seedlings22-se, ymax = seedlings22+se), data = df.seedlingSummary, width = 0.2)

save_directory <- "graphs"
saveRDS(object = seedlingsRoad, file.path(save_directory, "seedlingsRoad.rds"))

##seedlings road models
library(lme4)
seedlingNegBin<- glmer.nb(seedlings22~patchDistance+ roadDistance + I(roadDistance^2) +(1|transectNum), data=temp2)

summary(seedlingNegBin)

ggsave("Seedlings vs Road Distance", plot=seedlingsRoad, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)

boxplot(temp2$seedlingSurvival22~ temp2$roadDistance)

seedlingSurvivalRoad <- ggplot(temp2, aes(x=roadDistance, y=seedlingSurvival22))+
  geom_col()+
  ggtitle("Seedling Abundance Along Scrub to Road Gradient")+xlab("Distance from Scrub to Road (m)")+ylab("Number of Seedlings that Survived")


cor.test(temp2$heads21, temp2$seedlings22)
## .09
with(temp2[temp2$roadDistance==4,], cor.test(seedlings22, seedAbundance))

##testing correlation and boxplot of flowering heads 2021
with(temp2[temp2$roadDistance==4,], cor.test(heads21, seedAbundance))
boxplot(temp2$heads21~ temp2$roadDistance)
##yields cor of .08, not sure if this is the proper way to test it

temp2$ratio <-(temp2$seedlings22+1)/(temp2$seedAbundance+1)

boxplot(temp2$ratio~temp2$roadDistance) ##seedbanking happening more at 1,2,3

library(lme4)

m<-lmer(log(ratio)~roadDistance + (1|patchDistance), data=temp2)
summary(m)
Anova(m)
hist(resid(m)) ##checks histogram of reviduals, bad one is multi modal (2 peaks)
