##2/28

library(tidyverse)
library(dplyr)
library(car)
library(readxl)

demographyData <-read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\ecABSdemog.csv")
demographyData <- demographyData[demographyData$bald==1,]
fig1Data <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\bald1RoadsideRaw.csv")

#adds column of NAs for road dist, this is how we;re creating variabls
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
# %in% tells to just look at these values (1,3,5) could do triple OR statement, I am only going to do this for 1,3 so it can be
demographyData$alive22[demographyData$s22 %in% c(1,3, 5)] <- 1
#demographyData$seedling22[demographyData$s22 %in% c(5)] <- 1 #making seedling subset

#all things tht were alive are now 1s (1=alive, 3= new adult, 5=seedling)
#can make similar ones for seedlings (==5)

#could make one for flowering, 
demographyData$flwr21 <- NA
demographyData$flwr21<- demographyData$h22>=1

demographyData$seedlings22 <- NA
demographyData$seedlings22[demographyData$s22 %in% c(5)] <- 1

#aggregating two lists, variable, what you want to aggregate by
#making new colum called alive 22 based off demography data 22, can add # flowers etc response variables
#why sum, 1,3,5 are categories made 1s to count as alive

##mKING s22 seedling survival column
demographyData$seedlingSurvival22[!is.na(demographyData$s21)&demographyData$s21==5 & demographyData$s22==1]<- 1

demographyData$seedlingSurvival22[!is.na(demographyData$s21)&demographyData$s21==5 & demographyData$s22==0] <- 0

##make total seddling sur

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
#temp3 <- aggregate(list(seedlingSurvival22=demographyData$seedlingSurvival22), list(patchDistance=demographyData$patch, roadDistance=demographyData$roadDistance), mean, na.rm=TRUE)

##merge temp and temp3
temp4 <-merge(temp, temp3, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE)

temp2 <- merge(fig1Data, temp4, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE)

flwrFig1Data <-merge(fig1Data, temp, by=c("patchDistance", "roadDistance"), all.x = TRUE, all.y = FALSE) #merges fig1 data and temp which has eryngium demography data including flowring heads, should I remove NAs in flowering heads?

#check dim, same rows +2 colums, double check NA is never any plant vs 0 there was and it is dead, change NAs to zeros
##note there are still NAs in heads but I think they should stay that way



#chanigng NA to zero, ##opposite of is.na is !is.na()
temp2$alive22[is.na(temp2$alive)]<-0
##^tells that anything that is an NA value gets a zero
temp2$seedlings22[is.na(temp2$seedlings22)] <-0

flwrFig1Data$heads21[is.na(flwrFig1Data$heads21)] <- 0 #changing NAs in heads21 to zeros
flwrFig1Data$heads21[is.na(flwrFig1Data$seedlings22)] <- 0

library(ggplot2)

bestfit<- lm(flwrFig1Data$seedAbundance~flwrFig1Data$heads21)
flwrAbundance <- ggplot(flwrFig1Data, aes(x=flwrFig1Data$heads21, y=flwrFig1Data$seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Flowering Heads 2021")+xlab("Number of Flowering Heads")+ylab("Seed Abundance")

##doing this for all alive plants
bestfit<- lm(temp2$seedAbundance~temp2$alive22)
abundanceAlive <- ggplot(temp2, aes(x=alive22, y=seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Plants")+xlab("Number of Plants")+ylab("Seed Abundance")

##making graph of seedlings vs seed abundance
bestfit<- lm(flwrFig1Data$seedAbundance~flwrFig1Data$seedlings22)
abundanceSeedlings <- ggplot(flwrFig1Data, aes(x=seedlings22, y=seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Seedlings in 2022")+xlab("Number of Seedlings")+ylab("Seed Abundance")


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
df.seedlingSummary <- temp2 %>%
  group_by(roadDistance) %>%
  summarise(
    se = standard_error(seedlings22),
    seedlings22 = mean(seedlings22)
  )
df.seedlingSummary


seedlingsRoad <- ggplot(df.seedlingSummary, aes(x=roadDistance, y=seedlings22))+
  geom_col(fill='lightgrey', colour = "black")+
  ggtitle("Seedling Abundance Along Scrub to Road Gradient")+xlab("Distance from Scrub to Road (m)")+ylab("Number of Seedlings")+
  geom_errorbar( aes(ymin = seedlings22-se, ymax = seedlings22+se), data = df.seedlingSummary, width = 0.2)

ggsave("Seedlings vs Road Distance", plot=seedlingsRoad, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)

boxplot(temp2$seedlingSurvival22~ temp2$roadDistance)

seedlingSurvivalRoad <- ggplot(temp2, aes(x=roadDistance, y=seedlingSurvival22))+
  geom_col()+
  ggtitle("Seedling Abundance Along Scrub to Road Gradient")+xlab("Distance from Scrub to Road (m)")+ylab("Number of Seedlings")


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
