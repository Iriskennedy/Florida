# testing for correlation between seeds and location of living plants
##2/28
## create models to look at relationships between seeds and plants and seeds and flowers, making demography data fit roadside
##REMEMBER TO CLEAN OUT MESSED UP TRANSECT FROM MY BALD 1 DATA
rm(list=ls()) #clears global environment
library(tidyverse)
library(dplyr) #NOTE- some dplyr functions may be masked, fix with dplyr::
library(car)
library(readxl)
library(lme4)
library(MASS)

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


##doing this for all alive plants
bestfitAlive<- lm(temp2$seedAbundance~temp2$alive22)
abundanceAlive <- ggplot(temp2, aes(x=alive22, y=seedAbundance))+
  geom_point()+
  geom_smooth(method = "lm", se=TRUE, color="black", formula = y ~ x) +
  ggtitle("Seed Abundance vs Number of Plants")+xlab("Number of Plants")+ylab("Seed Abundance")

Save_directory <- "graphs" #saving graph object
saveRDS(object = abundanceAlive, file.path(save_directory, "abundanceAlive.rds"))

cor.test(temp2$alive22, temp2$seedAbundance)
cor.test(temp2$alive22, temp2$seedAbundance, method = "spearman")
#correlation of 0.349945 between living plants and seeds
#spearman rho .4487, moderate strength positive relationship