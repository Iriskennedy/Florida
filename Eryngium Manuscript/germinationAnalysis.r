##making seed germination figure
library(tidyverse)
library(ggplot2)
library(stats)

germinationData <- read.csv("C:/Users/irisa/Documents/Archbold/germinationData/germinationData.csv")

##total.germ variable 

bald1 <- subset(germinationData, site=="Bald 1")
bald1TotalSeeds <- sum(bald1$seeds) #472
bald1TotalGerminated <- sum(bald1$total.germ)#85
bald1SucessRate <- (bald1TotalGerminated/bald1TotalSeeds)*100 #success rate as percent, 18%

bald59 <- subset(germinationData, site=="Bald 59")
bald59TotalSeeds <- sum(bald59$seeds) #28
bald59TotalGerminated <- sum(bald59$total.germ)#9
bald59SuccessRate <- (bald59TotalGerminated/bald59TotalSeeds)*100 #32

bald62<- subset(germinationData, site=="Bald 62")
bald62TotalSeeds <- sum(bald62$seeds) #11
bald62TotalGerm <- sum(bald62$total.germ) #1
bald62SuccessRate <- (bald62TotalGerm/bald62TotalSeeds)*100 #9

##Creating the data frame, calling it germinationAnalysis

bald <- c("1", "59", "62")
habitat <- c("road", "scrub", "scrub")
TotalSeeds <- c(bald1TotalSeeds, bald59TotalSeeds, bald62TotalSeeds)
TotalGerminated <- c(bald1TotalGerminated, bald59TotalGerminated, bald62TotalGerm)
successRate <- c(bald1SucessRate,bald59SuccessRate, bald62SuccessRate)

germinationAnalysis <- data.frame(bald, TotalSeeds, TotalGerminated, successRate, habitat)
view(germinationAnalysis)

#statistics

germinationData$habitat <- c("Bald 1"="road", "Bald 59" = "scrub", "Bald 62"="scrub")[germinationData$site]

model <- glm(formula = cbind(total.germ, seeds-total.germ)~habitat, family = binomial, data=germinationData)
summary(model)
#no significant difference between road and scrub seeds

## glm(formula = cbind(Galumna, totalabund - Galumna) ~ Topo + WatrCont, 
##     family = binomial, data = mites)

