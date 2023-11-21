#manipulating all the rosemary bald data together (saved preliminary plots in Intern Project folder)
# preliminary data visualisation

rosemaryBalds <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\rosemaryBaldsCombined.csv")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)

##basic bar chart showing seeds near and far from rosemary
allSeeds <- ggplot(rosemaryBalds, aes(nearFar, seedAbundance))+
  geom_col()+
  ggtitle("Seed Abundance Far and Near to Rosemary")+ xlab("Near (N) or Far (F)")+ylab("Seed Abundance")

##to test the significance in this relationship should I use a chi squared test? Testing difference between two categorical variables (Near and Far)

meanRoseSeeds <- mean(rosemaryBalds$seedAbundance)
chisq.test(rosemaryBalds$seedAbundance, rosemaryBalds$nearFar, correct=FALSE)

t.test(rosemaryBalds$seedAbundance[rosemaryBalds$nearFar=="N"], rosemaryBalds$seedAbundance[rosemaryBalds$nearFar=="F"], correct=FALSE)

t.test(meanRoseSeeds[rosemaryBalds$nearFar=="N"], meanRoseSeeds[rosemaryBalds$nearFar=="F"], correct=FALSE)


##RESULT: Pearson's Chi-squared test
# data:  rosemaryBalds$seedAbundance and rosemaryBalds$nearFar
# X-squared = 4.3483, df = 6, p-value = 0.6297 --- not significant

#seed abundance in the different balds, note changed rosemaryBalds$bald to a factor
baldAbundance <- ggplot(rosemaryBalds, aes(bald, seedAbundance))+
  geom_col()+
  ggtitle("Seed Abundance in Different Rosemary Balds")+ xlab("Rosemary Bald")+ylab("Seed Abundance")

# looking at vegcover and seed abundance 
baldVegCover <- ggplot(rosemaryBalds, aes(vegCover, seedAbundance))+
  geom_point(size=2)+
  ggtitle("Vegetation Cover and Seed Abundance in Rosemary Balds")+ xlab("Vegetation Cover (number of stems")+ylab("Seed Abundance")
