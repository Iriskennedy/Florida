#manipulating all the rosemary bald data together (saved preliminary plots in Intern Project folder)
# preliminary data visualisation

rosemaryBalds <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\rosemaryBaldsCombined.csv")

library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)

##basic bar chart showing seeds near and far from rosemary
allSeeds <- ggplot(rosemaryBalds, aes(nearFar, abundanceMass))+
  geom_col()+
  ggtitle("Seed Abundance Far and Near to Rosemary")+ xlab("Far (F) or Near (N)")+ylab("Seed Abundance/Mass (seeds/gram)")

##to test the significance in this relationship should I use a chi squared test? Testing difference between two categorical variables (Near and Far)

meanRoseSeeds <- mean(rosemaryBalds$seedAbundance)
chisq.test(rosemaryBalds$abundanceMass, rosemaryBalds$nearFar, correct=FALSE)

## checking if I can use chi squared test:
# Create a contingency table
contingency_table <- table(rosemaryBalds$abundanceMass, rosemaryBalds$nearFar)

# Perform chi-squared test with simulate.p.value = TRUE to calculate expected frequencies
chi_squared_test <- chisq.test(rosemaryBalds$abundanceMass, rosemaryBalds$nearFar, simulate.p.value = TRUE)

# Extract the expected frequencies from the test result
expected_counts <- chi_squared_test$expected

# Combine observed and expected frequencies into a single table
contingency_table_with_expected <- cbind(contingency_table, expected_counts)

# Print the table
print(contingency_table_with_expected)


t.test(rosemaryBalds$abundanceMass[rosemaryBalds$nearFar=="N"], rosemaryBalds$abundanceMass[rosemaryBalds$nearFar=="F"], correct=FALSE)

t.test(meanRoseSeeds[rosemaryBalds$nearFar=="N"], meanRoseSeeds[rosemaryBalds$nearFar=="F"], correct=FALSE)


##RESULT: Pearson's Chi-squared test
# data:  rosemaryBalds$seedAbundance and rosemaryBalds$nearFar
# X-squared = 4.3483, df = 6, p-value = 0.6297 --- not significant

#seed abundance in the different balds, note changed rosemaryBalds$bald to a factor
baldAbundance <- ggplot(rosemaryBalds, aes(bald, abundanceMass))+
  geom_col()+
  ggtitle("Seed Abundance in Different Rosemary Balds")+ xlab("Rosemary Bald (Site)")+ylab("Seed Abundance/Volume (seeds/gram")

subset2 <- subset(rosemaryBalds, bald==45)
unique(subset2$seedAbundance)

# looking at vegcover and seed abundance 
baldVegCover <- ggplot(rosemaryBalds, aes(vegCover, seedAbundance))+
  geom_point(size=2)+
  ggtitle("Vegetation Cover and Seed Abundance in Rosemary Balds")+ xlab("Vegetation Cover (number of stems")+ylab("Seed Abundance")
