## looking at Bald 59 with depth
## started jan 24 2022
## data frame name is bald59

library(tidyverse)
library(ggplot2)
library(dplyr)
library(car)

bald59 <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\rosemaryBald59.csv")

##first omit NA values
bald59%>% #name of overall data set
  filter(bald59$nearFar == "F")->depthData ##renamed filtered data set as depthData

abundanceDepth <- ggplot(depthData, aes(depth, seedAbundance, na.rm=TRUE))+
  geom_col()+
  ggtitle("Bald 59 Seed Abundance at Different Depths")+ xlab("Depth (cm)")+ylab("Seed Abundance")

##how to determine if this is significant?
## how to display n=11?