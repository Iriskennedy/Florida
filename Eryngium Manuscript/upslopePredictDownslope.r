rm(list=ls()) #clears global environment
library(tidyverse)
library(dplyr)
library(car)
library(readxl)

demographyData <-read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\ecABSdemog.csv")
demographyData <- demographyData[demographyData$bald==1,]
fig1Data <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\bald1RoadsideRaw.csv")#this is the bald1data

#create a unique id (uid) for each record so that it can be merged with demography data. Create another column for downslope because names have to be the same
fig1Data$uid <- paste(fig1Data$patchDistance, fig1Data$roadDistance, sep = "_")
fig1Data$downslopeUid <- paste(fig1Data$patchDistance, fig1Data$roadDistance, sep = "_")

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

#make a roadAdjusted variable so that you can compare it to the seeds downslope 1m, add +1 to roadDistance
demographyData$roadAdjusted <- demographyData$roadDistance+1 #now we have NA, 2, 3, 4, 5

#create unique id for demography data, and downslope unique id
demographyData$uid <- paste(demographyData$patch, demographyData$roadDistance, sep = "_")
demographyData$downslopeUid <- paste(demographyData$patch, demographyData$roadAdjusted, sep = "_")

#now lets select only the columns we want to bring over from the fig1 data frame: downslopeUid, and seedAbundance

fig1Data <- fig1Data%>%
  select(seedAbundance, downslopeUid)

#now merge with demography data based on uid
demographyData <- merge(demographyData, fig1Data, by="downslopeUid")

ggplot(data= demographyData, aes(x=h21, y=seedAbundance))+
  geom_point()+
  xlim(0,80)+
  xlab("1m Upslope Flowering Heads in 2021") +   # Add x-axis title
  ylab("Seed Abundance in 2022")


#initialize the variable as full of NA values
demographyData$alive22 <- NA
# %in% tells to just look at these values (1,3,5) could do triple OR statement, I am only going to do this for 1,3 so it can be, basically %in% checks if something is equal to multiple things
demographyData$alive22[demographyData$s22 %in% c(1,3, 5)] <- 1 #assigns 1 to all living plants
#demographyData$seedling22[demographyData$s22 %in% c(5)] <- 1 #making seedling subset

#all things tht were alive are now 1s (1=alive, 3= new adult, 5=seedling)
#can make similar ones for seedlings (==5)

#could make one for flowering, 
demographyData$flwr21 <- NA
demographyData$flwr21<- demographyData$h22>=1

demographyData$seedlings22 <- NA
demographyData$seedlings22[demographyData$s22 %in% c(5)] <- 1
