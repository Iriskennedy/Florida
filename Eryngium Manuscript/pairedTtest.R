##trying to do a paired t test for scrub interior
#started on 5/22/2024

library(tidyverse)
library(stats)

scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45) #taking out bald 45 because there were no seeds found

scrubSub <- scrubData%>%
  select(transectNum, nearFar, seedAbundance, abundanceMass)%>%
  mutate(transectNum= as.character(transectNum))

#reshape to have columns for transectNum, N, F and Seed abundance

newdf <- reshape(scrubSub, direction = "wide", idvar = "transectNum", timevar = "nearFar")

t.test(scrubSub$nearFar)

t.test(scrubData$seedAbundance, fig1Data$seedAbundance)