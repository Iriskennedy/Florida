##trying to do a paired t test for scrub interior
#started on 5/22/2024

library(tidyverse)
library(stats)

scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45) #taking out bald 45 because there were no seeds found

scrubSub <- scrubData%>%
  select(transectNum, nearFar, seedAbundance)%>%
  mutate(transectNum= as.factor(transectNum),
         nearFar=as.factor(nearFar))

#reshape to have columns for transectNum, N, F and Seed abundance

newdf <- pivot_wider(scrubSub, names_from = nearFar, values_from = seedAbundance)

t.test(newdf$N, newdf$F)

t.test(scrubData$seedAbundance, fig1Data$seedAbundance)

#data:  newdf$N and newdf$F
#t = -0.43683, df = 45.143, p-value = 0.6643

#checking assumptions
# compute the difference, we need to check whether the differences of the pairs follow a normal distribution.
d <- with(my_data, 
          weight[group == "before"] - weight[group == "after"])
# Shapiro-Wilk normality test for the differences
shapiro.test(d) # => p-value = 0.6141