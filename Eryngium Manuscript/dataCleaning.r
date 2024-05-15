##Data cleaning
#started by Iris Kennedy on 5/2/2024
#cleaning up eryngium data sets

library(tidyverse)

fig1Data <- read_csv("Raw Data\\bald1RoadsideRaw.csv")#this is the bald1data

#create a unique id (uid) for each road record so that each record is a unique bag of sand which will be the random effect. Create another column because names have to be the same
fig1Data$uid <- paste(fig1Data$patchDistance, fig1Data$roadDistance, sep = "_")

#checking for duplicated unique ids which indicate an error
duplicates <- fig1Data$uid[duplicated(fig1Data$uid)]

#There is a duplicate at uid 130_4 so I will filter out the entire patchDistance 130 since I cannot say which number is correct

fig1Data <- fig1Data <- subset(fig1Data, patchDistance!=130)

#exporting the clean data:
write.csv(fig1Data, file = "cleanData\\roadData.csv", row.names = FALSE)
