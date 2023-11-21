## started Jan 7, 2023
##Attempting to make Figure 1 for Bald 1 Roadside Data

library(tidyverse)
fig1Data <- read.csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\bald1RoadsideRaw.csv")

#make FIGURE 1 scatterplot

roadsideOverviewPlot <- ggplot(data=fig1Data)+
  geom_point(mapping=aes(x=patchDistance, y=roadDistance, size=seedAbundance))+
  ggtitle("Seed Abundance Along 180 meters of Roadside")+ xlab("Distance Along the Road (m)")+ylab("Distance From Back Line of Scrub (m)")


ggsave("Roadside Overview", plot=roadsideOverviewPlot, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)

## making bar chart comparing seed abundance to distance from scrub
# how to add error bars?

library(dplyr)
df.summary <- fig1Data %>%
  group_by(roadDistance) %>%
  summarise(
    sd = sd(seedAbundance, na.rm = TRUE),
    seedAbundance = mean(seedAbundance)
  )
df.summary

## to make the basic column chart without error bars:
#ggplot(data=fig1Data)+
  #geom_col(mapping=aes(x=roadDistance, y=seedAbundance))

## NOPE plot1<-ggplot(fig1Data, aes(x=roadDistance, y=seedAbundance, ymin=seedAbundance-sd, ymax=seedAbundance + sd))

ggplot(df.summary, aes(roadDistance, seedAbundance))+
  geom_col(fill="lightgray", color="black")+
  geom_errorbar(aes(ymin=seedAbundance, ymax=seedAbundance + sd), width=.2)

#should i create a variable that is the total seed abundance for each road distance?

ggplot(fig1Data, aes(roadDistance, seedAbundance)) +
  geom_col(data = fig1Data, fill = 'lightgrey', color = "black") +
  geom_errorbar( aes(ymin = seedAbundance-sd, ymax = seedAbundance+sd), 
                 data = df.summary, width = 0.2 ) 

##creating standard error function
standard_error <- function(x) sd(x)/sqrt(length(x))

##TO FIX, instead of plus minus sd make it plus minus se

ggplot(data=df.summary)+
  geom_col(mapping=aes(x=roadDistance, y=seedAbundance))+
  geom_errorbar( aes(x=roadDistance, ymin = seedAbundance-standard_error(seedAbundance), ymax = seedAbundance-standard_error(seedAbundance), width = 0.3))
                

## creating variable to store standard error of seed abundance
seSeedAbundance <- (sqrt(sum((fig1Data$seedAbundance - mean(fig1Data$seedAbundance)) ^ 2/(length(fig1Data$seedAbundance) - 1))))/sqrt(length(fig1Data$seedAbundance))

##Initialize ggplot
plot1 <- ggplot(
  df.summary, 
  aes(x = roadDistance, y = seedAbundance, ymin = seedAbundance-sd, ymax = seedAbundance+sd)
)

plot1<-ggplot(df.summary, aes(x=roadDistance, y=seedAbundance))+geom_col()+  geom_errorbar(aes(ymin = seedAbundance+sd, ymax = roadDistance+sd), width = 0.2) 
#plot1<-ggplot(df.summary, aes(x=roadDistance, y=seedAbundance, ymin = seedAbundance-sd, ymax = seedAbundance+sd))

#plot1 + geom_pointrange()

# Standard error bars
plot + geom_errorbar(width = 0.2) +
  geom_point(size = 1.5)