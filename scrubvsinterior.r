##comparing interior and exterior
# started 1/27/23

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr) #for normality tests
#1 load data rosemary: intData bald 1 roadside:roadData
# road: fig1Data
# interior: scrubData

#check for normality, qqplot does correlation between data and normal line
ggqqplot(fig1Data$seedAbundance)
ggqqplot(scrubData$seedAbundance)
##both do not really show normal distribution
#Shapiro-Wilk normality test
shapiro.test(fig1Data$seedAbundance)
## gives small p value, shows that my data is sigificantly different than a normal distribution
##Visually show

scrubMeanSeed <-mean(scrubData$seedAbundance)
roadMeanSeed <- mean(fig1Data$seedAbundance)
##values: scrub: .51316, road:3.2721

seScrub <- standard_error(scrubData$seedAbundance)
seRoad <- standard_error(fig1Data$seedAbundance)

comparison<- data.frame(location=c("Scrub", "Road"), MeanSeeds=c(scrubMeanSeed, roadMeanSeed), standardError= c(seScrub, seRoad))

##computing standard error
seScrub <- standard_error(scrubData$seedAbundance)
seRoad <- standard_error(fig1Data$seedAbundance)


comparisonPlot <- ggplot(comparison, aes(x=location, y=MeanSeeds))+
  geom_col()+
  ggtitle("Mean Seed Abundance for Roadside and Scrub Interior")+xlab("Location")+ylab("Mean Seed Abundance")+
  geom_errorbar(aes(ymin = MeanSeeds-standardError, ymax = MeanSeeds+standardError), data = comparison, width = 0.2)+
  theme(text=element_text(size=16))

##exporting plot as jpg: 

ggsave("road vs scrub", plot=comparisonPlot, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)
  
tiff("R:/Aaron David - restricted/USDA/other projects/ABS sequencing/landscape ms/Mycologia/mycologia revision/fig2-dbRDA-varpart.tif",width = 165, height = 56,units = "mm", pointsize = 9,res=600)

#Make your figure

dev.off()

# running a t test to compare
t.test(rosemaryBalds$seedAbundance[rosemaryBalds$bald!="45"], fig1Data$seedAbundance)
## significant difference! note I excluded bald 45 and just looked at current populations in rosemary balds