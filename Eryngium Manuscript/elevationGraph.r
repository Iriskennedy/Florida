#making the elevation figure
#started august 22
library(lme4)
library(car)
library(tidyverse)

load("dataFrames/flwrFig1Data.RData")

# Rescale predictor variables
flwrFig1Data$heads21_scaled <- scale(flwrFig1Data$heads21)
flwrFig1Data$patchDistance_scaled <- scale(flwrFig1Data$patchDistance)
flwrFig1Data$roadDistance_scaled <- scale(flwrFig1Data$roadDistance)
flwrFig1Data$meanElevation_scaled <- scale(flwrFig1Data$meanElevation)

elevationPlot <- ggplot(data = flwrFig1Data)+
  geom_line(mapping = aes(x=patchDistance, y=meanElevation), linewidth=.75)+
  xlab("Distance Along the Road (m)")+
  ylab("Mean Elevation (m)")+
  ggtitle("Roadside Elevation")

save_directory <- "finalFigures"
saveRDS(object = elevationPlot, file.path(save_directory, "elevationPlot.rds"))
