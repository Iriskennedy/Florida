##creating the 4 panel figure

library(tidyverse)
library(patchwork)

#create graph objects like this: 
# save_directory <- "graphs"
# saveRDS(object = roadsideOverviewPlot, file.path(save_directory, "roadsideOverviewPlot.rds"))

# Load the graph objects

flwrAbundance <- readRDS(file.path("graphs", "flwrAbundance.rds"))
abundanceSeedlings <- readRDS(file.path("graphs", "abundanceSeedlings.rds"))
seedlingsRoad <- readRDS(file.path("graphs", "seedlingsRoad.rds"))
seedRoadPlot <- readRDS(file.path("graphs", "seedRoadPlot.rds"))


#make combined plot
combined_plot <- (seedRoadPlot | seedlingsRoad) / 
  (flwrAbundance | abundanceSeedlings)

# Display the combined plot
print(combined_plot)

