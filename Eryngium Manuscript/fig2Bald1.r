## Figure 2 Bald 1
## Road: Plotting seeds vs. distance to the road
#started jan 13
library(psych) #this was supposed to get an se function but failed?
library(tidyverse)
library(ggplot2)

# for some reason I am just calling the road data fig 1 data
fig1Data <- read_csv("C:\\Users\\irisa\\Documents\\Archbold\\Intern Project\\bald1RoadsideRaw.csv")

#making unique ids for every bag of sand
fig1Data$uid <- paste(fig1Data$patchDistance, fig1Data$roadDistance, sep = "_")
#check for duplicates
# Check for duplicates in the 'uid' column
duplicate_rows <- fig1Data[duplicated(fig1Data$uid), ]

# Print the duplicate rows, if any
print(duplicate_rows)
#filter out duplicated rows
fig1Data <- subset(fig1Data, patchDistance!=130)

# create fig2 data frame with only road distance and seed abundance

fig2data <- data.frame(x = fig1Data$roadDistance, y = mean(fig1Data$seedAbundance))

#create standard error function
standard_error <- function(x) sd(x)/sqrt(length(x))

# df.summary <- fig1Data %>%
#   group_by(roadDistance) %>%
#   summarise(
#     se = standard_error(seedAbundance),
#     seedAbundance = mean(seedAbundance)
#   )
# df.summary

df.summary <- fig1Data %>%
  group_by(roadDistance) %>%
  summarise(
    se = standard_error(abundanceMass),
    abundanceMass = mean(abundanceMass)
  )
df.summary

#df.summary now has three columns with roadDistance, se, and mean seedAbundance

#make column plot:
seedRoadPlot <- ggplot(df.summary, aes(roadDistance, abundanceMass))+
  geom_col(data = df.summary, fill = 'lightgrey', color = "black") +
  geom_errorbar( aes(ymin = abundanceMass-se, ymax = abundanceMass+se), data = df.summary, width = 0.2 )+
  ggtitle("Mean Seed Abundance Along Scrub to Road Gradient")+ xlab("Distance From Scrub to Road (m)")+ylab("Mean Seed Abundance/ Mass of Sand (seeds/gram)")

ggsave("Seed Abundance Scrub to Road", plot=seedRoadPlot, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)

#how to wrap title (actually do not need to here)
#(wrapper(my_title, width = 20))




