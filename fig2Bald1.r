## Figure 2 Bald 1
## Figure 2 Bald 1
#started jan 13
library(psych) #this was supposed to get an se function but failed?

# create fig2 data frame with only road distance and seed abundance

fig2data <- data.frame(x = fig1Data$roadDistance, y = mean(fig1Data$seedAbundance))

#create standard error function
standard_error <- function(x) sd(x)/sqrt(length(x))

df.summary <- fig1Data %>%
  group_by(roadDistance) %>%
  summarise(
    se = standard_error(seedAbundance),
    seedAbundance = mean(seedAbundance)
  )
df.summary

#df.summary now has three columns with roadDistance, se, and mean seedAbundance

#make column plot:
seedRoadPlot <- ggplot(df.summary, aes(roadDistance, seedAbundance))+
  geom_col(data = df.summary, fill = 'lightgrey', color = "black") +
  geom_errorbar( aes(ymin = seedAbundance-se, ymax = seedAbundance+se), data = df.summary, width = 0.2 )+
  ggtitle("Mean Seed Abundance Along Scrub to Road Gradient")+ xlab("Distance From Scrub to Road (m)")+ylab("Mean Seed Abundance")

ggsave("Seed Abundance Scrub to Road", plot=seedRoadPlot, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)

#how to wrap title (actually do not need to here)
#(wrapper(my_title, width = 20))




