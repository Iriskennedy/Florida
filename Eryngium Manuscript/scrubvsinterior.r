##comparing interior and exterior
# started 1/27/23

library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr) #for normality tests
library(lme4)
library(car)
library(MASS)#this masks dplyr
library(emmeans)
#1 load data rosemary: intData bald 1 roadside:roadData
# road: fig1Data
# interior: scrubData

fig1Data <- read_csv("cleanData/roadData.csv")
scrubData <- read_csv("C:\\Users\\irisa\\Documents\\Archbold\\Eryngium Manuscript\\rosemaryBaldsCombined.csv") #rosemary bald data
nrow(fig1Data)

#check for normality, qqplot does correlation between data and normal line
ggqqplot(fig1Data$abundanceMass)
ggqqplot(scrubData$seedAbundance)
##both do not really show normal distribution
#Shapiro-Wilk normality test
shapiro.test(fig1Data$seedAbundance)
shapiro.test(fig1Data$abundanceMass)
shapiro.test(fig1Data$logAbundanceMass)
subset1 <- subset(fig1Data,logAbundanceMass!=-Inf )
shapiro.test(subset1$abundanceMass) #log transformed seed abundance is still not normally distributed

#create a unique id (uid) for each road record so that each record is a unique bag of sand which will be the random effect. Create another column because names have to be the same
fig1Data$site <- paste(fig1Data$patchDistance, fig1Data$roadDistance, sep = "_")
scrubData$site <- paste(scrubData$transectNum, scrubData$nearFar, sep = "_")#this serves as a unique id for every unique bag of sand for the scrub sites
scrubData$patchDistance <- NA
scrubData$roadDistance <- NA

test <- subset(fig1Data, patchDistance==130)

#add habitat column to add that as a predictor in model, add bald column to fig1Data so you can use that in your model
fig1Data$habitat <- "road"
scrubData$habitat <- "scrub"

fig1Data$bald <- 1

## gives small p value, shows that my data is sigificantly different than a normal distribution

#changing from using seed abundance to using abundance per mass
scrubMeanSeed <-mean(scrubData$abundanceMass)
roadMeanSeed <- mean(fig1Data$abundanceMass)
##values: scrub: .51316, road:3.2721

#creating & applying standard error function
standard_error <- function(x) sd(x)/sqrt(length(x))

seScrub <- standard_error(scrubData$abundanceMass)
seScrubMean <- standard_error(mean(scrubData$abundanceMass))
seRoad <- standard_error(fig1Data$abundanceMass)

comparison<- data.frame(location=c("Scrub", "Road"), MeanSeeds=c(scrubMeanSeed, roadMeanSeed), standardError= c(seScrub, seRoad))

comparisonPlot <- ggplot(comparison, aes(x=location, y=MeanSeeds))+
  geom_col()+
  ggtitle("Mean Seed Abundance for Roadside and Scrub Interior")+xlab("Location")+ylab("Mean Seed Abundance/Mass (seed/gram) ")+
  geom_errorbar(aes(ymin = MeanSeeds-standardError, ymax = MeanSeeds+standardError), data = comparison, width = 0.2)+
  theme(text=element_text(size=16))

#running statistics, using a linear mixed effects model
# first select the columbs you need then combine data sets 
##do this if dplyr is masked
scrubDataSubset <- scrubData %>%
  dplyr::select(abundanceMass, habitat, site, seedAbundance, massBag,massActual, patchDistance, roadDistance, bald)%>%
  dplyr::rename(massFinal = massActual) #making column names match

roadDataSubset <- fig1Data%>%
  dplyr::select(abundanceMass, habitat, site, seedAbundance, massBag,massFinal, patchDistance, roadDistance, bald)

combinedData <- rbind(roadDataSubset, scrubDataSubset)
combinedData$habitat <- factor(combinedData$habitat)

# save(combinedData, file="dataFrames/combinedData.RData")

## create the linear mixed effects model
##try as glmer poisson
scrubRoadModel <- glmer(seedAbundance~habitat + (1|site), data=combinedData, family = poisson, offset = log(massFinal))
summary(scrubRoadModel)

#trying as negative binomial
control_params <- glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 500000))
## log offset poisson model, lower BIC than doing it with neg bin
modelPoisson<- glmer(seedAbundance~habitat +(1|site), data=combinedData, offset = log(massFinal), family= poisson)
summary(modelPoisson)

boxplot(seedAbundance~as.factor(habitat) ,data=combinedData)
table(combinedData$habitat)
summary(modelPoisson)

#use negative binomial model to deal with underdispersion

glmeNegBinomial<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params) #lowest AIC, but may be too low- multicolinearity? BEST MODEL
model_nb <- glmer.nb(seedAbundance ~ habitat + (1 |bald/site), data = combinedData, offset = log(massFinal), control = control_params) #this model is singlular
isSingular(model_nb)
isSingular(glmeNegBinomial)


summary(glmeNegBinomial)
mAnova <- Anova(glmeNegBinomial)
mAnova$`Pr(>Chisq)`

#doing emmeans
# Obtain estimated marginal means (EMMs) on the response scale
em_means <- emmeans(glmeNegBinomial, ~ habitat, type = "response")
# Extract back-transformed means and their standard errors
summary_em_means <- summary(em_means, infer = c(TRUE, TRUE)) #infer includes conifidence and p values
backtransformed_means <- summary_em_means$response #0.33916084 0.03283379
standard_errors <- summary_em_means$SE

#recreate plot with emmeans
# Create a data frame for plotting
levels_order <- summary_em_means$habitat

comparison <- data.frame(
  location = levels_order,
  MeanSeeds = backtransformed_means,
  standardError = standard_errors
)

# Create the plot
comparisonPlot <- ggplot(comparison, aes(x = location, y = MeanSeeds)) +
  geom_col() +
  ggtitle("Mean Seed Abundance for Roadside and Scrub Interior") +
  xlab("Habitat") +
  ylab("Mean Seed Abundance/Mass (seed/gram)") +
  geom_errorbar(aes(ymin = MeanSeeds - standardError, ymax = MeanSeeds + standardError), width = 0.2) +
  theme(text = element_text(size = 16))

#checking dispersion: (actually I don't think you do this for negative binomial)
# Extract fitted values from the model
fitted_values <- fitted(model_nb)

# Calculate dispersion statistic (variance to mean ratio)
dispersion_stat <- var(dev_resid) / mean(fitted_values)

#goal: combine into one data frame  go for something like this:
##model <- lmer(abundanceMass ~ transect + distance_from_road + site + (1 | group), data = combinedData)
  

##exporting plot as jpg: 
saveRDS(comparisonPlot, file = "graphs/scrubRoadPlot.rds")
ggsave("road vs scrub", plot=comparisonPlot, path = "C:\\Users\\irisa\\Documents\\Archbold\\Intern Project", width = 8, height = 6, device='jpg', dpi=600)
  
# tiff("R:/Aaron David - restricted/USDA/other projects/ABS sequencing/landscape ms/Mycologia/mycologia revision/fig2-dbRDA-varpart.tif",width = 165, height = 56,units = "mm", pointsize = 9,res=600)
# 
# #Make your figure
# 
# dev.off()

# checking for over and under dispersion:
# Extract deviance residuals
fitted_values <- fitted(modelPoisson)
dev_resid <- residuals(modelPoisson, type = "deviance")

# Calculate dispersion statistic (variance to mean ratio)
dispersion_stat <- var(dev_resid) / mean(fitted_values)

# Compare dispersion statistic to expected value (1 for Poisson distribution)
if (dispersion_stat > 1) {
  print("Model exhibits overdispersion")
} else if (dispersion_stat < 1) {
  print("Model exhibits underdispersion")
} else {
  print("Model has expected dispersion")
}

overdisp_fun(modelPoisson)

#testing pearson and deviance goodness of fit tests:
# Pearson's chi-square test for overdispersion
library(AER)
library(MASS)
pearson_test <- MASS::dispersiontest(modelPoisson, data = combinedData, type = "pearson")
print(pearson_test)

# Deviance goodness-of-fit test
deviance_test <- dispersiontest(modelPoisson, data = combinedData, type = "deviance")
print(deviance_test)

#trying a generalized poisson: 
# Fit the generalized Poisson model with log-transformed seedAbundance
model_gpoisson <- glm(log(seedAbundance) ~ habitat, family = genpoisson, data = combinedData, trace = TRUE, crit = "coef")

# Summary of the model
summary(model_gpoisson)


# running a t test to compare
t.test(scrubData$seedAbundance[scrubData$bald!="45"], fig1Data$seedAbundance)
## significant difference! note I excluded bald 45 and just looked at current populations in rosemary balds