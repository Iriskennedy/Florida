#making the flowering heads vs seeds graphs look better

library(tidyverse)
#load("yourdirectory/someworkspace.RData")
load("dataFrames/flwrFig1Data.RData")

#trying to make it smoother
# Load necessary packages
library(tidyverse)
library(lme4)
library(MASS)
load("~/Documents/GitHub/Florida/Eryngium Manuscript/cleanData/flwrFig1Data.RData")
# Check for missing values and remove rows with missing values
flwrSubset <- flwrFig1Data%>%
  filter(heads21<400)%>%
  dplyr::select(heads21, seedAbundance, seedlings22, patchDistance, roadDistance, uid)

flwrFig1Data_complete <- na.omit(flwrSubset)

# Ensure 'uid' is unique in both data frames
sum(duplicated(flwrFig1Data$uid))
sum(duplicated(flwrFig1Data_complete$uid))

# Fit the model with complete data
flwrModel2 <- glmer.nb(seedAbundance ~ heads21 + patchDistance + roadDistance + (1 | uid), 
                       data = flwrFig1Data_complete, 
                       control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

# Create a new dataset with a finer grid of heads21 values
new_data <- expand.grid(
  heads21 = seq(min(flwrFig1Data$heads21, na.rm = TRUE), max(flwrFig1Data$heads21, na.rm = TRUE), length.out = 100),
  patchDistance = unique(flwrFig1Data$patchDistance), 
  roadDistance = unique(flwrFig1Data$roadDistance), 
  uid = unique(flwrFig1Data$uid)
)

# Predict the values and confidence intervals for the new dataset
new_data$predicted <- predict(flwrModel2, newdata = new_data, type = "response", re.form = NA)
new_data$se.fit <- predict(flwrModel2, newdata = new_data, type = "link", se.fit = TRUE, re.form = NA)$se.fit

# Calculate confidence intervals
new_data$ci.lower <- exp(predict(flwrModel2, newdata = new_data, type = "link", re.form = NA) - 1.96 * new_data$se.fit)
new_data$ci.upper <- exp(predict(flwrModel2, newdata = new_data, type = "link", re.form = NA) + 1.96 * new_data$se.fit)

# Plot the original data with the model predictions and smoother confidence intervals
# Assuming flwrFig1Data is your original dataset with observed data
# Assuming new_data contains predictions and confidence intervals

# Plot the original data with the model predictions and smoother confidence intervals
flwrAbundanceMerged <- ggplot(flwrFig1Data, aes(x = heads21, y = seedAbundance)) +
  geom_point() +
  geom_line(data = new_data, aes(x = heads21, y = predicted), color = "blue", na.rm = TRUE) +
  geom_ribbon(data = new_data, aes(x = heads21, ymin = ci.lower, ymax = ci.upper), alpha = 0.2, fill = "blue", na.rm = TRUE) +
  ggtitle("Seed Abundance vs Number of Flowering Heads in 2021") +
  xlab("Number of Flowering Heads") +
  ylab("Seed Abundance") +
  ylim(0, 50) +
  xlim(0, 150)  # Exclude the outlier

# Display the plot
print(flwrAbundanceMerged)

# TEST
# Step 1: Create new_data without seedAbundance
new_data <- expand.grid(
  heads21 = seq(min(flwrFig1Data$heads21, na.rm = TRUE), max(flwrFig1Data$heads21, na.rm = TRUE), length.out = 100),
  patchDistance = unique(flwrFig1Data$patchDistance), 
  roadDistance = unique(flwrFig1Data$roadDistance), 
  uid = unique(flwrFig1Data$uid)
)

# Step 2: Predict seedAbundance for new_data
new_data$predicted <- predict(flwrModel2, newdata = new_data, type = "response", re.form = NA)

# Calculate confidence intervals (assuming flwrModel2 is a glmer model)
new_data$se.fit <- predict(flwrModel2, newdata = new_data, type = "link", se.fit = TRUE, re.form = NA)$se.fit
new_data$ci.lower <- exp(new_data$predicted - 1.96 * new_data$se.fit)
new_data$ci.upper <- exp(new_data$predicted + 1.96 * new_data$se.fit)

# Step 3: Plot using ggplot
flwrAbundanceMerged <- ggplot(flwrFig1Data, aes(x = heads21, y = seedAbundance)) +
  geom_point() +
  geom_line(data = new_data, aes(x = heads21, y = predicted), color = "blue", na.rm = TRUE) +
  geom_ribbon(data = new_data, aes(x = heads21, ymin = ci.lower, ymax = ci.upper), alpha = 0.2, fill = "blue", na.rm = TRUE) +
  ggtitle("Seed Abundance vs Number of Flowering Heads in 2021") +
  xlab("Number of Flowering Heads") +
  ylab("Seed Abundance") +
  ylim(0, 50) +
  xlim(0, 150)  # Exclude the outlier

# Display the plot
print(flwrAbundanceMerged)


# Display the plot
print(flwrAbundanceMerged)
#taking the outlier of 400 flowering heads out to better see trend
noOutlier <- subset(flwrFig1Data, heads21 <= 400)

flwrAbundance <- ggplot(noOutlier, aes(x=noOutlier$heads21, y=noOutlier$seedAbundance))+
  geom_point()+
#  geom_smooth(method = "glm", se=TRUE, color="black", formula = y ~ x) +
 # ggtitle("Seed Abundance vs Number of Flowering Heads in 2021")+
  xlab("Number of Flowering Heads in Previous Year")+ylab("Seed Bank Density (Seed Abundance)")

#saving with removed trendline and no title no outlier:
save_directory <- "graphs" #saving graph object
saveRDS(object = flwrAbundance, file.path(save_directory, "flwrAbundance.rds"))

flwrModel <- glm(seedAbundance~heads21+patchDistance+roadDistance, data = flwrFig1Data, family=poisson())
summary(flwrModel) #second lowest AIC so far
Anova(flwrModel)

flwrModel3 <- glm(seedAbundance~heads21+patchDistance+roadDistance, data = flwrFig1Data, family=poisson(), offset = log(massFinal)) #adding log offset
summary(flwrModel3)

#trying it with a negative binomial but it doesn't work, may be colinearity between patchDistance and transectNum
flwrModel2 <- glmer.nb(seedAbundance ~ heads21 + patchDistance + roadDistance + (1 | uid), data = flwrFig1Data, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))
summary(flwrModel2) #new lowest AIC actually

#omit missing data:
#get just coloms of interest
flwrSubset <- flwrFig1Data%>%
  filter(heads21<400)%>%
  dplyr::select(heads21, seedAbundance, seedlings22, patchDistance, roadDistance, uid)
flwrFig1Data_complete <- na.omit(flwrSubset)
flwrModelComplete <- glmer.nb(seedAbundance ~ heads21 + patchDistance + roadDistance + (1 | uid), 
                              data = flwrFig1Data_complete, 
                              control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

# Generate predictions
flwrFig1Data_complete$predicted <- predict(flwrModelComplete, type = "response", re.form = NA)
flwrFig1Data_complete$se.fit <- predict(flwrModelComplete, type = "link", se.fit = TRUE, re.form = NA)$se.fit

# Calculate confidence intervals
flwrFig1Data_complete$ci.lower <- exp(predict(flwrModelComplete, type = "link", re.form = NA) - 1.96 * flwrFig1Data_complete$se.fit)
flwrFig1Data_complete$ci.upper <- exp(predict(flwrModelComplete, type = "link", re.form = NA) + 1.96 * flwrFig1Data_complete$se.fit)

# Plot the data with the model predictions and confidence intervals
flwrAbundancePredicted <- ggplot(flwrFig1Data_complete, aes(x = heads21, y = seedAbundance)) +
  geom_point() +
  geom_line(aes(y = predicted), color = "blue") +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), alpha = 0.2, fill = "blue") +
  ggtitle("Seed Abundance vs Number of Flowering Heads in 2021") +
  xlab("Number of Flowering Heads") +
  ylab("Seed Abundance")

#merging to plot predicted data on top of real data
# Merge the predictions with the original dataset
flwrFig1Data <- flwrFig1Data %>%
  left_join(flwrFig1Data_complete %>%
              dplyr::select(uid, predicted, se.fit, ci.lower, ci.upper), by = "uid")

# Plot the original data with the model predictions and confidence intervals
flwrAbundanceMerged <- ggplot(flwrFig1Data, aes(x = heads21, y = seedAbundance)) +
  geom_point() +
  geom_line(aes(y = predicted.y), color = "blue", na.rm = TRUE) +
  geom_ribbon(aes(ymin = ci.lower.y, ymax = ci.upper.y), alpha = 0.2, fill = "blue", na.rm = TRUE) +
  ggtitle("Seed Abundance vs Number of Flowering Heads in 2021") +
  xlab("Number of Flowering Heads") +
  ylab("Seed Abundance")+
  ylim(0,50)+
  xlim(0,150) #this just excludes the weird outlier 

print(flwrAbundanceMerged)

save_directory <- "graphs" #saving graph object
saveRDS(object = flwrAbundance, file.path(save_directory, "flwrAbundance.rds"))
