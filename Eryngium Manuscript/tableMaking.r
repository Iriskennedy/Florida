#making tables for models
#started june 24, 2024

library(tidyverse)
library(lme4)
library(car)

#load data frames for models
load("cleanData/germinationData.csv")
load("dataFrames/combinedData.RData")
load("dataFrames/flwrFig1Data.RData")
fig1Data <- read_csv("cleanData/roadData.csv")
scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45)

fig1Data$roadDistanceSq <- (fig1Data$roadDistance)*(fig1Data$roadDistance)

#Road Model:
# glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + poly(roadDistance,2) +(1|transectNum), data=fig1Data)

glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + roadDistanceSq +(1|transectNum), data=fig1Data) #altered polynomial tern so it wouldn't give NAs in summary

#Scrub model
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

extract_model_info <- function(model, model_name, model_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Extract coefficients information
  coefficients <- summary_model$coefficients
  
  # Extract relevant columns and convert to data frame
  model_info <- data.frame(
    modelName = model_name,
    type = model_type,
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  return(model_info)
}

model_road_info <- extract_model_info(glmeNegBinomial, model_name = "roadside", model_type = "GLMM")

model_road_scrub_info <- extract_model_info(scrubVsRoad, model_name = "Scrub vs Road", model_type = "GLMM")

final_table <- bind_rows(model_road_info, model_road_scrub_info)
# View the table
print(final_table)

# View the final combined table
print(final_table)

#TESTING function that incorporates Anova and summary

# Function to extract model information
extract_model_info <- function(model, model_name, model_type, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    modelName = model_name,
    type = model_type,
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  # Extract ANOVA information and create a data frame
  anova_info <- data.frame(
    variable = rownames(anova_model),
    test_statistic_value = anova_model[, "Chisq"],  # Adjust based on Anova output
    p_value_anova = anova_model[, "Pr(>Chisq)"]  # Adjust based on Anova output
  )
  
  # Add the test_statistic_type column
  anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
  
  # Merge both data frames on the variable name
  final_info <- left_join(model_info, anova_info, by = "variable")
  
  return(final_info)
}

model_road_info <- extract_model_info(glmeNegBinomial, model_name = "roadside", model_type = "GLMM", test_statistic_type="Type II Wald chisquare tests")

model_road_scrub_info <- extract_model_info(scrubVsRoad, model_name = "Scrub vs Road", model_type = "GLMM")

final_table <- bind_rows(model_road_info, model_road_scrub_info)
# View the table
print(final_table)
view(final_table)

#test 3

glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + roadDistanceSq +(1|transectNum), data=fig1Data) #altered polynomial tern so it wouldn't give NAs in summary

#Scrub model
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

extract_model_info <- function(model, model_name, model_type, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    modelName = model_name,
    type = model_type,
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  # Extract ANOVA information and create a data frame
  anova_info <- data.frame(
    variable = rownames(anova_model),
    test_statistic_value_anova = anova_model[, "Chisq"],  # Adjust based on Anova output
    p_value_anova = anova_model[, "Pr(>Chisq)"]  # Adjust based on Anova output
  )
  
  # Add the test_statistic_type column
  anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
  
  # Merge both data frames on the variable name
  final_info <- left_join(model_info, anova_info, by = "variable")
  
  return(final_info)
}

# Extract information for both models with manually specified test statistic type
model_road_info <- extract_model_info(glmeNegBinomial, model_name = "Roadside", model_type = "GLMM", test_statistic_type = "Type II Wald chisquare tests")
model_scrub_info <- extract_model_info(scrubVsRoad, model_name = "Scrub vs Road", model_type = "GLMM", test_statistic_type = "Type II Wald chisquare tests")

#adding correct p value
scrubRoadAnova <- Anova(scrubVsRoad)
scrubRoadAnova$`Pr(>Chisq)`
scrubRoadAnova$Chisq

# Combine both model information into a final table
final_table <- bind_rows(model_road_info, model_scrub_info)

# View the final combined table
print(final_table)
view(final_table)

##test 4 trying to fix it- WORKS USE THIS VERSION
library(lme4)
library(car)
library(dplyr)
library(broom.mixed)

# Function to extract model information
extract_model_info <- function(model, model_name, model_type, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    modelName = model_name,
    type = model_type,
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  # Print model_info for debugging
  print(model_info)
  
  # Extract ANOVA information and create a data frame
  anova_info <- data.frame(
    variable = rownames(anova_model),
    test_statistic_value_anova = anova_model$Chisq,  # Adjust based on Anova output
    p_value_anova = anova_model$`Pr(>Chisq)`  # Adjust based on Anova output
  )
  
  # Add the test_statistic_type column
  anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
  
  # Print anova_info for debugging
  print(anova_info)
  
  # Adjust the variable names in anova_info to match the model_info
  anova_info$variable <- ifelse(anova_info$variable == "habitat", "habitatscrub", anova_info$variable)
  
  # Print adjusted anova_info for debugging
  print(anova_info)
  
  # Merge both data frames on the variable name
  final_info <- left_join(model_info, anova_info, by = "variable")
  
  # Print final_info for debugging
  print(final_info)
  
  return(final_info)
}

# Fit the models
glmeNegBinomial <- glmer.nb(seedAbundance ~ patchDistance + roadDistance + roadDistanceSq + (1|transectNum), data = fig1Data)
scrubVsRoad <- glmer.nb(seedAbundance ~ habitat + (1|site), data = combinedData, control = glmerControl(optimizer = "bobyqa"))

# Extract information for both models with manually specified test statistic type
model_road_info <- extract_model_info(glmeNegBinomial, model_name = "Roadside", model_type = "GLMM", test_statistic_type = "Type II Wald chisquare tests")
model_scrub_info <- extract_model_info(scrubVsRoad, model_name = "Scrub vs Road", model_type = "GLMM", test_statistic_type = "Type II Wald chisquare tests")

# Combine both model information into a final table
final_table <- bind_rows(model_road_info, model_scrub_info)

print(final_table)
view(final_table)

