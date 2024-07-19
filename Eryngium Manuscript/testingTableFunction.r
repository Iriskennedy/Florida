#table making final

library(tidyverse)
library(lme4)
library(car)

#load data frames for models
# germinationData <- read_csv("~/Documents/GitHub/Florida/Eryngium Manuscript/cleanData/germinationData.csv")
load("cleanData/germinationData.RData")
load("dataFrames/combinedData.RData")
load("dataFrames/flwrFig1Data.RData")
fig1Data <- read_csv("cleanData/roadData.csv")
scrubData <- read_csv("rosemaryBaldsCombined.csv")
scrubData<- subset(scrubData, bald != 45)

fig1Data$roadDistanceSq <- (fig1Data$roadDistance)*(fig1Data$roadDistance)

#MODELS:

#Road Model:
# glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + poly(roadDistance,2) +(1|transectNum), data=fig1Data)

glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + roadDistanceSq +(1|transectNum), data=fig1Data) #altered polynomial tern so it wouldn't give NAs in summary

#Scrub v Road model
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

#germination model
germinationModel <- glm(formula = cbind(total.germ, seeds-total.germ)~habitat, family = binomial, data=germinationData)

#Rosemary model (just scrub)
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6))

nearFarModel <- glmer.nb(seedAbundance ~ nearFar + (1 | bald), offset = log(massActual), 
                         data = scrubData, control = control, verbose = FALSE)

#flowering heads and seeds
flwrModel2 <- glmer.nb(seedAbundance ~ heads21 + patchDistance + roadDistance + (1 | uid), data = flwrFig1Data, control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 10000)))

#seeds and seedlings
seedSeedling <- lmer(log(seedlings22+1)~ scale(patchDistance) + poly(roadDistance,2) + poly(seedAbundance, 2)+ log(heads21+1)+ (1|transectNum), data = flwrFig1Data)

#table making function:
extract_model_info <- function(model, model_name, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  # Extract ANOVA information and create a data frame
  anova_info <- data.frame(
    variable = rownames(anova_model),
    chi_squared = anova_model$Chisq,  # Adjust based on Anova output
    p_value_anova = anova_model$`Pr(>Chisq)`  # Adjust based on Anova output
  )
  
  # Add the test_statistic_type column
  anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
  
  # Adjust the variable names in anova_info to match the model_info
  #add new lines here for every model with every variable name possible in that model
  anova_info$variable <- ifelse(anova_info$variable == "habitat", "habitatscrub", anova_info$variable)
  anova_info$variable <- ifelse(anova_info$variable == "nearFar", "nearFarN", anova_info$variable)
  anova_info$variable <- ifelse(anova_info$variable == "habitat", anova_info$variable)
  
  # Merge both data frames on the variable name
  final_info <- left_join(model_info, anova_info, by = "variable")
  
  # Create a row with the model name
  model_name_row <- data.frame(
    variable = model_name,
    estimate = NA,
    std_error = NA,
    test_statistic = NA,
    p_value = NA,
    chi_squared = NA,
    p_value_anova = NA,
    test_statistic_type = NA
  )
  # Combine the model name row and the model information
  final_info <- bind_rows(model_name_row, final_info)
  
  return(final_info)
}

# Fit the models
glmeNegBinomial <- glmer.nb(seedAbundance ~ patchDistance + roadDistance + roadDistanceSq + (1|transectNum), data = fig1Data)
scrubVsRoad <- glmer.nb(seedAbundance ~ habitat + (1|site), data = combinedData, control = glmerControl(optimizer = "bobyqa"))

# Extract information for both models with manually specified test statistic type
model_road_info <- extract_model_info(glmeNegBinomial, model_name = "Roadside", test_statistic_type = "Type II Wald chisquare tests")
model_scrub_info <- extract_model_info(scrubVsRoad, model_name = "Scrub vs Road", test_statistic_type = "Type II Wald chisquare tests")
model_near_far_info <- extract_model_info(nearFarModel, model_name = "Near vs Far", test_statistic_type = "Type II Wald chisquare tests")
model_germ_info <- extract_model_info(germinationModel, model_name = "Germination", test_statistic_type = "Type II Wald chisquare tests")


# Combine both model information into a final table
final_table <- bind_rows(model_road_info, model_scrub_info)

print(final_table)
view(final_table)

# Save the final table to the final figures directory
write.csv(final_table, "finalFigures/final_table.csv", row.names = FALSE)

#debugging germination table
library(lme4)
library(car)
library(dplyr)

# Define the function to extract model information
extract_model_info <- function(model, model_name, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],  # For GLMMs, use "t value" for LMMs
    p_value = coefficients[, "Pr(>|z|)"]  # For GLMMs, use "Pr(>|t|)" for LMMs
  )
  
  # Get the ANOVA table
  anova_model <- Anova(model, type = 3)
  
  # Print ANOVA table for debugging
  print("ANOVA Model:")
  print(anova_model)
  
  # Check if the ANOVA table has rows
  if (nrow(anova_model) > 0) {
    # Extract ANOVA information and create a data frame
    anova_info <- data.frame(
      variable = rownames(anova_model),
      chi_squared = anova_model$Chisq,  # Adjust based on Anova output
      p_value_anova = anova_model$`Pr(>Chisq)`  # Adjust based on Anova output
    )
    
    # Add the test_statistic_type column
    anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
    
    # Adjust the variable names in anova_info to match the model_info
    anova_info$variable <- ifelse(anova_info$variable == "habitat", "habitatscrub", anova_info$variable)
    anova_info$variable <- ifelse(anova_info$variable == "nearFar", "nearFarN", anova_info$variable)
    
    # Print adjusted anova_info for debugging
    print("ANOVA Info:")
    print(anova_info)
    
    # Merge both data frames on the variable name
    final_info <- left_join(model_info, anova_info, by = "variable")
  } else {
    # If the ANOVA table has no rows, create an empty anova_info data frame with the same structure
    anova_info <- data.frame(
      variable = character(0),
      chi_squared = numeric(0),
      p_value_anova = numeric(0),
      test_statistic_type = character(0)
    )
    final_info <- model_info
  }
  
  # Print final_info for debugging
  print("Final Info:")
  print(final_info)
  
  # Create a row with the model name
  model_name_row <- data.frame(
    variable = model_name,
    estimate = NA,
    std_error = NA,
    test_statistic = NA,
    p_value = NA,
    chi_squared = NA,
    p_value_anova = NA,
    test_statistic_type = NA
  )
  
  # Combine the model name row and the model information
  final_info <- bind_rows(model_name_row, final_info)
  
  return(final_info)
}

model_germ_info <- extract_model_info(germinationModel, model_name = "Germination", test_statistic_type = "Type II Wald chisquare tests")
