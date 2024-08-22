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
# 
# glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + roadDistanceSq +(1|transectNum), data=fig1Data) #altered polynomial tern so it wouldn't give NAs in summary
glmeNegBinomial <- glmer.nb(seedAbundance~ roadDistance + patchDistance + +meanElevation_scaled+poly(roadDistance,2)+(1|transectNum), data=flwrFig1Data[flwrFig1Data$patchDistance<=120,])

#Scrub v Road model
control_params <- glmerControl(optimizer = "bobyqa", 
                               optCtrl = list(maxfun = 500000))
scrubVsRoad<- glmer.nb(seedAbundance~habitat + (1|site), data=combinedData, control = control_params)

#germination model
germinationModel <- glm(formula = cbind(total.germ, seeds-total.germ)~habitat, family = binomial, data=germinationData)

#Rosemary model (just scrub)
control <- glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e6))

nearFarModel <- glmer.nb(seedAbundance ~ nearFar + (1 | bald), offset = log(massActual), 
                         data = scrubData, control = control, verbose = FALSE)

#flowering heads and seeds
flwrElevationTest <- glmer.nb(seedAbundance ~ heads21_scaled + patchDistance_scaled + meanElevation_scaled+ roadDistance_scaled + (1 | transectNum), data = flwrFig1Data[flwrFig1Data$patchDistance<=130,], control = controls)

#seeds and seedlings


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


# Combine both model information into a final table
final_table <- bind_rows(model_road_info, model_scrub_info, model_near_far_info)

print(final_table)
view(final_table)

# Save the final table to the final figures directory
write.csv(final_table, "finalFigures/final_table.csv", row.names = FALSE)

#testing germination table

extract_germination_info <- function(model, model_name, test_statistic_type) {
  # Get the summary of the model
  summary_model <- summary(model)
  
  # Extract coefficients information from summary
  coefficients <- summary_model$coefficients
  
  # Create a data frame for coefficients
  model_info <- data.frame(
    variable = rownames(coefficients),
    estimate = coefficients[, "Estimate"],
    std_error = coefficients[, "Std. Error"],
    test_statistic = coefficients[, "z value"],
    p_value = coefficients[, "Pr(>|z|)"]
  )
  
  # Extract ANOVA information if available
  if ("Anova" %in% class(model)) {
    anova_model <- Anova(model, type = 3)
    
    # Extract ANOVA information and create a data frame
    anova_info <- data.frame(
      variable = rownames(anova_model),
      chi_squared = anova_model$Chisq,
      p_value_anova = anova_model$`Pr(>Chisq)`
    )
    
    # Add the test_statistic_type column
    anova_info <- mutate(anova_info, test_statistic_type = test_statistic_type)
    
    # Adjust variable names if necessary
    anova_info$variable <- ifelse(anova_info$variable == "habitat", "habitatscrub", anova_info$variable)
  } else {
    # If ANOVA information is not available, create an empty data frame
    anova_info <- data.frame(
      variable = character(0),
      chi_squared = numeric(0),
      p_value_anova = numeric(0),
      test_statistic_type = character(0)
    )
  }
  
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

germinationData$habitat <- as.factor(germinationData$habitat)
model_germ_info <- extract_germination_info(germinationModel, model_name = "Germination", test_statistic_type = "Type II Wald chisquare tests")
Anova(germinationModel)
anova_results <- Anova(germinationModel, type = 2)  # Use type = 2 for Type II tests
# Find the row in model_germ_info corresponding to 'habitat'

anova_results$`Pr(>Chisq)`
habitat_row <- which(model_germ_info$variable == "habitatscrub")
intercept_chisq <- anova_results$`LR Chisq`["(Intercept)"]

# Update chi_squared and p_value_anova with ANOVA results
model_germ_info$chi_squared[habitat_row] <- anova_results$`LR Chisq`["habitat"]
model_germ_info$p_value_anova[habitat_row] <- anova_results$`Pr(>Chisq)`["habitat"]

# Update test_statistic_type if needed
model_germ_info$test_statistic_type[habitat_row] <- "Type II tests"  # Adjust based on your test type
print(model_germ_info)
Anova(germinationModel)
summary(germinationModel)
chi.f<-function(x){round(unlist(lapply(x, FUN=function(x) x[1,1])),2)}
chi.f(anova_results)
