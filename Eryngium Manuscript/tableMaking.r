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

#Road Model:
glmeNegBinomial<- glmer.nb(seedAbundance~patchDistance+ roadDistance + poly(roadDistance,2) +(1|transectNum), data=fig1Data)

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

models <- list(model_nb = model_nb)
model_summaries <- lapply(names(models), function(name) {
  extract_model_info(models[[name]], model_name = name, model_type = "GLMM")
})

final_table <- bind_rows(model_summaries)

# View the final combined table
print(final_table)
