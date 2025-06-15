#cleaning up the table
library(tidyverse)
modelTable <- read_csv("C:\\Users\\irisa\\Downloads\\Model Table - Sheet1.csv")

#pasting standard error into estimate column, 3 indicates to three decimal places
modelTable1<- modelTable%>%
  mutate(estimate_se = ifelse(!is.na(estimate) & !is.na(std_error),
                              paste0(round(estimate, 3), " ± ", round(std_error, 3)),
                              NA))

#selecting just the columns I want in the order that I want them
newTable <- modelTable1 %>%
 dplyr:: select(variable, estimate_se, test_statistic_z, p_value_anova, test_statistic_type)
view(newTable)

write.csv(newTable, "newTable.csv", row.names = FALSE)
