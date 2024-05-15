##making dataframe with all rosemary balds in one data frame
library(readxl)
library(tidyverse)

#specify file path/ set working directory
setwd("C:\\Users\\irisa\\Documents//Archbold\\Intern Project")
getwd()

excel_sheets("C:\\Users\\irisa\\Documents//Archbold\\Intern Project\\rosemaryBalds.xlsx")
#tryinh to combine, map applys function to each list item
excel_sheets("C:\\Users\\irisa\\Documents//Archbold\\Intern Project\\rosemaryBalds.xlsx")%>%
  map_df(~read_xlsx("C:\\Users\\irisa\\Documents//Archbold\\Intern Project\\rosemaryBalds.xlsx",.))


#apply sheet names to data frame names
data_frame=lapply(setNames(sheet,sheet), function(x) read_excel("rosemaryBalds.xlsx", sheet=x))

print(data_frame)

##success! data frame is now a combo of all those xl sheets

##wanted to export combined but this looks disastrous... will fix later if needed
write.csv(data_frame, "C:\\Users\\irisa\\Documents//Archbold\\Intern Project\\rosemaryBaldsCombined.csv", row.names=FALSE)

##trying some summary plots
library(ggplot2)

totalSeedAbundancePlot <- ggplot(data_frame, aes(x=roadDistance, y=seedAbundance))+
  geom_col()
