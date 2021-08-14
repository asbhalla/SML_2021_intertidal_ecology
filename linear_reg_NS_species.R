library(readr)
sml_pc_tdht <- read_csv("FuBa/sml_pc_tdht.csv")
View(sml_pc_tdht)

library(tidyverse)
#what every col parsed as
glimpse(sml_pc_tdht)
#to reassign cols parse_<chr/num/dbl>

#filter data to yrs post-'85
pc_filtered <- filter(sml_pc_tdht, Year>=1985)
#to check if yrs filtered
unique(pc_filtered$Year)
#or use Boolean query, if done incorrectly
#TRUE/FALSE
unique(pc_filtered$Year>=1985)

#filter for one species
pc_asco <- filter(pc_filtered, 
Organism =="Ascophyllum nodosum (canopy)")
#check whether A. nodosum has been filtered
unique(pc_asco$Organism=="Ascophyllum nodosum (canopy)")

#write linear reg for pc_asco percent cover and temp by year
# library
library(ggplot2)
# basic scatterplot
ggplot(data=pc_asco,
       mapping =aes(x=Year, y=Percent_cover)) + 
  geom_point()
#how to identify rows containing missing values

