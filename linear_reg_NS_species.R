<<<<<<< HEAD
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
glimpse(pc_filtered)


#average replicates at each level, year, species (later on north/south)
pc_filtered2 <- pc_filtered %>%
  group_by(Year, Level, Organism)%>% #add north/south groupings later
  summarise(mean_group1=mean(Percent_cover))%>% #group1 is avgs across yr,spp,level
            #note that percent cover is saved as "mean_group1" here
  ungroup()%>% #ungroup the data by year, level and organism
  group_by(Year, Organism)%>% #add north/south later
  summarize(mean_group2=mean(mean_group1))#avg the mean percent cover
            #note that percent cover is saved as "mean_group2" here
#check output
head(pc_filtered2)
#remember that pc_filtered2 now has averaged percent covers (mean_group2) for years post-'85 

#filter for one species (asco)
pc_asco <- filter(pc_filtered2, Organism=="Ascophyllum nodosum (canopy)")
#check whether A. nodosum has been filtered
unique(pc_asco$Organism=="Ascophyllum nodosum (canopy)")
#view pc_asco
glimpse(pc_asco)

#write linear reg for pc_asco percent cover and temp by year
# library
library(ggplot2)
# basic scatterplot
ggplot(data=pc_asco,
       mapping =aes(x=Year, y=mean_group2)) + 
  geom_point()
#how to identify rows containing missing values??
