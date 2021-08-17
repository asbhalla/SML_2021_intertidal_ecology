# Objectives of this code are to:------------------------
#   1. calculate rsq and p-values for:
#     1.1 mean percent cover of all species and year
#     1.2 mean percent cover of all species and mean temp
#     
#   2. plot a linear regression graph for:
#     2.1 mean percent cover of all species and year
#     2.2 mean percent cover of all species and mean annual temp
#---------------------------------------------------------

#set working directory
setwd("~/SML_2021_intertidal_ecology/Lauren_code")

#use purrr to calculate rsq and p-values
#use loop to do plots in ggplot
#load libraries
library(readr)
library(tidyverse)
library(ggplot2)
library(purrr)
library(magrittr)
library(ggpubr)
library(broom)
library(gt)
#read in required data
sml_pc_tdht_max_cover <- read_csv("sml_pc_tdht_max_cover.csv")
View(sml_pc_tdht_max_cover)
sml_environmental_data <- read_csv("Annual Environmental data.xlsx - Sheet1.csv")
view(sml_environmental_data)

#let's add in mean annual temp to the df--------------------------
  #pare down the df to only year and sst_temp
  sml_temp <- select(sml_environmental_data, Year, sst_yr)
  glimpse(sml_temp)
  view(sml_temp)
  #join sml_temp with sml_pc_tdht_max_cover
  sml_pc_temp <-left_join(sml_pc_tdht_max_cover, sml_temp, by="Year")
  glimpse(sml_pc_temp)
#done!-------------------------------------------------
  
#filter out years post-'85 because crappy data is as crappy data does------
  sml_cover_temp_85 <- filter(sml_pc_temp, Year>=1985)
  unique(sml_cover_temp_85$Year>=1985)
#done!------------------------------------ #working df = sml_cover_temp85

#find out mean percent cover for species by year--------------
  mean_cover <- sml_cover_temp_85%>%
    group_by(Organism, Desig_category, Year, Level, sst_yr)%>%
    summarise(mean1=mean(Percent_cover))%>%
    ungroup()%>%
    group_by(Organism, Year, Desig_category, sst_yr)%>%
    summarise(mean2=mean(mean1))
  glimpse(mean_cover)
#done!---------------------------------------- #now, working df = mean_cover
  
# calculate rsq and p-values for all species over year using purrr--------
# this has two parts to it:
#   1. rsq and p-values for species mean percent cover with mean annual temp
  loop_data_temp <- mean_cover %>%
    group_by(Organism, Desig_category
            ) %>%
    nest()%>%
    mutate(fit = map(data, ~ lm(.$mean2 ~ .$sst_yr)),
           summary = map(fit, glance)) %>%
    unnest(c(Organism, Desig_category, data,summary))%>%
    select(r.squared, p.value, sst_yr, Year, mean2)%>%
    ungroup()
head(loop_data_temp)
view(loop_data_temp)

#get rid of sst_yr, year and mean2
loop_data_temp_select <- loop_data_temp%>%
  select(c(1:5,7))%>%
  distinct()%>%
  filter(p.value<=0.05) #selecting only significant p-values
head(loop_data_temp_select)
write.csv(loop_data_temp_select, "sig_cover_temp.csv")
  
# 2. rsq and p-values for species mean percent cover with year
loop_data_year <- mean_cover %>%
  group_by(Organism, Desig_category
  ) %>%
  nest()%>%
  mutate(fit = map(data, ~ lm(.$mean2 ~ .$Year)),
         summary = map(fit, glance)) %>%
  unnest(c(Organism, Desig_category, data,summary))%>%
  select(r.squared, p.value, sst_yr, Year, mean2)%>%
  ungroup()
head(loop_data_year)
view(loop_data_year)

#get rid of sst_yr, year and mean2
loop_data_year_select <- loop_data_temp%>%
  select(c(1:4,6:7))%>%
  distinct()%>%
  filter(p.value<=0.05) #selecting only significant p-values
head(loop_data_year_select)
write.csv(loop_data_year_select, "sig_cover_year.csv")

#cue some helpful code from Lauren------------------------------
# # regressions with purrr
# group_by(Site_code, Level) %>%
#   nest() %>%
#   mutate(fit = map(data, ~ lm(.$Mean_length ~ .$Calendar_year)),
#          summary = map(fit, glance)) %>%
#   # un-nest data, preserving linear model summary
#   unnest(Site_code, Level, data, summary) %>%
#   select(-c(adj.r.squared, sigma, statistic, df:nobs, fit)) 
#done! ----------------------------------------------------------

#LOOPS--------------------------------------------------------------
#create a list of organisms to loop over
species <- unique(mean_cover$Organism)

#build a loop in ggplot2 to compare slopes of mean pc with temp

for(i in 1:length(species)){

  ggplot(data=filter(loop_data_temp_select, Organism == species[i]), 
         aes(y=mean2, x=sst_yr)) +
  geom_point() +
  geom_smooth(method='lm') #lm model creates a fit to the line
  
  ggsave(paste(species[i], 'pc_vs_temp.png'))  #to save a plot ggsave
}
 


#build a loop in ggplot2 to compare slopes of mean pc with yr

for(i in 1:length(species)){
  
  ggplot(data=filter(loop_data_year_select, Organism == species[i]), 
         aes(y=mean2, x=Year)) +
    geom_point() +
    geom_smooth(method='lm') #lm model creates a fit to the line
  
  ggsave(paste(species[i], 'pc_vs_year.png'))#to save a plot ggsave
  summary('lm')
  pdf(summary('lm'), paste(species[i],"summary.pdf"))
}

#export summary stats to csv
#group_by "desig_category"
#then for slopes of each regression,
#compare "cool" to "warm" to "average"






                    
