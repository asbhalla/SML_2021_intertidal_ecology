# Deal with issue of primary and canopy cover
# Sum primary and canopy cover for spp with both
# Leave only primary spp alone

# load libraries
library(tidyverse)
library(readr)
library(data.table)
library(janitor)

# make "not in" operator
`%notin%` <- Negate(`%in%`)

# read in cover data from SML folder
cover <- read_csv('sml_pc_tdht.csv')

# make a list of spp with primary and canopy cover
primary_canopy_list <- cover %>%
  filter(Organism %like% '(primary)' | Organism %like% '(canopy)') %>%
  select(Organism) %>%
  distinct()
  
# filter for spp with primary and canopy cover
cover2 <- cover %>%
  filter(Organism %like% '(primary)' | Organism %like% '(canopy)') %>%
  # get rid of primary and canopy labels
  mutate(Organism = str_remove(Organism, '\\(primary\\)'),
         Organism = str_remove(Organism, '\\(canopy\\)'),
         Organism = str_remove(Organism, '\\(cprimary\\)')) %>%
  # remove extra space
  mutate(Organism = substr(Organism, 1, (nchar(Organism) - 1)))

# this is a reference for summarizing data - already present in code
# cover_sum <- cover2 %>%
#   group_by(Year, Level, Organism) %>%
#   summarise(mean_group1 = mean(Percent_cover)) %>%
#   ungroup() %>%
#   group_by(Year, Organism) %>%
#   summarise(mean_group2 = mean(mean_group1))

# filter for spp without primary/canopy designation
cover3 <- cover %>%
  filter(Organism %notin% primary_canopy_list)

colnames(cover2) == colnames(cover3)

# make df with summarised data (primary/canopy is summed where applicable)
new_cover <- rbind(cover2, cover3)

# export and save
write_csv(new_cover, 'sml_pc_tideht_sum_cover.csv')
  