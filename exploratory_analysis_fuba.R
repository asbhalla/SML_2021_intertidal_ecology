##### Filtering Data Demo #####

# Clear Working Directory ---------
rm(list=ls())

# Load packages---------------------
library('tidyverse')
library('ggpubr')

# Set Working Directory ------------
setwd("C:/Users/avnisb/OneDrive - Cornell University/Documents/SML_2021_intertidal_ecology/FuBa")


# Load Data and view it---------------------
library(readr)
sml_pc_tdht <- read_csv("sml_pc_tdht.csv")
View(sml_pc_tdht)
pc <- sml_pc_tdht

#Check Data ------------------------ 
head(pc)
str(pc)
unique(pc$Percent_cover)


#Filtering ------------------------

##Filtering by time
#Select years greater than or equal to 1985
pc_time = filter(pc, Year >=1985)

#Select particular years
##pc_time2 = filter(sml_pc_tdht, Year %in% c(1985, 1995))


##Filtering by Level------------
#Select levels for low intertidal only
##pc_levels = filter(pc2, Level <=13 & Level >=9)


##Filtering by Species
#Select Fucus species and Semibalanus Balanoides
pc_fuba = filter(pc_time, Organism %in% c("Fucus (canopy)",
                                         "Semibalanus balanoides",
                                     
                                     "Fucus vesiculosus (canopy)",
                        
                                     "Fucus spiralis (canopy)",
                                     
                                     "Fucus disticus (canopy)"
                                  ))

### Filter all at once!(example for mastocarpus and chondrus)

#pc_select = pc2 %>%
 # filter(Year >=1985) %>%
 # filter(Level <=13 & Level >=9) %>%
  #filter(Organism %in% c("Chondrus crispus (canopy)", 
                     #    "Mastocarpus stellatus (canopy)"))

#Save file
##write.csv(pc_select, file="pc_select.csv")
write.csv(pc_fuba, file = "pc_fuba.csv")

#Summarize & Graph ------------------
#create new data frame with mean and standard deviation
#pc_avg = pc_fuba %>% 
 # group_by(Year, Organism) %>% #carry over info on Year, Transect, Level (needed?), and Organism
  #summarise(
   # sd = sd(Percent_cover), #standard deviation
    #avg = log(mean(Percent_cover)) #mean
#)
#str(pc_summary)
#log_Avg$pc_summary=log10(Avg$pc_summary)

#scale (normalize) the percent cover for all species-----------
fuba_scaled <-pc_fuba[,-c(1:8,10)]<-scale(pc_fuba[,-c(1:8,10)]) #columns unnecessary to scale

#rejoin scaled pc to original??? START HERE!

#find mean and sd of scaled percent cover     
pc_avg = fuba_scaled %>% 
  group_by(Year, Organism) %>% #carry over info on Year, Transect, Level (needed?), and Organism
summarise(
sd = sd(Percent_cover), #standard deviation
avg = mean(Percent_cover) #mean
)
str(pc_summary)

theme_set(ggpubr::theme_pubr()) #use preset publication ready graphing theme

pc_line = ggline(pc_avg, x="Year", y="avg", color="Organism",
                  add = c("mean_se"),
                  linetype = "Organism", size=1.25)
ggpar(pc_line, xlab = "Year", ylab = "Percent Cover", caption="*no data 2007 and 2008",
      font.x = c(10, "bold"),
      font.y = c(10, "bold"),
      font.caption = c(10, "italic"), legend = "bottom", font.legend = c(16, "italic"))


