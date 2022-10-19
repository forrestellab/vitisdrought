## Cumulative Water Use for Individuals 

# General Notes -----------------------------------------------------------

#make sure to check if entire data set or just subset (different .csv files and saving paths!)

#calculate for both control and dry down cumulative water use for individuals (boxplot for control and drought (can do all 10 together-> subset), make separate page with cumulative water use 

# Packages ----------------------------------------------------------------

library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

# For the boxplot (themes)
library(tidyverse)
library(hrbrthemes)
library(viridis)

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data -------------------------------------------------------------

# read file for entire data set
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
             mutate( species_geno = paste(Species, Genotype, sep = "_")))

# read file for subset 
swclong<- read.csv("data/Subset/sub_swc.csv")


# Cumulative Wateruse -----------------------------------------------------

cumWU <- swclong %>% 
        group_by(ID, Species, Genotype, Treatment, species_geno) %>% 
        mutate(csum = cumsum(WU))  %>%
        summarise(sumwu = sum(WU))  


# Box-Plot Graph----------------------------------------------------------------

genos<-unique(cumWU$species_geno)

for (i in genos) {
  cumWU_plot<-cumWU%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Genotype, Y = sumwu, fill = Treatment))+
    geom_boxplot(aes(y = sumwu))+
    scale_y_continuous(name = "WU (in liters)", limits=c(0, 21), breaks = seq(0, 21, by = 2))+
    theme_classic()+
    ggtitle(paste("Cumulative WU of", i))
  print(cumWU_plot)

# Saving File -------------------------------------------------------------

  #path to save all files:
  #ggsave(paste0("fig_output/SWC/WU/cumWU/cumWU",i, ".png"))
  #ggsave(paste0("fig_output/SWC/WU/cumWU/cumWU",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset/SWC/WU/cumWU/cumWU",i, ".png"))
  ggsave(paste0("fig_output/Subset/SWC/WU/cumWU/cumWU",i, ".pdf"))
}

