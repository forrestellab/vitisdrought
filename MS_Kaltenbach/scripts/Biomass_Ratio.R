## Above/ Below Biomass by Water Usage

# General Notes -----------------------------------------------------------

# had to extract the leaf area from the excel sheet and modify it (it oterwise would have not had species and genotype in it)

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


cumWU  <- read.csv("data/Subset/cumWu.csv")
harvest<- read.csv("data/Subset/Harvest.csv")
leaf  <-  read.csv("data/Subset/Harvest_Leaf.csv")



# Subset and Combine Data Sets -------------------------------------------------------------

harvest<-   harvest%>%
  select(ID.Order, ID, Date, Treatment, Genotype, canopy_biomass, root_biomass)


# Calculate Indices  -------------------------------------------------------------

biomass_ratio <- harvest %>%
                 mutate("ratio_canopy_root" = canopy_biomass/root_biomass) 

write.csv(biomass_ratio, file= "data/Subset/sub_small_biomass_ratio.csv")

# Plot Ratio  -------------------------------------------------------------

biomass_ratio_plot<-biomass_ratio %>%
                    ggplot(aes(x = Genotype, y = ratio_canopy_root, fill = Treatment))+
                    geom_boxplot(aes(y = ratio_canopy_root))+
                    theme_classic()+
                    theme(legend.position="bottom")+
                    theme(axis.text.x = element_text(angle = 30, hjust = 1))+
                    ggtitle("Canopy Root Biomass Ratio")

print(biomass_ratio_plot)

        #path to save subset files: 
        ggsave(paste0("fig_output/Subset_small/Biomass_Ratio/Biomass_Ratio", ".png"))
        ggsave(paste0("fig_output/Subset_small/Biomass_Ratio/Biomass_Ratio", ".pdf"))


