## Above/ Below Biomass by Water Usage

# General Notes -----------------------------------------------------------


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
            select(ID, Treatment, Genotype, canopy_biomass, root_biomass)

cumWU<-     cumWU%>%
            select(-c(X))


leaf<-     leaf%>%
            select(-c(X))

df<- merge(cumWU, harvest, by= c("ID", "Genotype", "Treatment"), all = TRUE) 

joineddf<- merge(df, leaf, by= c("ID", "Genotype", "Treatment", "Species"), all = TRUE) 

# Subset and Combine Data Sets -------------------------------------------------------------


biomass_WU <- joineddf %>%
              mutate("canopy_wu" = canopy_biomass/sumwu) %>%
              mutate("root_wu" = root_biomass/sumwu) %>%
              mutate("Leafarea_wu" = TotalLeafArea/sumwu) 

biomass_WU_long <-  gather(biomass_WU, key = "Type", value = "value", "canopy_wu", "root_wu", "Leafarea_wu") %>%
                    select(-c(root_biomass, TotalLeafArea, sumwu, canopy_biomass))%>%
                    drop_na()
  

biomass_WU_long_plot<-biomass_WU_long%>%
                      ggplot(aes(x = species_geno, y = value, fill = Treatment))+
                      geom_boxplot(aes(y = value))+
                      theme_classic()+
                      theme(legend.position="bottom")+
                      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
                      facet_grid(facets = Type ~ ., scales = "free")+
                      xlab("")+
                      ylab("")+
                      ggtitle("Biomass/ WU")

print(biomass_WU_long_plot)

#path to save subset files: 
ggsave(paste0("fig_output/Subset/Biomass_WU/Biomass_WU", ".png"))
ggsave(paste0("fig_output/Subset/Biomass_WU/Biomass_WU", ".pdf"))



