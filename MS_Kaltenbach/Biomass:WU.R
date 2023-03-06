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

cumWU  <-   cumWU%>%
            select(-c(X))

leaf  <-   leaf%>%
            select(-c(X))

df <- merge(cumWU, harvest, by= c("Genotype", "Treatment", "ID"), all = TRUE)%>%
            select(-c(ID.Order))

biomass_combi<- merge(df, leaf, by= c("ID", "Genotype", "Treatment", "Species"), all = TRUE)


# Calculate Indices  -------------------------------------------------------------

biomass_combi <- biomass_combi %>%
                  mutate("leafarea/wu" = TotalLeafArea/sumwu) %>%
                  mutate("canopy_biomass/wu" = canopy_biomass/sumwu) %>%
                  mutate("root_biomass/wu" = root_biomass/sumwu)

biomass_long <-  gather(biomass_combi, key = "MassType", value = "value", "root_biomass/wu", "canopy_biomass/wu", "leafarea/wu") %>%
                  select(-c(canopy_biomass, root_biomass, TotalLeafArea, Date, sumwu))  %>%
                  drop_na()


# Plot  -------------------------------------------------------------


  plot<-biomass_long%>%
    ggplot(aes(x = species_geno, y = value, fill = Treatment))+
    geom_boxplot(aes(y = value))+
    theme_classic()+
    theme(legend.position="bottom")+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))+
    facet_grid(facets = MassType ~ ., scales = "free")+
    xlab("")+
    ylab("")
  
  print(plot)
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset/Biomass_WU/Biomass_WU", ".png"))
  ggsave(paste0("fig_output/Subset/Biomass_WU/Biomass_WU", ".pdf"))
  

 