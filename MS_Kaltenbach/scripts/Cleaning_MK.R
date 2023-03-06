# Cleaning Files (Miriam)


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


# Clean Harvest Data ------------------------------------------------------

harvest<- read.csv("data/2020_GHDD_Harvest_Data.csv")

harvest_clean_mk <- harvest %>%
                  select("ID","trt","ID.Order","genotype","species", "block","harvest_date","pd_pot","md_pot", "pd_pot_als","harvest_sm", 
                         "GWC_top","GWC_bottom","stem_diam", "canopy_biomass", "perc_canopy", "dry_root_bag", "bag_tare", "root_biomass", 
                         "perc_root", "root_g", "total_root_length", "sys_area", "surface_area", "srl", "srsa", "exud_time", "exud",
                         "exud_flow", "exud_gg","exp") %>%
                  rename("Treatment" = "trt", "Genotype" = "genotype","Species" = "species", "Block" = "block","Date" = "harvest_date" )%>%
                  mutate( species_geno = paste(Species, Genotype, sep = "_"))%>%
                  mutate(Date = format(as.Date(as.character(Date),"%m/%d/%y"),"%m/%d"))
                  as.Date(harvest$Date, "%m/%d")

# print(harvest_clean)
                  
write.csv(harvest_clean_mk, file= "data/harvest_clean_mk.csv")
     


# Clean Harvest Leaf Biomass Data ------------------------------------------------------

harvest_leaf<- read.csv("data/Leaf_Harvest_Genotypes.csv")

harvest_leaf_clean <- harvest_leaf %>%
                select("ID",  "Genotype", "Species", "Treatment", "TotalLeafArea")  %>%
                mutate(TotalLeafArea = as.numeric(TotalLeafArea)) %>%
                drop_na()

# print(harvest_leaf)

write.csv(harvest_leaf_clean, file= "data/harvest_leaf_clean_mk.csv")