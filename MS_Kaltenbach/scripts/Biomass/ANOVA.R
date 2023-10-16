
# Load the dplyr package
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(kableExtra)

remove.packages("multcomp")

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Harvest Graphs####

harvest<-read_csv("data/2020_GHDD_Harvest_Data.csv")

# Import data
cumWU <- read.csv("data/Subset/cumWu.csv")
harvest <- read.csv("data/Subset/Harvest.csv")
leaf <- read.csv("data/Subset/Harvest_Leaf.csv")


# Subset and merge data sets
harvest <- harvest  %>% select(ID, Treatment, Genotype, canopy_biomass, root_biomass)%>%
                        mutate(ratio_canopy_root = canopy_biomass / root_biomass) 

cumWU <-cumWU %>%select(-X)%>%
  distinct()
leaf <- leaf %>% select(-X)%>%
  distinct()

genotypes_to_remove <- c("9035", "SC2", "TX6704", "9031")
cumWU <- cumWU %>%
  filter(!Genotype %in% genotypes_to_remove)

biomass<- merge(cumWU, harvest, by = c("Genotype", "Treatment", "ID"), all = TRUE) 
biomass<-biomass%>%
  distinct()
biomass <- merge(biomass, leaf, by = c("ID", "Genotype", "Treatment", "Species"), all = TRUE)
biomass$Species[biomass$Genotype == "TXNM0821"] <- "hybrid"

biomass<- biomass %>%
  mutate("leafarea_wu" = TotalLeafArea / sumwu,
         "canopy_biomass_wu" = canopy_biomass / sumwu,
         "root_biomass_wu" = root_biomass / sumwu)%>%
  distinct()

biomass$Species <- as.factor(biomass$Species)
biomass$Treatment_factor <- as.factor(biomass$Treatment)

   
   # For "sumwu" column
   sumwu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_sumwu = mean(sumwu), sd_sumwu = sd(sumwu)) %>%
     arrange(desc(mean_sumwu))
   print(sumwu_summary)
   
   # For "canopy_biomass" column
   canopy_biomass_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_canopy_biomass = mean(canopy_biomass), sd_canopy_biomass = sd(canopy_biomass)) %>%
     arrange(desc(mean_canopy_biomass))
   print(canopy_biomass_summary)
   
   # For "root_biomass" column
   root_biomass_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_root_biomass = mean(root_biomass), sd_root_biomass = sd(root_biomass)) %>%
     arrange(desc(mean_root_biomass))
   print(root_biomass_summary)
   

   # For "ratio_canopy_root" column
   ratio_canopy_root_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_ratio_canopy_root = mean(ratio_canopy_root), sd_ratio_canopy_root = sd(ratio_canopy_root)) %>%
     arrange(desc(mean_ratio_canopy_root))
   print(ratio_canopy_root_summary)
   
   # For "TotalLeafArea" column
   TotalLeafArea_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_TotalLeafArea = mean(TotalLeafArea), sd_TotalLeafArea = sd(TotalLeafArea)) %>%
     arrange(desc(mean_TotalLeafArea))
   print(TotalLeafArea_summary)
   
   # For "leafarea/wu" column
   leafarea_wu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_leafarea_wu = mean(`leafarea_wu`), sd_leafarea_wu = sd(`leafarea/wu`)) %>%
     arrange(desc(mean_leafarea_wu))
   print(leafarea_wu_summary)
   
   # For "canopy_biomass/wu" column
   canopy_biomass_wu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_canopy_biomass_wu = mean(`canopy_biomass_wu`), sd_canopy_biomass_wu = sd(`canopy_biomass/wu`)) %>%
     arrange(desc(mean_canopy_biomass_wu))
   print(canopy_biomass_wu_summary)
   
   # For "root_biomass/wu" column
   root_biomass_wu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_root_biomass_wu = mean(`root_biomass_wu`), sd_root_biomass_wu = sd(`root_biomass/wu`)) %>%
     arrange(desc(mean_root_biomass_wu))
   print(root_biomass_wu_summary)
   
   

# perform ANOVA -----------------------------------------------------------

   library(multcompView)
   library(multcomp)
   
   
   # For "sumwu" column
   anova_sumwu <- aov(sumwu ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_sumwu <- TukeyHSD(anova_sumwu)
   sumwu.cld <- multcompLetters4(anova_sumwu, tukey_sumwu)
   
   cld_sumwu <- as.data.frame.list(sumwu.cld$`Species`)$Letters
   sumwu_combined_letters <- rep(cld_sumwu, each = length(unique(biomass$Treatment_factor)))
   sumwu_summary$Tukey_sumwu <- sumwu_combined_letters
   sumwu_summary_df <- as.data.frame(sumwu_summary)
   
   # For "root_biomass" column
   anova_root_biomass <- aov(root_biomass ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_root_biomass <- TukeyHSD(anova_root_biomass)
   root_biomass.cld <- multcompLetters4(anova_root_biomass, tukey_root_biomass)
   
   cld_root_biomass <- as.data.frame.list(root_biomass.cld$`Species`)$Letters
   root_biomass_combined_letters <- rep(cld_root_biomass, each = length(unique(biomass$Treatment_factor)))
   root_biomass_summary$Tukey_root_biomass <- root_biomass_combined_letters
   root_biomass_summary_df <- as.data.frame(root_biomass_summary)
   
   # For "canopy_biomass" column
   anova_canopy_biomass <- aov(canopy_biomass ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_canopy_biomass <- TukeyHSD(anova_canopy_biomass)
   canopy_biomass.cld <- multcompLetters4(anova_canopy_biomass, tukey_canopy_biomass)
   
   cld_canopy_biomass <- as.data.frame.list(canopy_biomass.cld$`Species`)$Letters
   canopy_biomass_combined_letters <- rep(cld_canopy_biomass, each = length(unique(biomass$Treatment_factor)))
   canopy_biomass_summary$Tukey_canopy_biomass <- canopy_biomass_combined_letters
   canopy_biomass_summary_df <- as.data.frame(canopy_biomass_summary)
   
   # For "ratio_canopy_root" column
   anova_ratio_canopy_root <- aov(ratio_canopy_root ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_ratio_canopy_root <- TukeyHSD(anova_ratio_canopy_root)
   ratio_canopy_root.cld <- multcompLetters4(anova_ratio_canopy_root, tukey_ratio_canopy_root)
   
   cld_ratio_canopy_root <- as.data.frame.list(ratio_canopy_root.cld$`Species`)$Letters
   ratio_canopy_root_combined_letters <- rep(cld_ratio_canopy_root, each = length(unique(biomass$Treatment_factor)))
   ratio_canopy_root_summary$Tukey_ratio_canopy_root <- ratio_canopy_root_combined_letters
   ratio_canopy_root_summary_df <- as.data.frame(ratio_canopy_root_summary)
   
   # For "TotalLeafArea" column
   anova_TotalLeafArea <- aov(TotalLeafArea ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_TotalLeafArea <- TukeyHSD(anova_TotalLeafArea)
   TotalLeafArea.cld <- multcompLetters4(anova_TotalLeafArea, tukey_TotalLeafArea)
   
   cld_TotalLeafArea <- as.data.frame.list(TotalLeafArea.cld$`Species`)$Letters
   TotalLeafArea_combined_letters <- rep(cld_TotalLeafArea, each = length(unique(biomass$Treatment_factor)))
   TotalLeafArea_summary$Tukey_TotalLeafArea <- TotalLeafArea_combined_letters
   TotalLeafArea_summary_df <- as.data.frame(TotalLeafArea_summary)

   # For "leafarea/wu" column
   anova_leafarea_wu <- aov(`leafarea_wu` ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_leafarea_wu <- TukeyHSD(anova_leafarea_wu)
   leafarea_wu.cld <- multcompLetters4(anova_leafarea_wu, tukey_leafarea_wu)
   
   cld_leafarea_wu <- as.data.frame.list(leafarea_wu.cld$`Species`)$Letters
   leafarea_wu_combined_letters <- rep(cld_leafarea_wu, each = length(unique(biomass$Treatment_factor)))
   leafarea_wu_summary$Tukey_leafarea_wu <- leafarea_wu_combined_letters
   leafarea_wu_summary_df <- as.data.frame(leafarea_wu_summary)
   
   # For "canopy_biomass/wu" column
   anova_canopy_biomass_wu <- aov(`canopy_biomass_wu` ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_canopy_biomass_wu <- TukeyHSD(anova_canopy_biomass_wu)
   canopy_biomass_wu.cld <- multcompLetters4(anova_canopy_biomass_wu, tukey_canopy_biomass_wu)
   
   cld_canopy_biomass_wu <- as.data.frame.list(canopy_biomass_wu.cld$`Species`)$Letters
   canopy_biomass_wu_combined_letters <- rep(cld_canopy_biomass_wu, each = length(unique(biomass$Treatment_factor)))
   canopy_biomass_wu_summary$Tukey_canopy_biomass_wu <- canopy_biomass_wu_combined_letters
   canopy_biomass_wu_summary_df <- as.data.frame(canopy_biomass_wu_summary)
   
   # For "root_biomass/wu" column
   anova_root_biomass_wu <- aov(`root_biomass_wu` ~ Species + Treatment_factor + Species*Treatment_factor, data = biomass)
   tukey_root_biomass_wu <- TukeyHSD(anova_root_biomass_wu)
   root_biomass_wu.cld <- multcompLetters4(anova_root_biomass_wu, tukey_root_biomass_wu)
   
   cld_root_biomass_wu <- as.data.frame.list(root_biomass_wu.cld$`Species`)$Letters
   root_biomass_wu_combined_letters <- rep(cld_root_biomass_wu, each = length(unique(biomass$Treatment_factor)))
   root_biomass_wu_summary$Tukey_root_biomass_wu <- root_biomass_wu_combined_letters
   root_biomass_wu_summary_df <- as.data.frame(root_biomass_wu_summary)
   

# create combined file  ---------------------------------------------------

   biomass_anova_data <- biomass_data %>%
     merge(sumwu_summary_df, by = c("Species", "Treatment")) %>%
     merge(canopy_biomass_summary_df, by = c("Species", "Treatment")) %>%
     merge(root_biomass_summary_df, by = c("Species", "Treatment")) %>%
     merge(ratio_canopy_root_biomass_summary_df, by = c("Species", "Treatment")) %>%
     merge(TotalLeafArea_summary_df, by = c("Species", "Treatment")) %>%
     merge(leafarea_wu_summary_df, by = c("Species", "Treatment")) %>%
     merge(canopy_biomass_wu_summary_df, by = c("Species", "Treatment")) %>%
     merge(root_biomass_wu_summary_df, by = c("Species", "Treatment")) 
   
