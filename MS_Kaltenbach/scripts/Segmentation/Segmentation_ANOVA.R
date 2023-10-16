

# libraries ---------------------------------------------------------------


# Load the dplyr package
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(ggplot2)
remove.packages("multcomp")


# Modify data -------------------------------------------------------------


# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Create Segmentation csv -------------------------------------------------
geo_df <- read.csv("data_output/Geo_Data/bio_table")
geo_df$species_geno <- paste0(geo_df$Species, "_", geo_df$Genotype)
species_geno_df <- geo_df %>% 
  select(Species, species_geno, Genotype)

segmentation_data<- read.csv("data/Subset/segmentation_all.csv")

segmentation_data<- read.csv("data_output/segmentation/segmentation_output.csv")
segmentation_data <- segmentation_data %>%
  rename(Genotype = "Species")
segmentation_data $Genotype[segmentation_data $Genotype == "V60_96"] <- "V60-96"

segmentation_data <- inner_join(species_geno_df, segmentation_data, by = "Genotype") %>% 
  mutate(Treatment = recode(Treatment, "C" = "Control", "D" = "Drought"))

# select required columns
segmentation_data <- segmentation_data %>%
  select(Species, species_geno, Genotype, Treatment, 
         Rep, Porosity, Mesophyll_SV.Total_MesophyllV, 
         Mesophyll_PV.Total.mesophyllV, Mesophyll_SV.Mesophyll_PV,
         Leaf_Width_um, Mesophyll_Width_um)%>%
  rename(SpongyV_TotalMesophyllV = Mesophyll_SV.Total_MesophyllV,
         PalisadeV_TotalMesophyllV = Mesophyll_PV.Total.mesophyllV,
         SpongyV_PalisadeV =  Mesophyll_SV.Mesophyll_PV)

segmentation_data <- segmentation_data %>%  select(Species, 
                                                   species_geno, Genotype, Treatment, Porosity, 
                                                   SpongyV_TotalMesophyllV, 
                                                   PalisadeV_TotalMesophyllV,
                                                   SpongyV_PalisadeV, Leaf_Width_um, Mesophyll_Width_um)

segmentation_data$Species <- as.factor(segmentation_data$Species)
segmentation_data$Treatment_factor <- as.factor(segmentation_data$Treatment)

# create data summary -----------------------------------------------------

  # Porosity ----------------------------------------------------------------
    Porosity_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_Porosity=mean(Porosity), sd_Porosity=sd(Porosity)) %>%
      arrange(desc(mean_Porosity))
    print(Porosity_summary)

  # Spongy/Total ------------------------------------------------------------
    SpongyV_TotalMesophyllV_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_SpongyV_TotalMesophyllV=mean(SpongyV_TotalMesophyllV), 
                sd_SpongyV_TotalMesophyllV=sd(SpongyV_TotalMesophyllV)) %>%
      arrange(desc(mean_SpongyV_TotalMesophyllV))
    print(SpongyV_TotalMesophyllV_summary)

  # Palisade/Total ------------------------------------------------------------
    PalisadeV_TotalMesophyllV_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_PalisadeV_TotalMesophyllV=mean(PalisadeV_TotalMesophyllV), 
                sd_PalisadeV_TotalMesophyllV=sd(PalisadeV_TotalMesophyllV)) %>%
      arrange(desc(mean_PalisadeV_TotalMesophyllV))
    print(PalisadeV_TotalMesophyllV_summary)

  # Spongy/Palisade ------------------------------------------------------------
    SpongyV_PalisadeV_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_SpongyV_PalisadeV=mean(SpongyV_PalisadeV), sd_SpongyV_PalisadeV=sd(SpongyV_PalisadeV)) %>%
      arrange(desc(mean_SpongyV_PalisadeV))
    print(SpongyV_PalisadeV_summary)
    
  # leaf width --------------------------------------------------------------
    Leaf_Width_um_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_Leaf_Width_um=mean(Leaf_Width_um), sd_Leaf_Width_um=sd(Leaf_Width_um)) %>%
      arrange(desc(mean_Leaf_Width_um))
    print(Leaf_Width_um_summary)

  # mesophyll width ---------------------------------------------------------
    Mesophyll_Width_um_summary <- group_by(segmentation_data, Species, Treatment) %>%
      summarise(mean_Mesophyll_Width_um=mean(Mesophyll_Width_um), sd_Mesophyll_Width_um=sd(Mesophyll_Width_um)) %>%
      arrange(desc(mean_Mesophyll_Width_um))
    print(Mesophyll_Width_um_summary)
    
# run anovas --------------------------------------------------------------
    library(multcompView)
    library(multcomp)
    
    # Porosity ----------------------------------------------------------------
    anova_Porosity <- aov(Porosity ~ Species + Treatment_factor + Species*Treatment_factor, data = segmentation_data)
    tukey_Porosity <- TukeyHSD(anova_Porosity)
    Porosity.cld <- multcompLetters4(anova_Porosity, tukey_Porosity)
    cld_Porosity <- as.data.frame.list(Porosity.cld$`Species:Treatment_factor`)
    Porosity_summary$Tukey_Porosity <- cld_Porosity$Letters
    
    # Convert the porosity_summary data frame to a regular data frame
    Porosity_summary_df <- as.data.frame(Porosity_summary)
    
    # Spongy/Total ------------------------------------------------------------

    anova_SpongyV_TotalMesophyllV <- aov(SpongyV_TotalMesophyllV ~ Species + Treatment_factor + Species*Treatment_factor, 
                                         data = segmentation_data)
    tukey_SpongyV_TotalMesophyllV <- TukeyHSD(anova_SpongyV_TotalMesophyllV)
    SpongyV_TotalMesophyllV.cld <- multcompLetters4(anova_SpongyV_TotalMesophyllV, tukey_SpongyV_TotalMesophyllV)
    cld_SpongyV_TotalMesophyllV <- as.data.frame.list(SpongyV_TotalMesophyllV.cld$`Species:Treatment_factor`)
    SpongyV_TotalMesophyllV_summary$Tukey_SpongyV_TotalMesophyllV <- cld_SpongyV_TotalMesophyllV$Letters
    
    # Convert the SpongyV_TotalMesophyllV_summary data frame to a regular data frame
    SpongyV_TotalMesophyllV_summary_df <- as.data.frame(SpongyV_TotalMesophyllV_summary)
    
    # Palisade/Total ------------------------------------------------------------
    anova_PalisadeV_TotalMesophyllV <- aov(PalisadeV_TotalMesophyllV ~ Species + Treatment_factor + Species*Treatment_factor, 
                                           data = segmentation_data)
    tukey_PalisadeV_TotalMesophyllV <- TukeyHSD(anova_PalisadeV_TotalMesophyllV)
    PalisadeV_TotalMesophyllV.cld <- multcompLetters4(anova_PalisadeV_TotalMesophyllV, tukey_PalisadeV_TotalMesophyllV)
    cld_PalisadeV_TotalMesophyllV <- as.data.frame.list(PalisadeV_TotalMesophyllV.cld$`Species:Treatment_factor`)
    PalisadeV_TotalMesophyllV_summary$Tukey_PalisadeV_TotalMesophyllV <- cld_PalisadeV_TotalMesophyllV$Letters
    
    # Convert the PalisadeV_TotalMesophyllV_summary data frame to a regular data frame
    PalisadeV_TotalMesophyllV_summary_df <- as.data.frame(PalisadeV_TotalMesophyllV_summary)
    
    # Spongy/Palisade ------------------------------------------------------------
    anova_SpongyV_PalisadeV <- aov(SpongyV_PalisadeV ~ Species + Treatment_factor + Species*Treatment_factor, 
                                   data = segmentation_data)
    tukey_SpongyV_PalisadeV <- TukeyHSD(anova_SpongyV_PalisadeV)
    SpongyV_PalisadeV.cld <- multcompLetters4(anova_SpongyV_PalisadeV, tukey_SpongyV_PalisadeV)
    cld_SpongyV_PalisadeV <- as.data.frame.list(SpongyV_PalisadeV.cld$`Species:Treatment_factor`)
    SpongyV_PalisadeV_summary$Tukey_SpongyV_PalisadeV <- cld_SpongyV_PalisadeV$Letters
    
    # Convert the SpongyV_PalisadeV_summary data frame to a regular data frame
    SpongyV_PalisadeV_summary_df <- as.data.frame(SpongyV_PalisadeV_summary)
    
    # leaf width --------------------------------------------------------------
    anova_Leaf_Width_um <- aov(Leaf_Width_um ~ Species + Treatment_factor + Species*Treatment_factor, data = segmentation_data)
    tukey_Leaf_Width_um <- TukeyHSD(anova_Leaf_Width_um)
    Leaf_Width_um.cld <- multcompLetters4(anova_Leaf_Width_um, tukey_Leaf_Width_um)
    cld_Leaf_Width_um <- as.data.frame.list(Leaf_Width_um.cld$`Species:Treatment_factor`)
    Leaf_Width_um_summary$Tukey_Leaf_Width_um <- cld_Leaf_Width_um$Letters
    
    # Convert the Leaf_Width_um_summary data frame to a regular data frame
    Leaf_Width_um_summary_df <- as.data.frame(Leaf_Width_um_summary)
    
    # mesophyll width ---------------------------------------------------------

    anova_Mesophyll_Width_um <- aov(Mesophyll_Width_um ~ Species + Treatment_factor + Species*Treatment_factor, 
                                    data = segmentation_data)
    tukey_Mesophyll_Width_um <- TukeyHSD(anova_Mesophyll_Width_um)
    Mesophyll_Width_um.cld <- multcompLetters4(anova_Mesophyll_Width_um, tukey_Mesophyll_Width_um)
    cld_Mesophyll_Width_um <- as.data.frame.list(Mesophyll_Width_um.cld$`Species:Treatment_factor`)
    Mesophyll_Width_um_summary$Tukey_Mesophyll_Width_um <- cld_Mesophyll_Width_um$Letters
    
    # Convert the Mesophyll_Width_summary data frame to a regular data frame
    Mesophyll_Width_um_summary_df <- as.data.frame(Mesophyll_Width_um_summary)

# Create combined File-------------------------------------------------------------------------
    
    combined_seg_data <- segmentation_data %>%
      merge(Porosity_summary_df, by = c("Species", "Treatment")) %>%
      merge(SpongyV_TotalMesophyllV_summary_df, by = c("Species", "Treatment")) %>%
      merge(PalisadeV_TotalMesophyllV_summary_df, by = c("Species", "Treatment")) %>%
      merge(SpongyV_PalisadeV_summary_df, by = c("Species", "Treatment")) %>%
      merge(Leaf_Width_um_summary_df, by = c("Species", "Treatment")) %>%
      merge(Mesophyll_Width_um_summary_df, by = c("Species", "Treatment"))
    
    write.csv(combined_seg_data, file = "data_output/segmentation/combined_seg_data.csv", row.names = FALSE)
    