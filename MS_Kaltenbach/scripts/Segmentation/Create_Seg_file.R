library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)

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

colnames(segmentation_data)

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
         SpongyV_PalisadeV, Leaf_Width_um, Mesophyll_Width_um )

grouped_data_seg <- segmentation_data %>%
  group_by(species_geno, Treatment, Species, Genotype) %>%
  summarize(
    mean_Porosity = mean(Porosity),
    sem_Porosity = sd(Porosity, na.rm = TRUE) / sqrt(sum(!is.na(Porosity))),
    mean_SpongyV_TotalMesophyllV = mean(SpongyV_TotalMesophyllV),
    sem_SpongyV_TotalMesophyllV = sd(SpongyV_TotalMesophyllV, na.rm = TRUE) / sqrt(sum(!is.na(SpongyV_TotalMesophyllV))),
    mean_PalisadeV_TotalMesophyllV = mean(PalisadeV_TotalMesophyllV),
    sem_PalisadeV_TotalMesophyllV = sd(PalisadeV_TotalMesophyllV, na.rm = TRUE) / 
      sqrt(sum(!is.na(PalisadeV_TotalMesophyllV))),
    mean_SpongyV_PalisadeV = mean(SpongyV_PalisadeV),
    sem_SpongyV_PalisadeV = sd(SpongyV_PalisadeV, na.rm = TRUE) / sqrt(sum(!is.na(SpongyV_PalisadeV))), 
    mean_Leaf_Width_um = mean(Leaf_Width_um),
    sem_Leaf_Width_um = sd(Leaf_Width_um, na.rm = TRUE) / sqrt(sum(!is.na(Leaf_Width_um))), 
    mean_Mesophyll_Width_um = mean(Mesophyll_Width_um),
    sem_Mesophyll_Width_um = sd(Mesophyll_Width_um, na.rm = TRUE) / sqrt(sum(!is.na(Mesophyll_Width_um))), 
    
  )



for (variable in c("Porosity", 
                   "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", 
                   "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um" ))
{ print(variable)
  t_test_results <- data.frame(Genotype = character(),
                               p_value = numeric(),
                               stringsAsFactors = FALSE)
  
  for (genotype in unique(segmentation_data$Genotype)) {
    subset_data <- subset(segmentation_data, 
                          Genotype == genotype, 
                          select = c(variable, "Treatment", "Genotype"))
    if (all(table(subset_data$Treatment) >= 2)) {
      t_test <- t.test(subset_data[[variable]] ~ Treatment, data = subset_data)
      t_test_results <- rbind(t_test_results,
                              data.frame(
                                Genotype = genotype,
                                p_value= t_test$p.value,
                                stringsAsFactors = FALSE))

  }
  }
  colnames(t_test_results)[ncol(t_test_results)] <- paste0("p_value_", variable)
  grouped_data_seg <- left_join(grouped_data_seg, 
                                 t_test_results, by = "Genotype")
}


write.csv(grouped_data_seg, file = "data_output/segmentation/grouped_data_seg.csv", row.names = FALSE)


segmentation_data <- left_join(segmentation_data, grouped_data_seg, 
                               by = c("Genotype", "Species", "species_geno", "Treatment"))
write.csv(segmentation_data, file = "data_output/segmentation/segmentation_data.csv", row.names = FALSE)

