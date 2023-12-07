# libraries ---------------------------------------------------------------
library(tidyverse)
library(ggpubr)
library(hrbrthemes)
library(xtable)

# Modify data -------------------------------------------------------------
# Set Working Directory
 setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")



geo_df <- read.csv("data_output/Geo_Data/bio_table") %>% 
  unite("species_geno", Species, Genotype, remove = FALSE)
species_geno_df <- geo_df %>% 
  select(Species, species_geno, Genotype)



segmentation_data<- read.csv("data/segmentation_data.csv")%>% 
  rename(Genotype = "Species") %>% 
  mutate(Genotype = str_replace_all(Genotype, pattern = "V60_",
                                    replacement = "V60-"))

segmentation_data <- inner_join(species_geno_df, segmentation_data, by = "Genotype") %>% 
  mutate(Treatment = recode(Treatment, "C" = "Control", "D" = "Drought"))


segmentation_data <- segmentation_data %>%
  select(Species, species_geno, Genotype, Treatment, 
         Rep, Porosity, Mesophyll_SV.Total_MesophyllV, 
         Mesophyll_PV.Total.mesophyllV, Mesophyll_SV.Mesophyll_PV,
         Leaf_Width_um, Mesophyll_Width_um, 
         SA.Spongy..SAWMES,SA.Palisade.SAWMES., SA.SPOnGY.Palisade..SAWMES)%>%
  rename(SpongyV_TotalMesophyllV = Mesophyll_SV.Total_MesophyllV,
         PalisadeV_TotalMesophyllV = Mesophyll_PV.Total.mesophyllV,
         SpongyV_PalisadeV =  Mesophyll_SV.Mesophyll_PV,
         SASP_SAWM = SA.Spongy..SAWMES ,
         SAPA_SAWM = SA.Palisade.SAWMES., 
         SASPPA_SAWM = SA.SPOnGY.Palisade..SAWMES)

 segmentation_data <- segmentation_data %>%
   select(Species,
          species_geno, Genotype, Treatment, Porosity, 
          SpongyV_TotalMesophyllV, 
          PalisadeV_TotalMesophyllV,
          SpongyV_PalisadeV, Leaf_Width_um, Mesophyll_Width_um, SASP_SAWM, SAPA_SAWM, SASPPA_SAWM )


segmentation_data <- segmentation_data %>% 
  mutate(Species = factor(Species),
         Treatment_factor = factor(Treatment))


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
   summarise(mean_SpongyV_PalisadeV=mean(SpongyV_PalisadeV),
             sd_SpongyV_PalisadeV=sd(SpongyV_PalisadeV)) %>%
   arrange(desc(mean_SpongyV_PalisadeV))
 print(SpongyV_PalisadeV_summary)

# leaf width --------------------------------------------------------------
 Leaf_Width_um_summary <- group_by(segmentation_data, Species, Treatment) %>%
   summarise(mean_Leaf_Width_um=mean(Leaf_Width_um),
             sd_Leaf_Width_um=sd(Leaf_Width_um)) %>%
   arrange(desc(mean_Leaf_Width_um))
 print(Leaf_Width_um_summary)

# mesophyll width ---------------------------------------------------------
 Mesophyll_Width_um_summary <- group_by(segmentation_data, Species, Treatment) %>%
   summarise(mean_Mesophyll_Width_um=mean(Mesophyll_Width_um),
             sd_Mesophyll_Width_um=sd(Mesophyll_Width_um)) %>%
   arrange(desc(mean_Mesophyll_Width_um))
 print(Mesophyll_Width_um_summary)

 SASP_SAWM_summary <- group_by(segmentation_data, Species, Treatment) %>%
   summarise(mean_SASP_SAWM=mean(SASP_SAWM), sd_SASP_SAWM=sd(SASP_SAWM)) %>%
   arrange(desc(mean_SASP_SAWM))
 print(SASP_SAWM_summary)
 
 SAPA_SAWM_summary <- group_by(segmentation_data, Species, Treatment) %>%
   summarise(mean_SAPA_SAWM=mean(SAPA_SAWM), sd_SAPA_SAWM=sd(SAPA_SAWM)) %>%
   arrange(desc(mean_SAPA_SAWM))
 print(SAPA_SAWM_summary)
 
 SASPPA_SAWM_summary <- group_by(segmentation_data, Species, Treatment) %>%
   summarise(mean_SASPPA_SAWM=mean(SASPPA_SAWM), sd_SASPPA_SAWM=sd(SASPPA_SAWM)) %>%
   arrange(desc(mean_SASPPA_SAWM))
 print(SASPPA_SAWM_summary)
 
# run anovas --------------------------------------------------------------
    library(multcompView)
    library(multcomp)
    
create.my.cld <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova.Y <- aov(form, data = segmentation_data)
#  print(summary(anova.Y))
  
  tukey.Y <- TukeyHSD(anova.Y)
  Y.cld <- multcompLetters4(anova.Y, tukey.Y)


  Y.cld <- as.data.frame.list(Y.cld$`Species`)
  cld.Y <- Y.cld[order(rownames(Y.cld)), "Letters"]

  rep(cld.Y, each = nlevels(segmentation_data$Treatment_factor))
  }

my.aov <- function(Y) {
   form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
   anova(aov(form, data = segmentation_data))
   }

 
Porosity_summary_df <- ungroup(Porosity_summary) %>%
  arrange(Species) %>% 
  mutate(Tukey_Porosity = create.my.cld("Porosity")) %>% 
  as.data.frame()

SpongyV_TotalMesophyllV_summary_df <- ungroup(SpongyV_TotalMesophyllV_summary) %>% 
  arrange(Species) %>% 
  mutate(Tukey_SpongyV_TotalMesophyllV = create.my.cld("SpongyV_TotalMesophyllV")) %>% 
  as.data.frame()

PalisadeV_TotalMesophyllV_summary_df <- ungroup(PalisadeV_TotalMesophyllV_summary) %>% 
  arrange(Species) %>% 
  mutate(Tukey_PalisadeV_TotalMesophyllV = create.my.cld("PalisadeV_TotalMesophyllV")) %>% 
  as.data.frame()

SpongyV_PalisadeV_summary_df <- ungroup(SpongyV_PalisadeV_summary) %>% 
  arrange(Species) %>% 
  mutate(Tukey_SpongyV_PalisadeV = create.my.cld("SpongyV_PalisadeV")) %>% 
  as.data.frame()

Leaf_Width_um_summary_df <- ungroup(Leaf_Width_um_summary) %>% 
  arrange(Species) %>% 
  mutate(Tukey_Leaf_Width_um = create.my.cld("Leaf_Width_um")) %>% 
  as.data.frame()

Mesophyll_Width_um_summary_df <- ungroup(Mesophyll_Width_um_summary) %>% 
  arrange(Species) %>% 
  mutate(Tukey_Mesophyll_Width_um = create.my.cld("Mesophyll_Width_um")) %>% 
  as.data.frame()

SASP_SAWM_summary_df <- ungroup(SASP_SAWM_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_SASP_SAWM = create.my.cld("SASP_SAWM")) %>% 
  as.data.frame()

SAPA_SAWM_summary_df <- ungroup(SAPA_SAWM_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_SAPA_SAWM = create.my.cld("SAPA_SAWM")) %>% 
  as.data.frame()

SASPPA_SAWM_summary_df <- ungroup(SASPPA_SAWM_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_SASPPA_SAWM = create.my.cld("SASPPA_SAWM")) %>% 
  as.data.frame()



resp.vars <- c("Porosity", "SpongyV_TotalMesophyllV",
               "PalisadeV_TotalMesophyllV",  "SpongyV_PalisadeV",
               "Leaf_Width_um", "Mesophyll_Width_um",  "SASP_SAWM",
               "SAPA_SAWM", 
               "SASPPA_SAWM" )


aov.latex.outputlist <- lapply(resp.vars, function(v) {
  anova_table <- my.aov(v) %>%
    xtable(caption = paste("Analysis of variance (ANOVA) of the main and effects and interaction effects of", v))
  print(anova_table, booktabs = FALSE, hline.after = FALSE, hline.before = TRUE, include.rownames = TRUE, comment = FALSE)
}) %>% 
  setNames(resp.vars)



# Create combined File-------------------------------------------------------------------------

species_seg_anova_data <- segmentation_data %>%
  merge(Porosity_summary_df, by = c("Species", "Treatment")) %>%
  merge(SpongyV_TotalMesophyllV_summary_df, by = c("Species", "Treatment")) %>%
  merge(PalisadeV_TotalMesophyllV_summary_df, by = c("Species", "Treatment")) %>%
  merge(SpongyV_PalisadeV_summary_df, by = c("Species", "Treatment")) %>%
  merge(Leaf_Width_um_summary_df, by = c("Species", "Treatment")) %>%
  merge(Mesophyll_Width_um_summary_df, by = c("Species", "Treatment"))%>%
  merge(SASP_SAWM_summary_df, by = c("Species", "Treatment"))%>%
  merge(SAPA_SAWM_summary_df, by = c("Species", "Treatment"))%>%
  merge(SASPPA_SAWM_summary_df, by = c("Species", "Treatment"))

write.csv(species_seg_anova_data, file = "data_output/segmentation/seg_anova.csv", row.names = FALSE)
