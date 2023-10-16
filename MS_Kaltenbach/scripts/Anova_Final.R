
#Final Date

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

# Load & Modify Data ---------------------------------------------------------------

licor_final_df <- read.csv("data_output/dates_merged/licor_final")
wp_final_df <- read.csv("data_output/dates_merged/wp_final")
# create data summary -----------------------------------------------------

# Final ---------------------------------------------------------------
# select required columns
licor_final_df <- licor_final_df%>% select(Species, Genotype, Treatment, A, Ci, gsw, E, Tleaf, intrWUE_AE, instWUE_AGs)

licor_final_df$Species <- as.factor(licor_final_df$Species)
licor_final_df$Treatment_factor <- as.factor(licor_final_df$Treatment)

wp_final_df <- wp_final_df%>% select(Species, Genotype, Treatment, LWP, SWP, PD, leafwp_predawn)

wp_final_df$Species <- as.factor(wp_final_df$Species)
wp_final_df$Treatment_factor <- as.factor(wp_final_df$Treatment)

# create data summary -----------------------------------------------------

# A ----------------------------------------------------------------
A_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_A=mean(A, na.rm = TRUE), sd_A=sd(A, na.rm = TRUE)) %>%
  arrange(desc(mean_A))
print(A_summary)

# Ci ----------------------------------------------------------------
Ci_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_Ci=mean(Ci, na.rm = TRUE), sd_Ci=sd(Ci, na.rm = TRUE)) %>%
  arrange(desc(mean_Ci))
print(Ci_summary)

# gsw----------------------------------------------------------------
gsw_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_gsw=mean(gsw, na.rm = TRUE), sd_gsw=sd(gsw, na.rm = TRUE)) %>%
  arrange(desc(mean_gsw))
print(gsw_summary)

# E ----------------------------------------------------------------
E_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_E=mean(E, na.rm = TRUE), sd_E=sd(E, na.rm = TRUE)) %>%
  arrange(desc(mean_E))
print(E_summary)

# Tleaf ----------------------------------------------------------------
Tleaf_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_Tleaf=mean(Tleaf, na.rm = TRUE), sd_Tleaf=sd(Tleaf, na.rm = TRUE)) %>%
  arrange(desc(mean_Tleaf))
print(Tleaf_summary)

# intrWUE_AE ----------------------------------------------------------------
intrWUE_AE_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_intrWUE_AE=mean(intrWUE_AE, na.rm = TRUE), sd_intrWUE_AE=sd(intrWUE_AE, na.rm = TRUE)) %>%
  arrange(desc(mean_intrWUE_AE))
print(intrWUE_AE_summary)

# instWUE_AGs ----------------------------------------------------------------
instWUE_AGs_summary <- group_by(licor_final_df, Species, Treatment) %>%
  summarise(mean_instWUE_AGs=mean(instWUE_AGs, na.rm = TRUE), sd_instWUE_AGs=sd(instWUE_AGs, na.rm = TRUE)) %>%
  arrange(desc(mean_instWUE_AGs))
print(instWUE_AGs_summary)

# LWP ----------------------------------------------------------------
LWP_summary <- group_by(wp_final_df, Species, Treatment) %>%
  summarise(mean_LWP=mean(LWP, na.rm = TRUE), sd_LWP=sd(LWP, na.rm = TRUE)) %>%
  arrange(desc(mean_LWP))
print(LWP_summary)

# PD ----------------------------------------------------------------
PD_summary <- group_by(wp_final_df, Species, Treatment) %>%
  summarise(mean_PD=mean(PD, na.rm = TRUE), sd_PD=sd(PD, na.rm = TRUE)) %>%
  arrange(desc(mean_PD))
print(PD_summary)

# SWP ----------------------------------------------------------------
SWP_summary <- group_by(wp_final_df, Species, Treatment) %>%
  summarise(mean_SWP=mean(SWP, na.rm = TRUE), sd_SWP=sd(SWP, na.rm = TRUE)) %>%
  arrange(desc(mean_SWP))
print(SWP_summary)

# leafwp_predawn ----------------------------------------------------------
leafwp_predawn_summary <- group_by(wp_final_df, Species, Treatment) %>%
  summarise(mean_leafwp_predawn=mean(leafwp_predawn, na.rm = TRUE), sd_leafwp_predawn=sd(leafwp_predawn, na.rm = TRUE)) %>%
  arrange(desc(mean_leafwp_predawn))
print(leafwp_predawn_summary)


# Anova final ---------------------------------------------------------
library(multcompView)
library(multcomp)

# A ----------------------------------------------------------------
anova_A <- aov(A ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_A <- TukeyHSD(anova_A)
A.cld <- multcompLetters4(anova_A, tukey_A)

cld_A <- as.data.frame.list(A.cld$`Species`)$Letters
A_combined_letters <- rep(cld_A, each = length(unique(licor_final_df$Treatment_factor)))
A_summary$Tukey_A <- A_combined_letters
A_summary_df <- as.data.frame(A_summary)

# Tleaf ----------------------------------------------------------------
anova_Ci <- aov(Ci ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_Ci <- TukeyHSD(anova_Ci)
Ci.cld <- multcompLetters4(anova_Ci, tukey_Ci)

cld_Ci <- as.data.frame.list(Ci.cld$`Species`)$Letters
Ci_combined_letters <- rep(cld_Ci, each = length(unique(licor_final_df$Treatment_factor)))
Ci_summary$Tukey_Ci <- Ci_combined_letters
Ci_summary_df <- as.data.frame(Ci_summary)

# gsw ----------------------------------------------------------------
anova_gsw <- aov(gsw ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_gsw <- TukeyHSD(anova_gsw)
gsw.cld <- multcompLetters4(anova_gsw, tukey_gsw)

cld_gsw <- as.data.frame.list(gsw.cld$`Species`)$Letters
gsw_combined_letters <- rep(cld_gsw, each = length(unique(licor_final_df$Treatment_factor)))
gsw_summary$Tukey_gsw <- gsw_combined_letters
gsw_summary_df <- as.data.frame(gsw_summary)

# E ----------------------------------------------------------------
anova_E <- aov(E ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_E <- TukeyHSD(anova_E)
E.cld <- multcompLetters4(anova_E, tukey_E)

cld_E <- as.data.frame.list(E.cld$`Species`)$Letters
E_combined_letters <- rep(cld_E, each = length(unique(licor_final_df$Treatment_factor)))
E_summary$Tukey_E <- E_combined_letters
E_summary_df <- as.data.frame(E_summary)

# Tleaf ----------------------------------------------------------------
anova_Tleaf <- aov(Tleaf ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_Tleaf <- TukeyHSD(anova_Tleaf)
Tleaf.cld <- multcompLetters4(anova_Tleaf, tukey_Tleaf)

cld_Tleaf <- as.data.frame.list(Tleaf.cld$`Species`)$Letters
Tleaf_combined_letters <- rep(cld_Tleaf, each = length(unique(licor_final_df$Treatment_factor)))
Tleaf_summary$Tukey_Tleaf <- Tleaf_combined_letters
Tleaf_summary_df <- as.data.frame(Tleaf_summary)

# intrWUE_AE ----------------------------------------------------------------
anova_intrWUE_AE <- aov(intrWUE_AE ~ Species + Treatment_factor + Species*Treatment_factor, data =licor_final_df)
tukey_intrWUE_AE <- TukeyHSD(anova_intrWUE_AE)
intrWUE_AE.cld <- multcompLetters4(anova_intrWUE_AE, tukey_intrWUE_AE)

cld_intrWUE_AE <- as.data.frame.list(intrWUE_AE.cld$`Species`)$Letters
intrWUE_AE_combined_letters <- rep(cld_intrWUE_AE, each = length(unique(licor_final_df$Treatment_factor)))
intrWUE_AE_summary$Tukey_intrWUE_AE <- intrWUE_AE_combined_letters
intrWUE_AE_summary_df <- as.data.frame(intrWUE_AE_summary)

# instWUE_AGs ----------------------------------------------------------------
anova_instWUE_AGs <- aov(instWUE_AGs ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_final_df)
tukey_instWUE_AGs <- TukeyHSD(anova_instWUE_AGs)
instWUE_AGs.cld <- multcompLetters4(anova_instWUE_AGs, tukey_instWUE_AGs)

cld_instWUE_AGs <- as.data.frame.list(instWUE_AGs.cld$`Species`)$Letters
instWUE_AGs_combined_letters <- rep(cld_instWUE_AGs, each = length(unique(licor_final_df$Treatment_factor)))
instWUE_AGs_summary$Tukey_instWUE_AGs <- instWUE_AGs_combined_letters
instWUE_AGs_summary_df <- as.data.frame(instWUE_AGs_summary)

# LWP ----------------------------------------------------------------
anova_LWP <- aov(LWP ~ Species + Treatment_factor + Species*Treatment_factor, data = wp_final_df)
tukey_LWP <- TukeyHSD(anova_LWP)
LWP.cld <- multcompLetters4(anova_LWP, tukey_LWP)

cld_LWP <- as.data.frame.list(LWP.cld$`Species`)$Letters
LWP_combined_letters <- rep(cld_LWP, each = length(unique(wp_final_df$Treatment_factor)))
LWP_summary$Tukey_LWP <- LWP_combined_letters
LWP_summary_df <- as.data.frame(LWP_summary)


# PD ----------------------------------------------------------------------

anova_PD <- aov(PD ~ Species + Treatment_factor + Species*Treatment_factor, data = wp_final_df)
tukey_PD <- TukeyHSD(anova_PD)
PD.cld <- multcompLetters4(anova_PD, tukey_PD)

cld_PD <- as.data.frame.list(PD.cld$`Species`)$Letters
PD_combined_letters <- rep(cld_PD, each = length(unique(wp_final_df$Treatment_factor)))
PD_summary$Tukey_PD <- PD_combined_letters
PD_summary_df <- as.data.frame(PD_summary)

# SWD ----------------------------------------------------------------------

anova_SWP <- aov(SWP ~ Species + Treatment_factor + Species*Treatment_factor, data = wp_final_df)
tukey_SWP <- TukeyHSD(anova_SWP)
SWP.cld <- multcompLetters4(anova_SWP, tukey_SWP)

cld_SWP <- as.data.frame.list(SWP.cld$`Species`)$Letters
SWP_combined_letters <- rep(cld_SWP, each = length(unique(wp_final_df$Treatment_factor)))
SWP_summary$Tukey_SWP <- SWP_combined_letters
SWP_summary_df <- as.data.frame(SWP_summary)

# leafwp_predawn ----------------------------------------------------------------------

#anova_leafwp_predawn <- aov(leafwp_predawn ~ Species + Treatment_factor + Species*Treatment_factor, data = wp_final_df)
anova_leafwp_predawn <- aov(leafwp_predawn ~ Species + Treatment_factor, data = wp_final_df)
tukey_leafwp_predawn <- TukeyHSD(anova_leafwp_predawn)
leafwp_predawn.cld <- multcompLetters4(anova_leafwp_predawn, tukey_leafwp_predawn)

cld_leafwp_predawn <- as.data.frame.list(leafwp_predawn.cld$`Species`)$Letters
leafwp_predawn_combined_letters <- rep(cld_leafwp_predawn, each = length(unique(wp_final_df$Treatment_factor)))
leafwp_predawn_summary$Tukey_leafwp_predawn <- leafwp_predawn_combined_letters
leafwp_predawn_summary_df <- as.data.frame(leafwp_predawn_summary)



final_summary <- full_join(A_summary, Ci_summary, by = c("Species", "Treatment")) %>%
  full_join(gsw_summary_df, by = c("Species", "Treatment")) %>%
  full_join(E_summary_df, by = c("Species", "Treatment")) %>%
  full_join(Tleaf_summary_df, by = c("Species", "Treatment")) %>%
  full_join(intrWUE_AE_summary_df, by = c("Species", "Treatment")) %>%
  full_join(instWUE_AGs_summary_df, by = c("Species", "Treatment")) %>%
  full_join(LWP_summary_df, by = c("Species", "Treatment")) %>%
  full_join(PD_summary_df, by = c("Species", "Treatment")) %>%
  full_join(SWP_summary_df, by = c("Species", "Treatment")) %>%
  full_join(leafwp_predawn_summary_df, by = c("Species", "Treatment"))

# Print the combined summary dataset
print(final_summary)

write.csv(final_summary, file = "data_output/anova/final_summary", row.names = FALSE)    

# merge total -------------------------------------------------------------

species_final_anova_data <- licor_final_df %>%
  left_join(wp_final_df, by = c("Species", "Treatment", "Treatment_factor", "Genotype")) %>%
  left_join(final_summary, by = c("Species", "Treatment"))

str(licor_final_df)
str(final_summary)
str(wp_final_df)
duplicated(species_final_anova_data)

write.csv(species_final_anova_data, file = "data_output/anova/species_final_anova_data", row.names = FALSE)

