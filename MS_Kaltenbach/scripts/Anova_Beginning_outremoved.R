

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

licor_beginning_df <- read.csv("data_output/dates_merged/licor_beginning")
wp_beginning_df <- read.csv("data_output/dates_merged/wp_beginning")

# beginning ---------------------------------------------------------------
# Beginning: Modify-------------------------------------------------------------------------

# select required columns
licor_beginning_df <- licor_beginning_df%>% select(Species, Genotype, Treatment, A, Ci, gsw, E, Tleaf, intrWUE_AE, instWUE_AGs)

licor_beginning_df$Species <- as.factor(licor_beginning_df$Species)
licor_beginning_df$Treatment_factor <- as.factor(licor_beginning_df$Treatment)

wp_beginning_df <- wp_beginning_df%>% select(Species, Genotype, Treatment, LWP)

wp_beginning_df$Species <- as.factor(wp_beginning_df$Species)
wp_beginning_df$Treatment_factor <- as.factor(wp_beginning_df$Treatment)



# Remove Outliers ---------------------------------------------------------


licor_beginning_df<-licor_beginning_df%>%
                    mutate(gsw = ifelse(gsw > 0, gsw, NA))%>%
                    mutate(Tleaf = ifelse(Tleaf > 33, Tleaf, NA))%>%
                    mutate(Ci = ifelse(Ci > 110, gsw, NA))
  

colnames(licor_beginning_df)
min(licor_beginning_df$Tleaf)

boxplot(licor_beginning_df$A)
boxplot(licor_beginning_df$Ci)
boxplot(licor_beginning_df$gsw)
boxplot(licor_beginning_df$E)
boxplot(licor_beginning_df$intrWUE_AE)
boxplot(licor_beginning_df$instWUE_AGs)
# create data summary -----------------------------------------------------

  # A ----------------------------------------------------------------
  A_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_A=mean(A, na.rm = TRUE), sd_A=sd(A, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_A))
  print(A_summary)
  
  # Ci ----------------------------------------------------------------
  Ci_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_Ci=mean(Ci), na.rm = TRUE, sd_Ci=sd(Ci, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_Ci))
  print(Ci_summary)
  
  # gsw----------------------------------------------------------------
  gsw_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_gsw=mean(gsw, na.rm = TRUE), sd_gsw=sd(gsw, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_gsw))
  print(gsw_summary)
  
  # E ----------------------------------------------------------------
  E_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_E=mean(E, na.rm = TRUE), sd_E=sd(E, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_E))
  print(E_summary)
  
  # Tleaf ----------------------------------------------------------------
  Tleaf_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_Tleaf=mean(Tleaf, na.rm = TRUE), sd_Tleaf=sd(Tleaf, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_Tleaf))
  print(Tleaf_summary)
  
  # intrWUE_AE ----------------------------------------------------------------
  intrWUE_AE_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_intrWUE_AE=mean(intrWUE_AE, na.rm = TRUE), sd_intrWUE_AE=sd(intrWUE_AE, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_intrWUE_AE))
  print(intrWUE_AE_summary)
  
  # instWUE_AGs ----------------------------------------------------------------
  instWUE_AGs_summary <- group_by(licor_beginning_df, Species, Treatment) %>%
    summarise(mean_instWUE_AGs=mean(instWUE_AGs, na.rm = TRUE), sd_instWUE_AGs=sd(instWUE_AGs, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_instWUE_AGs))
  print(instWUE_AGs_summary)
  
  # LWP ----------------------------------------------------------------
  LWP_summary <- group_by(wp_beginning_df, Species, Treatment) %>%
    summarise(mean_LWP=mean(LWP, na.rm = TRUE), sd_LWP=sd(LWP, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(mean_LWP))
  print(LWP_summary)
  

# Anova Beginning ---------------------------------------------------------
library(multcompView)
library(multcomp)

    # A ----------------------------------------------------------------
    anova_A <- aov(A ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_A <- TukeyHSD(anova_A)
    A.cld <- multcompLetters4(anova_A, tukey_A)
    
    cld_A <- as.data.frame.list(A.cld$`Species`)$Letters
    A_combined_letters <- rep(cld_A, each = length(unique(licor_beginning_df$Treatment_factor)))
    A_summary$Tukey_A <- A_combined_letters
    A_summary_df <- as.data.frame(A_summary)
    
    # Tleaf ----------------------------------------------------------------
    anova_Ci <- aov(Ci ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_Ci <- TukeyHSD(anova_Ci)
    Ci.cld <- multcompLetters4(anova_Ci, tukey_Ci)
    
    cld_Ci <- as.data.frame.list(Ci.cld$`Species`)$Letters
    Ci_combined_letters <- rep(cld_Ci, each = length(unique(licor_beginning_df$Treatment_factor)))
    Ci_summary$Tukey_Ci <- Ci_combined_letters
    Ci_summary_df <- as.data.frame(Ci_summary)
    
    # gsw ----------------------------------------------------------------
    anova_gsw <- aov(gsw ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_gsw <- TukeyHSD(anova_gsw)
    gsw.cld <- multcompLetters4(anova_gsw, tukey_gsw)
    
    cld_gsw <- as.data.frame.list(gsw.cld$`Species`)$Letters
    gsw_combined_letters <- rep(cld_gsw, each = length(unique(licor_beginning_df$Treatment_factor)))
    gsw_summary$Tukey_gsw <- gsw_combined_letters
    gsw_summary_df <- as.data.frame(gsw_summary)
    
    # E ----------------------------------------------------------------
    anova_E <- aov(E ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_E <- TukeyHSD(anova_E)
    E.cld <- multcompLetters4(anova_E, tukey_E)
    
    cld_E <- as.data.frame.list(E.cld$`Species`)$Letters
    E_combined_letters <- rep(cld_E, each = length(unique(licor_beginning_df$Treatment_factor)))
    E_summary$Tukey_E <- E_combined_letters
    E_summary_df <- as.data.frame(E_summary)
    
    # Tleaf ----------------------------------------------------------------
    anova_Tleaf <- aov(Tleaf ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_Tleaf <- TukeyHSD(anova_Tleaf)
    Tleaf.cld <- multcompLetters4(anova_Tleaf, tukey_Tleaf)
    
    cld_Tleaf <- as.data.frame.list(Tleaf.cld$`Species`)$Letters
    Tleaf_combined_letters <- rep(cld_Tleaf, each = length(unique(licor_beginning_df$Treatment_factor)))
    Tleaf_summary$Tukey_Tleaf <- Tleaf_combined_letters
    Tleaf_summary_df <- as.data.frame(Tleaf_summary)
    
    # intrWUE_AE ----------------------------------------------------------------
    anova_intrWUE_AE <- aov(intrWUE_AE ~ Species + Treatment_factor + Species*Treatment_factor, data =licor_beginning_df)
    tukey_intrWUE_AE <- TukeyHSD(anova_intrWUE_AE)
    intrWUE_AE.cld <- multcompLetters4(anova_intrWUE_AE, tukey_intrWUE_AE)
    
    cld_intrWUE_AE <- as.data.frame.list(intrWUE_AE.cld$`Species`)$Letters
    intrWUE_AE_combined_letters <- rep(cld_intrWUE_AE, each = length(unique(licor_beginning_df$Treatment_factor)))
    intrWUE_AE_summary$Tukey_intrWUE_AE <- intrWUE_AE_combined_letters
    intrWUE_AE_summary_df <- as.data.frame(intrWUE_AE_summary)
    
    # instWUE_AGs ----------------------------------------------------------------
    anova_instWUE_AGs <- aov(instWUE_AGs ~ Species + Treatment_factor + Species*Treatment_factor, data = licor_beginning_df)
    tukey_instWUE_AGs <- TukeyHSD(anova_instWUE_AGs)
    instWUE_AGs.cld <- multcompLetters4(anova_instWUE_AGs, tukey_instWUE_AGs)
    
    cld_instWUE_AGs <- as.data.frame.list(instWUE_AGs.cld$`Species`)$Letters
    instWUE_AGs_combined_letters <- rep(cld_instWUE_AGs, each = length(unique(licor_beginning_df$Treatment_factor)))
    instWUE_AGs_summary$Tukey_instWUE_AGs <- instWUE_AGs_combined_letters
    instWUE_AGs_summary_df <- as.data.frame(instWUE_AGs_summary)
    
    # LWP ----------------------------------------------------------------
    anova_LWP <- aov(LWP ~ Species + Treatment_factor + Species*Treatment_factor, data = wp_beginning_df)
    tukey_LWP <- TukeyHSD(anova_LWP)
    LWP.cld <- multcompLetters4(anova_LWP, tukey_LWP)
    
    cld_LWP <- as.data.frame.list(LWP.cld$`Species`)$Letters
    LWP_combined_letters <- rep(cld_LWP, each = length(unique(wp_beginning_df$Treatment_factor)))
    LWP_summary$Tukey_LWP <- LWP_combined_letters
    LWP_summary_df <- as.data.frame(LWP_summary)

# Combine All -------------------------------------------------------------

    beginning_summary_outremoved <- full_join(A_summary, Ci_summary, by = c("Species", "Treatment")) %>%
      full_join(gsw_summary_df, by = c("Species", "Treatment")) %>%
      full_join(E_summary_df, by = c("Species", "Treatment")) %>%
      full_join(Tleaf_summary_df, by = c("Species", "Treatment")) %>%
      full_join(intrWUE_AE_summary_df, by = c("Species", "Treatment")) %>%
      full_join(instWUE_AGs_summary_df, by = c("Species", "Treatment")) %>%
      full_join(LWP_summary_df, by = c("Species", "Treatment"))
    
    # Print the combined summary dataset
    print(beginning_summary_outremoved)
    
    write.csv(beginning_summary_outremoved, file = "data_output/anova/beginning_summary_outremoved", row.names = FALSE)    

# merge total -------------------------------------------------------------


    species_beginning_anova_data_outremoved <- licor_beginning_df %>%
      left_join(wp_beginning_df, by = c("Species", "Treatment", "Treatment_factor", "Genotype")) %>%
      left_join(beginning_summary_outremoved, by = c("Species", "Treatment"))

    str(licor_beginning_df)
    str(beginning_summary_outremoved)
    str(wp_beginning_df)
    duplicated(species_beginning_anova_data_outremoved)
    
    
    
write.csv(species_beginning_anova_data_outremoved, file = "data_output/anova/species_beginning_anova_data_outremoved", row.names = FALSE)
    