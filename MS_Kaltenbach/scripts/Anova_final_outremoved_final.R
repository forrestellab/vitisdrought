
# libraries ---------------------------------------------------------------

# Load the dplyr package
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(ggplot2)
library(xtable)
remove.packages("multcomp")

# Modify data -------------------------------------------------------------

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Load & Modify Data ---------------------------------------------------------------

licor_final_df <- read.csv("data_output/dates_merged/licor_final")
wp_final_df <- read.csv("data_output/dates_merged/wp_final")

# select required columns
licor_final_df <- licor_final_df %>% select(Species, Genotype, Treatment, A, Ci, gsw, E, Tleaf, intrWUE_AE, instWUE_AGs)

licor_final_df$Species <- as.factor(licor_final_df$Species)
licor_final_df$Treatment_factor <- as.factor(licor_final_df$Treatment)

wp_final_df <- wp_final_df %>% select(Species, Genotype, Treatment, LWP, PD)

wp_final_df$Species <- as.factor(wp_final_df$Species)
wp_final_df$Treatment_factor <- as.factor(wp_final_df$Treatment)

wp_final_df_lwp <- wp_final_df[complete.cases(wp_final_df$LWP), ]
wp_final_df_pd <- wp_final_df[complete.cases(wp_final_df$PD), ]

wp_final_df_lwp <- wp_final_df[complete.cases(wp_final_df$LWP) & wp_final_df$Species != "vulpina", ]


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
LWP_summary <- group_by(wp_final_df_lwp, Species, Treatment) %>%
  summarise(mean_LWP=mean(LWP, na.rm = TRUE), sd_LWP=sd(LWP, na.rm = TRUE)) %>%
  arrange(desc(mean_LWP))
print(LWP_summary)

# PD ----------------------------------------------------------------
PD_summary <- group_by(wp_final_df_pd, Species, Treatment) %>%
  summarise(mean_PD=mean(PD, na.rm = TRUE), sd_PD=sd(PD, na.rm = TRUE)) %>%
  arrange(desc(mean_PD))
print(PD_summary)




# Anova final ---------------------------------------------------------
library(multcompView)
library(multcomp)

# A ----------------------------------------------------------------


create.my.cld <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova.Y <- aov(form, data = licor_final_df)
  
  tukey.Y <- TukeyHSD(anova.Y)
  Y.cld <- multcompLetters4(anova.Y, tukey.Y)
  
  Y.cld <- as.data.frame.list(Y.cld$`Species`)
  cld.Y <- Y.cld[order(rownames(Y.cld)), "Letters"]

  
  rep(cld.Y, each = nlevels(licor_final_df$Treatment_factor))
}

my.aov <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova(aov(form, data = licor_final_df))
}


A_summary_df <- ungroup(A_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_A = create.my.cld("A")) %>% 
  as.data.frame()

Ci_summary_df <- ungroup(Ci_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_Ci = create.my.cld("Ci")) %>% 
  as.data.frame()

gsw_summary_df <- ungroup(gsw_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_gsw = create.my.cld("gsw")) %>% 
  as.data.frame()

E_summary_df <- ungroup(E_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_E = create.my.cld("E")) %>% 
  as.data.frame()

Tleaf_summary_df <- ungroup(Tleaf_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_Tleaf = create.my.cld("Tleaf")) %>% 
  as.data.frame()

intrWUE_AE_summary_df <- ungroup(intrWUE_AE_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_intrWUE_AE = create.my.cld("intrWUE_AE")) %>% 
  as.data.frame()

instWUE_AGs_summary_df <- ungroup(instWUE_AGs_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_instWUE_AGs = create.my.cld("instWUE_AGs")) %>% 
  as.data.frame()


create.my.cld_lwp <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova.Y <- aov(form, data = wp_final_df_lwp)
  
  tukey.Y <- TukeyHSD(anova.Y)
  Y.cld <- multcompLetters4(anova.Y, tukey.Y)
  
  Y.cld <- as.data.frame.list(Y.cld$`Species`)
  cld.Y <- Y.cld[order(rownames(Y.cld)), "Letters"]
  
  rep(cld.Y, each = nlevels(wp_final_df_lwp$Treatment_factor))
}

my.aov_lwp <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova(aov(form, data = wp_final_df_lwp))
}


LWP_summary_df <- ungroup(LWP_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_LWP = create.my.cld_lwp("LWP")) %>% 
  as.data.frame()

create.my.cld_pd <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova.Y <- aov(form, data = wp_final_df_pd)
  
  tukey.Y <- TukeyHSD(anova.Y)
  Y.cld <- multcompLetters4(anova.Y, tukey.Y)
  
  Y.cld <- as.data.frame.list(Y.cld$`Species`)
  cld.Y <- Y.cld[order(rownames(Y.cld)), "Letters"]
  
  rep(cld.Y, each = nlevels(wp_final_df_pd$Treatment_factor))
}

my.aov_pd <- function(Y) {
  form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
  anova(aov(form, data = wp_final_df_pd))
}

PD_summary_df <- ungroup(PD_summary) %>%
  arrange(Species) %>%
  mutate(Tukey_PD = create.my.cld_pd("PD")) %>% 
  as.data.frame()



final_summary_df <- full_join(A_summary_df, Ci_summary_df, by = c("Species", "Treatment")) %>%
  full_join(gsw_summary_df, by = c("Species", "Treatment")) %>%
  full_join(E_summary_df, by = c("Species", "Treatment")) %>%
  full_join(Tleaf_summary_df, by = c("Species", "Treatment")) %>%
  full_join(intrWUE_AE_summary_df, by = c("Species", "Treatment")) %>%
  full_join(instWUE_AGs_summary_df, by = c("Species", "Treatment")) %>%
  full_join(LWP_summary_df, by = c("Species", "Treatment")) %>%
  full_join(PD_summary_df, by = c("Species", "Treatment")) 

# Print the combined summary dataset
print(final_summary_df)


resp.vars <- c("A", "Ci",
               "gsw",  "E",
               "Tleaf", "intrWUE_AE",  "instWUE_AGs")


aov.latex.outputlist <- lapply(resp.vars, function(v) {
  anova_table <- my.aov(v) %>%
    xtable(caption = paste("Analysis of variance (ANOVA) of the main and effects and interaction effects of", v))
  print(anova_table, booktabs = FALSE, hline.after = FALSE, hline.before = TRUE, include.rownames = TRUE, comment = FALSE)
}) %>% 
  setNames(resp.vars)

resp.vars <- c("LWP")


aov.latex.outputlist <- lapply(resp.vars, function(v) {
  anova_table <- my.aov_lwp(v) %>%
    xtable(caption = paste("Analysis of variance (ANOVA) of the main and effects and interaction effects of", v))
  print(anova_table, booktabs = FALSE, hline.after = FALSE, hline.before = TRUE, include.rownames = TRUE, comment = FALSE)
}) %>% 
  setNames(resp.vars)

resp.vars <- c("PD")


aov.latex.outputlist <- lapply(resp.vars, function(v) {
  anova_table <- my.aov_pd(v) %>%
    xtable(caption = paste("Analysis of variance (ANOVA) of the main and effects and interaction effects of", v))
  print(anova_table, booktabs = FALSE, hline.after = FALSE, hline.before = TRUE, include.rownames = TRUE, comment = FALSE)
}) %>% 
  setNames(resp.vars)



write.csv(final_summary_df, file = "data_output/anova/final_summary_outremoved_final", row.names = FALSE)    

# merge total -------------------------------------------------------------
species_final_anova_data <- licor_final_df %>%
  left_join(wp_final_df, by = c("Species", "Treatment", "Treatment_factor", "Genotype")) %>%
  left_join(final_summary_df, by = c("Species", "Treatment"))


write.csv(species_final_anova_data, file = "data_output/anova/species_final_anova_data_outremoved_1", row.names = FALSE)

