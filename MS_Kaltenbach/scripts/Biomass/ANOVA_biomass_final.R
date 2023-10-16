
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
library(xtable)

remove.packages("multcomp")

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Harvest Graphs####

harvest<-read_csv("data/2020_GHDD_Harvest_Data_mk.csv")
# Import data
cumWU <- read.csv("data/Subset/cumWu.csv")
#harvest <- read.csv("data/Subset/Harvest.csv")
leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

harvest <- harvest  %>% select(ID, trt, genotype, species, canopy_biomass, root_biomass)

genotypes_to_keep <- c("9018", "b40-14", "TXNM0821", "b42-34", "T52", "NY1", "T48", "Vru42", "V60-96")

harvest <- harvest %>%
  filter(genotype %in% genotypes_to_keep)  %>%
  distinct(ID, .keep_all = TRUE)%>%
  rename(Treatment = trt, Genotype = genotype)


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
  #filter(Species != "vulpina") %>%
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
     summarise(mean_leafarea_wu = mean(`leafarea_wu`), sd_leafarea_wu = sd(`leafarea_wu`)) %>%
     arrange(desc(mean_leafarea_wu))
   print(leafarea_wu_summary)
   
   # For "canopy_biomass/wu" column
   canopy_biomass_wu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_canopy_biomass_wu = mean(`canopy_biomass_wu`), sd_canopy_biomass_wu = sd(`canopy_biomass_wu`)) %>%
     arrange(desc(mean_canopy_biomass_wu))
   print(canopy_biomass_wu_summary)
   
   # For "root_biomass/wu" column
   root_biomass_wu_summary <- group_by(biomass, Species, Treatment) %>%
     summarise(mean_root_biomass_wu = mean(`root_biomass_wu`), sd_root_biomass_wu = sd(`root_biomass_wu`)) %>%
     arrange(desc(mean_root_biomass_wu))
   print(root_biomass_wu_summary)
   
   

# perform ANOVA -----------------------------------------------------------

   library(multcompView)
   library(multcomp)
   
   
   
   create.my.cld <- function(Y) {
     form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
     anova.Y <- aov(form, data = biomass)
     #  print(summary(anova.Y))
     
     tukey.Y <- TukeyHSD(anova.Y)
     Y.cld <- multcompLetters4(anova.Y, tukey.Y)
     
     ############# Hier war m. E. ein Zuordnungsfehler der Letters zu den Species!!
     Y.cld <- as.data.frame.list(Y.cld$`Species`)
     cld.Y <- Y.cld[order(rownames(Y.cld)), "Letters"]
     #cld.Y <- as.data.frame.list(Y.cld$`Species`)$Letters
     
     rep(cld.Y, each = nlevels(biomass$Treatment_factor))
   }
   
   my.aov <- function(Y) {
     form <- as.formula(paste(Y, "~ Species * Treatment_factor"))
     anova(aov(form, data = biomass))
   }
   
   
   
   
   sumwu_summary_df <- ungroup(sumwu_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_sumwu = create.my.cld("sumwu")) %>% 
     as.data.frame()
   
   root_biomass_summary_df <- ungroup(root_biomass_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_root_biomass = create.my.cld("root_biomass")) %>% 
     as.data.frame()
   
   canopy_biomass_summary_df <- ungroup(canopy_biomass_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_canopy_biomass = create.my.cld("canopy_biomass")) %>% 
     as.data.frame()
   
   ratio_canopy_root_summary_df <- ungroup(ratio_canopy_root_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_ratio_canopy_root = create.my.cld("ratio_canopy_root")) %>% 
     as.data.frame()

   TotalLeafArea_summary_df <- ungroup(TotalLeafArea_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_TotalLeafArea = create.my.cld("TotalLeafArea")) %>% 
     as.data.frame()
   
   leafarea_wu_summary_df <- ungroup(leafarea_wu_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_leafarea_wu = create.my.cld("leafarea_wu")) %>% 
     as.data.frame()
   
   canopy_biomass_wu_summary_df <- ungroup(canopy_biomass_wu_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_canopy_biomass_wu = create.my.cld("canopy_biomass_wu")) %>% 
     as.data.frame()
   
   root_biomass_wu_summary_df <- ungroup(root_biomass_wu_summary) %>%
     arrange(Species) %>%
     mutate(Tukey_root_biomass_wu = create.my.cld("root_biomass_wu")) %>% 
     as.data.frame()

  
   
   resp.vars <- c("sumwu",
                  "root_biomass",
                  "canopy_biomass",
                  "ratio_canopy_root",
                  "TotalLeafArea",
                  "leafarea_wu",
                  "canopy_biomass_wu",
                  "root_biomass_wu" )
   
   
   aov.latex.outputlist <- lapply(resp.vars, function(Y) {
     anova_table <- my.aov(Y) %>%
       xtable(caption = paste("Analysis of variance (ANOVA) of the main and effects and interaction effects of", Y))
     print(anova_table, booktabs = FALSE, hline.after = FALSE, hline.before = TRUE, include.rownames = TRUE, comment = FALSE)
   }) %>% 
     setNames(resp.vars)
   
   
   

# create combined file  ---------------------------------------------------

   biomass_anova<- biomass %>%
     merge(sumwu_summary_df, by = c("Species", "Treatment")) %>%
     merge(canopy_biomass_summary_df, by = c("Species", "Treatment")) %>%
     merge(root_biomass_summary_df, by = c("Species", "Treatment")) %>%
     merge(ratio_canopy_root_summary_df, by = c("Species", "Treatment")) %>%
     merge(TotalLeafArea_summary_df, by = c("Species", "Treatment")) %>%
     merge(leafarea_wu_summary_df, by = c("Species", "Treatment")) %>%
     merge(canopy_biomass_wu_summary_df, by = c("Species", "Treatment")) %>%
     merge(root_biomass_wu_summary_df, by = c("Species", "Treatment")) 
   
   write.csv( biomass_anova, file = "data_output/biomass/biomass_anova_1.csv", row.names = FALSE)