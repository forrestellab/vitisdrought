# Gs of Final Day


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

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# Final Date --------------------------------------------------------------


# Import Data -------------------------------------------------------------
gsw_final<- read.csv("data/Subset/new/Porometer.csv")

  # to select Wanted Days -------------------------------------------------------------
gsw_final <- gsw_final%>% 
              group_by(Date) %>%
              filter(any(Date == c("12/02")))


  # Plot Graph --------------------------------------------------------------

gsw_final_plot<-gsw_final%>%
                ggplot(aes(x = species_geno, Y = gsw_porometer, fill = Treatment))+
                geom_boxplot(aes(y =gsw_porometer))+
                theme_classic()+
                xlab("Genotype")+
                ylab("Gs (mmol m−2 s−1)")+
                theme(legend.position="bottom") +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                ggtitle("Gs of 12/02")
print(gsw_final_plot)

  # Save Graph --------------------------------------------------------------
      #path to save final day files: 
      ggsave(paste0("fig_output/Subset/bygenotype/GSWfinal/GSWfinal", ".png"))
      ggsave(paste0("fig_output/Subset/bygenotype/GSWfinal/GSWfinal", ".pdf"))



# two last Dates ----------------------------------------------------------

  # Import Data -------------------------------------------------------------
gsw_final<- read.csv("data/Subset/new/Porometer.csv")

  # to select Wanted Days -------------------------------------------------------------
gsw_final <- gsw_final%>% 
  group_by(Date) %>%
  filter(any(Date == c("11/23-24", "12/02")))

  # lengthen Data -------------------------------------------------------------
gsw_combined_final<- gsw_final %>% 
                      mutate( Genotype_Date = paste(Genotype, Date, sep = "_"))


  # Plot Graph --------------------------------------------------------------

gsw_final_plot<-gsw_combined_final%>%
                ggplot(aes(x = Genotype_Date, Y = gsw_porometer, fill = Treatment))+
                geom_boxplot(aes(y = gsw_porometer))+
                theme_classic()+
                xlab("Genotype and Date")+
                ylab("Gs (mmol m−2 s−1)")+
                theme(legend.position="bottom") +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                ggtitle("Gs of 11/23-24 & 12/02")
print(gsw_final_plot)

# Save Graph --------------------------------------------------------------
        #path to save final day files: 
        ggsave(paste0("fig_output/Subset/bygenotype/GSWtwofinaldays/GSWtwofinaldays", ".png"))
        ggsave(paste0("fig_output/Subset/bygenotype/GSWtwofinaldays/GSWtwofinaldays", ".pdf"))

  # all combined  ----------------------------------------------------------

  # Import Data -------------------------------------------------------------
gsw_combined<- read.csv("data/Subset/new/Porometer.csv")

  # Plot Graph --------------------------------------------------------------

gsw_combined_plot<-gsw_combined%>%
                ggplot(aes(x = species_geno, Y = gsw_porometer, fill = Treatment))+
                geom_boxplot(aes(y = gsw_porometer))+
                theme_classic()+
                xlab("Genotype and Date")+
                ylab("Gs (mmol m−2 s−1)")+
                theme(legend.position="bottom") +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                ggtitle("Gs all dates by Species")
print(gsw_combined_plot)

  # Save Graph --------------------------------------------------------------
        #path to save final day files: 
        ggsave(paste0("fig_output/Subset/bygenotype/GSWalldates/GSWalldates", ".png"))
        ggsave(paste0("fig_output/Subset/bygenotype/GSWalldates/GSWalldates", ".pdf"))

