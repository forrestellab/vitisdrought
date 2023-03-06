#Waterpotentials Final Day


# General Notes -----------------------------------------------------------
    # this file is to just plot the final day of LWP, PD and SWP to compare among species

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

# Import Data -------------------------------------------------------------

WP_final <-read.csv("data/Subset/new/combined_WP.csv")

# to select Wanted Days -------------------------------------------------------------

WP_final <- WP_final%>% 
            group_by(Date_group) %>%
            filter(any(Date_group == c("Dec 11 to 18")))


# Plot Graph --------------------------------------------------------------

  new_plot<-WP_final%>%
    ggplot(aes(x = species_geno, Y = WP, fill = Treatment))+
    geom_boxplot(aes(y =-WP))+
    #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    theme_classic()+
    xlab("Genotype")+
    ylab("WP (MPa)")+
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    facet_grid(facets = WPType ~ .)+
    ggtitle("WP Dec 11 to 18")
  print(new_plot)
  
# Save Graph --------------------------------------------------------------
  #path to save final day files: 
  ggsave(paste0("fig_output/Subset/bygenotype/WPcombined/WPcombined", ".png"))
  ggsave(paste0("fig_output/Subset/bygenotype/WPcombined/WPcombined", ".pdf"))

