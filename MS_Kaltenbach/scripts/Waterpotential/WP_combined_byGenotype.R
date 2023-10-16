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
WP_final<- (WP_final%>%
               filter(species_geno %in% c("9018", "T52", "b40-14", "b42-34", 
                                      "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

# to select Wanted Days -------------------------------------------------------------

WP_final <- WP_final%>% 
            group_by(Date_group) %>%
            filter(any(Date_group == c("Dec 11 to 18")))

WP_final <- WP_final %>% 
  group_by(WPType) %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()


# Plot Graph --------------------------------------------------------------

  new_plot<-WP_final%>%
    ggplot(aes(x = species_geno, Y = WP, fill = Treatment))+
    geom_boxplot(aes(y =-WP))+
    #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    theme_classic()+
    xlab("Genotype")+
    ylab("WP (MPa)")+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_rect(fill = "gray")) +
 geom_vline(
             aes(xintercept = as.numeric(factor(species_geno)) + 0.5), 
          linetype = "dashed", color = "grey") + 
  geom_hline(yintercept = Inf, linetype = "dashed", color = "grey")+
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    facet_grid(facets = WPType ~ .)+
    ggtitle("WP Dec 11 to 18")
  print(new_plot)
  
  
  
# Save Graph --------------------------------------------------------------
  #path to save final day files: 
  ggsave(paste0("fig_output/Subset_small/bygenotype/WPcombined/WPcombined", ".png"))
  ggsave(paste0("fig_output/Subset_small/bygenotype/WPcombined/WPcombined", ".pdf"))

