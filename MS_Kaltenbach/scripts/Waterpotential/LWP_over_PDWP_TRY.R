#Leaf Waterpotential / Predawn Waterpotential


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

# Subset
WP_gathered <- read.csv("data/Subset/sub_wp.csv")
WP_gathered <- WP_gathered[, c(2, 12, 4:8)]

# Clean Data
WP_gathered$Date <- as.factor(WP_gathered$Date)
WP_gathered$Month <- substr(WP_gathered$Date, 1, 2)
WP_gathered$Day <- substr(WP_gathered$Date, 4, 5)
WP_gathered$Date <- paste0(WP_gathered$Month, "/", WP_gathered$Day)
WP_gathered <- WP_gathered[, c(1:7)]
WP_gathered$LWP <- as.numeric(gsub(",", ".", WP_gathered$LWP))
WP_gathered$SWP <- as.numeric(gsub(",", ".", WP_gathered$SWP))
WP_gathered$PD <- as.numeric(gsub(",", ".", WP_gathered$PD))

# Calculate Indices
leafwp_predawn <- WP_gathered %>%
  mutate("leafwp_predawn" = LWP / PD) %>%
  select(-c(SWP)) %>%
  na.omit()


# ifelse for Date------------------------------------------------------------------

# combined 12/01 and 12/02 to 12/02 and 12/11 - 12/18 to 12/18 

leafwp_predawn$Date_group <- ifelse( leafwp_predawn$Date=="12/1"| 
                                       leafwp_predawn$Date=="12/2","12/02",
                                           ifelse( leafwp_predawn$Date=="12/11"|
                                                     leafwp_predawn$Date=="12/15"|
                                                     leafwp_predawn$Date=="12/16"| 
                                                    leafwp_predawn$Date=="12/17"|
                                                    leafwp_predawn$Date=="12/18","12/18",
                                                   leafwp_predawn$Date))

# Plot Ratio LeafWP over PreDawn WP per Date -------------------------------------------------------------

leafwp_predawn_plot<-leafwp_predawn %>%
                      ggplot(aes(x = species_geno, y = leafwp_predawn, fill = Treatment))+
                      geom_boxplot(aes(y = leafwp_predawn))+
                      theme_classic()+
                      theme(legend.position="bottom")+
                      scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
                      facet_grid(facets = Date_group ~ ., scales = "free_y") +
                      theme(axis.text.x = element_text(angle = 30, hjust = 1))+
                      ggtitle("LeafWP over PreDawn WP combined")

print(leafwp_predawn_plot)

#path to save subset files: 
ggsave(paste0("fig_output/WP/leafwp_predawn_combined/leafwp_predawn_combined", ".png"))
ggsave(paste0("fig_output/WP/leafwp_predawn_combined/leafwp_predawn_combined", ".pdf"))



