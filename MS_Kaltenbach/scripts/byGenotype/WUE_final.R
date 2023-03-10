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

WUE_final <-read.csv("data/Subset/new/joined_WUE.csv")
WUE_final<- (WUE_final%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                    "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

# to select Wanted Days -------------------------------------------------------------

WUE_final <- WUE_final%>% 
              group_by(Date) %>%
              filter(any(Date == c("12/06-09")))

# Plot Graph --------------------------------------------------------------

WUE_final_Plot<-WUE_final%>%
                ggplot(aes(x = species_geno, Y = value, fill = Treatment))+
                geom_boxplot(aes(y = value))+
                theme_classic()+
                facet_grid(facets = TypeWUE ~ ., scales = "free")+
                ylab("")+
                xlab("")+
                #scale_y_continuous(name = "", limits=c(-500, 10000), breaks = seq(-500, 10000, by = 1000))+ 
                theme(legend.position="bottom") +
                ggtitle("Water Use Efficency by Species 12/06-09")


  print(WUE_final_Plot)
  
  # Save Plot ---------------------------------------------------------------

  #path to save subset files: 
  
  ggsave(paste0("fig_output/Subset_small/bygenotype/WUE_final/WUE_final", ".png"))
  ggsave(paste0("fig_output/Subset_small/bygenotype/WUE_final/WUE_final", ".pdf"))
  


#write.csv(joined_WUE , file= "data/Subset/new/joined_WUE.csv")