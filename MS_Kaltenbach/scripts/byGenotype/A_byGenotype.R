#Waterpotentials Final Day


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

# Last Day -------------------------------------------------------------

  # Subset File
gswclean_6800 <-read.csv("data/Subset/new/Licor.csv")

gswclean_6800<- (gswclean_6800%>%
              filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                     "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

  # to select Wanted Days -------------------------------------------------------------
gswclean_6800_final <- gswclean_6800%>% 
                        group_by(Date) %>%
                        filter(any(Date == c("12/06−09")))

  # Plot Graph --------------------------------------------------------------

gswclean_6800_final_plot<-gswclean_6800%>%
                          ggplot(aes(x = species_geno, Y = A, fill = Treatment))+
                          geom_boxplot(aes(y = A))+
                          theme_classic()+
                          xlab("Genotype")+
                          ylab("A (mol m−2 s−1)")+
                          theme(legend.position="bottom") +
                          theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                          ggtitle("A of 12/06−09")
                        print(gswclean_6800_final_plot)

# Save Graph --------------------------------------------------------------
#path to save final day files: 
ggsave(paste0("fig_output/Subset_small/bygenotype/Licorfinal/Licorfinal", ".png"))
ggsave(paste0("fig_output/Subset_small/bygenotype/Licorfinal/Licorfinal", ".pdf"))



# all combined  ----------------------------------------------------------

  # Import Data -------------------------------------------------------------
gswclean_6800_combined<- read.csv("data/Subset/new/Licor.csv")

gswclean_6800_combined<- (gswclean_6800_combined%>%
                   filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                          "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

  # Plot Graph --------------------------------------------------------------

gswclean_6800_combined_plot<-gswclean_6800_combined%>%
                ggplot(aes(x = species_geno, Y = A, fill = Treatment))+
                geom_boxplot(aes(y = A))+
                theme_classic()+
                xlab("Genotype and Date")+
                ylab("A (mol m−2 s−1)")+
                theme(legend.position="bottom") +
                theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                ggtitle("A all dates by Species")
print(gswclean_6800_combined_plot)

  # Save Graph --------------------------------------------------------------
#path to save final day files: 
ggsave(paste0("fig_output/Subset_small/bygenotype/Licoralldates/Licoralldates", ".png"))
ggsave(paste0("fig_output/Subset_small/bygenotype/Licoralldates/Licoralldates", ".pdf"))

