
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# WaterUse ----------------------------------------------------------------

swc_csv<-read.csv("data/WaterUse_SWC_Clean.csv") 
sub_swc<- (swc_csv%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                   "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
              mutate( species_geno = paste(Species, Genotype, sep = "_")))

dir.create("data/Subset")
write.csv(sub_swc, file= "data/Subset/sub_swc.csv")


# Waterpotential ----------------------------------------------------------
waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") 
sub_wp<- (waterpotentials_gathered%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                    "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
             mutate( species_geno = Genotype)) # unfortunately no "species" column to merge together, should've been picked too!

write.csv(sub_wp, file= "data/Subset/sub_wp.csv")

# Porometer ---------------------------------------------------------------

gswclean<-read.csv("data/gsw_porometer_cleaned.csv")

sub_geswc<- (gswclean%>%
            filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                   "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
            mutate( species_geno = Genotype)) # unfortunately no "species" column to merge together, should've been picked too!

write.csv(sub_geswc, file= "data/Subset/sub_gswc.csv")


# Licor6800 Data ----------------------------------------------------------
gswclean_6800<-read.csv("data/gsw_6800_cleaned.csv")

sub_geswc_6800<- (gswclean_6800%>%
               filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                      "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
               mutate( species_geno = Genotype)) # unfortunately no "species" column to merge together, should've been picked too!

write.csv(sub_geswc_6800, file= "data/Subset/sub_gswc_6800.csv")