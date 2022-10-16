
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

#---WaterUse_SWC_Clean.csv-----

swc_csv<-read.csv("data/WaterUse_SWC_Clean.csv") 

sub_swc<- (swc_csv%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                   "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
              mutate( species_geno = paste(Species, Genotype, sep = "_")))

dir.create("data/Subset")
write.csv(sub_swc, file= "data/Subset/sub_swc.csv")

#---Clean_WP.csv
waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") 
sub_wp<- (waterpotentials_gathered%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                    "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
             mutate( species_geno = Genotype)) # unfortunately no "species" column to merge together, should've been picked too!

write.csv(sub_wp, file= "data/Subset/sub_wp.csv")


#--- Porometer Data
gswclean<-read.csv("data/gsw_porometer_cleaned.csv")


