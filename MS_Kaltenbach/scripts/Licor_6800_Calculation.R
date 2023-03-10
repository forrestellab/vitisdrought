#Licor 6800 Point Measurement Graphs


# General Notes -----------------------------------------------------------

# Looking at a variation between control and drought
# Dates were recorded according to the Master Script of Nico
# Genotype "V37-96" was excluded according to the Master Script of Nico

#there is a second version of plotting the Licor 6800 Measurements as histograms in the Masterscript from Nico

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

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

# Import data -------------------------------------------------------------

# Entire File
gswclean_6800<-read.csv("data/gsw_6800_cleaned.csv")
gswclean_6800<- gswclean_6800%>%
                mutate(species_geno = Genotype)

# Subset File
gswclean_6800 <-read.csv("data/Subset/sub_gswc_6800.csv")

# Clean Data --------------------------------------------------------------

gswclean_6800<-gswclean_6800%>%
  filter(!is.na(A))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))


# ifelse for Date ---------------------------------------------------------

gswclean_6800$Date <- ifelse(gswclean_6800$Date=="10/26"|gswclean_6800$Date=="10/27" |
                              gswclean_6800$Date=="10/28" |gswclean_6800$Date=="10/29","10/26-29",
                      ifelse(gswclean_6800$Date=="11/12"|gswclean_6800$Date=="11/13","11/12-13",
                      ifelse(gswclean_6800$Date=="12/06"|gswclean_6800$Date=="12/07"
                            |gswclean_6800$Date=="12/08" |gswclean_6800$Date=="12/09","12/06-09",
                             gswclean_6800$Date)))

#write.csv(gswclean_6800, file= "data/Subset/new/Licor.csv")

write.csv(SWP_data, file= "data/Subset/sub_small_Licor.csv")

# Plot Graph --------------------------------------------------------------
  
genos<-unique(gswclean_6800$species_geno)

for (i in genos) {
  graph<-gswclean_6800%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = A, fill = Treatment))+
    geom_boxplot(aes(y =A))+
    theme_classic()+
    ggtitle(i)
  print(graph)

# Save Graph --------------------------------------------------------------
  #path to save all files: 
  #ggsave(paste0("fig_output/Licor6800/Licor6800",i, ".png"))
  #ggsave(paste0("fig_output/Licor6800/Licor6800",i,".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset_small/Licor6800/Licor6800",i, ".png"))
  ggsave(paste0("fig_output/Subset_small/Licor6800/Licor6800",i, ".pdf"))
}
