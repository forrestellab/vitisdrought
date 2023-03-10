# Water Use Efficiency 

# General Notes -----------------------------------------------------------

# Water Use Efficiency (WUE) = net assimilation-photosynthetic rate (An) / stomatal conductance (gs)

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

# intrinsic WUE Calculation ---------------------------------------------------------

gswclean_6800 <- gswclean_6800 %>%
  mutate(WUE= A / gsw_6800)

# instantaneous WUE Calculation ---------------------------------------------------------

gswclean_6800 <- gswclean_6800 %>%
  mutate(WUE= A / gsw_6800)

#write.csv(gswclean_6800, file= "data/Subset/new/WUE.csv")

# Plot Graph --------------------------------------------------------------

genos<-unique(gswclean_6800$species_geno)

for (i in genos) {
  graph<-gswclean_6800%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = WUE, fill = Treatment))+
    geom_boxplot(aes(y =WUE))+
    theme_classic()+
    ylab(label = "intrinsic WUE (A/Gs)")+
    ggtitle(i)
  print(graph)
  
  # Save Plot ---------------------------------------------------------------
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset_small/inWUE/inWUE",i, ".png"))
  ggsave(paste0("fig_output/Subset_small/inWUE/inWUE",i, ".pdf"))
  
}