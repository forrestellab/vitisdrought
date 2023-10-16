## Water Potential#----------
#make sure to check if entire data set or just subset (different .csv files and saving paths!

library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# ---- read file for entire data set
waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame 
#---- read file for subset 
waterpotentials_gathered <-read.csv("data/Subset/sub_wp.csv")

waterpotentials_gathered<- (waterpotentials_gathered%>%
                            mutate( species_geno = Genotype))%>%# unfortunately no "species" column 
                                                              # to merge together, should've been picked too!
                            mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))
##Leaf Water Potential----------

LWP_data<-waterpotentials_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
  filter(!is.na(LWP))%>%
  filter(!is.na(species_geno))%>%
  mutate(Date= recode(Date, "11/09" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/10" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/11" = "11/9-11"))%>%
  mutate(Date= recode(Date, "11/16" = "11/16-17"))%>%
  mutate(Date= recode(Date, "11/17" = "11/16-17"))%>%
  mutate(Date= recode(Date, "12/01" = "12/01-02"))%>%
  mutate(Date= recode(Date, "12/02" = "12/01-02"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(LWP = as.numeric(gsub(",", ".", LWP)))

# write.csv(LWP_data, file= "data/Subset/sub_small_LWP.csv")

#View(LWP_plot)  

genosLWP<-unique(LWP_data$species_geno)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")


for (i in genosLWP) {
  LWP_plot<-LWP_data%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = is.na(LWP), fill = Treatment))+ #does the is.na function help here?
    geom_boxplot(aes(y =-LWP))+
    theme_classic()+
    scale_x_discrete(limits = order1)+
    scale_y_continuous(name = "LWP (in bar)", limits=c(-20, -5), breaks = seq(-20, -5, by = 2))+
    ggtitle(paste("LWP of", i))
  print(LWP_plot)
  
  #path to save all files:
  #ggsave(paste0("fig_output/WP/Subset/LWP/LWP",i, ".png"))
  #ggsave(paste0("fig_output/WP/Subset/LWP/LWP",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/WP/Subset_small/LWP/LWP",i, ".png"))
  ggsave(paste0("fig_output/WP/Subset_small/LWP/LWP",i, ".pdf"))
  
  #there is still a warning message: saying that there were rows containing missing values removed
} 


##Stem Water Potential##
SWP_data<-waterpotentials_gathered%>%
  filter(!is.na(SWP))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(SWP = as.numeric(gsub(",", ".", SWP)))

write.csv(SWP_data, file= "data/Subset/sub_small_SWP.csv")

genosSWP<-unique(SWP_data$species_geno)

for (i in genosSWP) {
  SWP_plot<-SWP_data%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
    geom_boxplot(aes(y =-SWP))+
    scale_y_continuous(name = "SWP (in bar)", limits=c(-20, -5), breaks = seq(-20, -5, by = 2))+
    theme_classic()+
    ggtitle(paste("SWP of", i))
  print(SWP_plot)
  
  #path to save all files:
  #ggsave(paste0("fig_output/WP/SWP/SWP",i, ".png"))
  #ggsave(paste0("fig_output/WP/SWP/SWP",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/WP/Subset_small/SWP/SWP",i, ".png"))
  ggsave(paste0("fig_output/WP/Subset_small/SWP/SWP",i, ".pdf"))
}

##Predawn Water Potential## 

#NOTES: 11/18 was ALS

PD_data<-waterpotentials_gathered%>% #NA introduced by coercion ok
  filter(!is.na(PD))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date= recode(Date, "11/11" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/12" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/13" = "11/11-13"))%>%
  mutate(Date= recode(Date, "12/01" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/02" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/03" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(PD = as.numeric(gsub(",", ".", PD)))

#write.csv(PD_data, file= "data/Subset/sub_small_PD.csv")

#View(PD_plot) 
genosPD<-unique(PD_data$species_geno)
  
  for (i in genosPD) {
    PD_plot<-PD_data%>%
      filter(species_geno == i)%>%
      ggplot(aes(x = Date, Y = PD, fill = Treatment))+
      geom_boxplot(aes(y =-PD))+
      theme_classic()+
      scale_y_continuous(name = "PD (in bar)", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
      ggtitle(paste("PD of", i))
    
    print(PD_plot)
    
    #path to save all files:
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".png"))
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".pdf"))

    #path to save subset files: 
    ggsave(paste0("fig_output/WP/Subset_small/PD/PD",i, ".png"))
    ggsave(paste0("fig_output/WP/Subset_Small/PD/PD",i, ".pdf"))
}
