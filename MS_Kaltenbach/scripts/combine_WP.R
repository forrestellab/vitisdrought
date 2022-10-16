## Water Potentials Cambined in 1 Plot#----------
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
#waterpotentials_gathered <-read.csv("data/Subset/sub_wp.csv")

# CODE STARTS HERE: 
waterpotentials_gathered<- (waterpotentials_gathered%>%
                              mutate( species_geno = Genotype))%>%# unfortunately no "species" column 
  # to merge together, should've been picked too!
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))

##  Mutations

#----- Mutation Leaf Water Potential----------

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
        mutate(LWP = as.numeric(gsub(",", ".", LWP))) # changed to substitute the "," with a "." so code is running, otherwise invalid data

#View(LWP_plot)  

##------Stem Water Potential Mutation ----##
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
  
  ##-----Predawn Water Potential## 
  
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
      

      
# -------  Plotting WP
genosLWP<-unique(LWP_data$species_geno)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")
genosSWP<-unique(SWP_data$species_geno)
genosPD<-unique(PD_data$species_geno)


#library(foreach)
#foreach(i = genosLWP, j = genosSWP, k =genosPD) %do% {

for (i in genosLWP, j in genosSWP, k in genosPD ) {

  # Plotting LWP
  LWP_plot<-LWP_data%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = is.na(LWP), fill = Treatment))+ #does the is.na function help here?
    geom_boxplot(aes(y =-LWP))+
    theme_classic()+
    scale_x_discrete(limits = order1)+
    scale_y_continuous(name = "WP (in bar)", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    xlab(NULL)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position="none")
  #print(LWP_plot)
  

##--- Plotting Stem Water Potential##



  SWP_plot<-SWP_data%>%
    filter(species_geno == j)%>%
    ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
    geom_boxplot(aes(y =-SWP))+
    scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    theme_classic()+
    xlab(NULL)+
    theme(legend.position="none")
  #print(SWP_plot)


##Plotting Predawn Water Potential## 


  PD_plot<-PD_data%>%
    filter(species_geno == k)%>%
    ggplot(aes(x = Date, Y = PD, fill = Treatment))+
    geom_boxplot(aes(y =-PD))+
    theme_classic()+
    scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    xlab(NULL)+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    theme(legend.position="none")
  
  #print(PD_plot)
  
  ###--Merging the PSWC, POSWC and WU plots 
  
  plot1 <-ggarrange(LWP_plot, NULL, SWP_plot, NULL, PD_plot, ncol=5, nrow=1, widths =  c(3, 0.00001, 3, 0.00001, 3), common.legend = TRUE, legend="bottom",align = "v", labels = "LWP", "SWP", "PD") #merge the 3 plots from above together
  
  
  combined <- annotate_figure(plot1, top = text_grob((paste("SWC & WU  of", i)), color = "black", face = "italic", size = 11)) #add title as genotype
  
  print(combined)
  #path to save all files:
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".pdf"))
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".png"))
  
  #path to save subset files: 
  #ggsave(paste0("fig_output/WP/Subset/PD/PD",i, ".png"))
  #ggsave(paste0("fig_output/WP/Subset/PD/PD",i, ".pdf"))
  
  
}
