####Pre- and Post- Water Soil Moisture Content Calculations----------
#make sure to check if entire data set or just subset (different .csv files and saving paths!)

library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

#set working directory

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# read file for entire data set
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
             mutate( species_geno = paste(Species, Genotype, sep = "_")))

# read file for subset 
#swclong<- read.csv("data/Subset/sub_swc.csv")


#to calculate Pre-Water SWC: PWSWC = (PWW/mpot)*100 (Note: Bamboo-Shoots are not in the calculations)

swclong$PSWC= swclong$PWW/swclong$mPot..kg.

#to calculate Post-Water SWC: POSWC = (POWW/mpot)*100
#if no watering happened: pre-water weight used instead of Post-Water Weight

swclong<-swclong%>%
  mutate(POSWC = ((ifelse(is.na(POWW),PWW,POWW)))/swclong$mPot..kg.)

#### Pre-Water SMC Graphs---------------------------------------------------  

accessions<-unique(swclong$species_geno) # unique() used to eliminate or delete the duplicate values/ rows present

for (i in accessions) {
  
  error.pswc <- swclong %>%
    filter(species_geno == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(PSWC, na.rm = TRUE),
      se = (sd(PSWC)/sqrt(length(PSWC))), #   #how to calculate standard error? (sd(PSWC)/sqrt(length(PSWC)))
      len = mean(PSWC), #what does this part stand for?
      Treatment = Treatment,
      Date = Date)
  
  
  PSWCplot<-swclong%>% 
    filter(species_geno == i)%>%
    ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ #use point instead of "line", how can I plot the mean and error
    # how to plot regression curve? and mean sqrt?
    stat_summary(fun="mean",geom="line",size = 1)+ # or just points: stat_summary(fun="mean",geom="point",size = 2, shape = 3)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, #is standard error here correct written?
                      ymin = len-se,
                      ymax = len+se),
                 color = "black",
                position=position_dodge(width=0.5),
                size = .3,
                linetype = "solid",
                data = distinct(error.pswc))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+  #is there a way to name it day x rather than the actual date?
    xlab(label = "Date")+
    scale_y_continuous(name = "SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
    theme(legend.position="bottom")+
    ggtitle(paste("Pre- SWC of", i))
  print(PSWCplot)
  
  #path to save all files: 
  #ggsave(paste0("fig_output/SWC/PSWC/PSWC",i, ".png"))
  #ggsave(paste0("fig_output/SWC/PSWC/PSWC",i, ".pdf"))

  #path to save subset files: 
  #ggsave(paste0("fig_output/SWC/Subset/PSWC/PSWC",i, ".png"))
  #ggsave(paste0("fig_output/SWC/Subset/PSWC/PSWC",i, ".pdf"))
  }

#### Post-Water SMC Graphs---------------------------------------------------  

accessions<-unique(swclong$species_geno) 

for (i in accessions) {
  
  error.poswc <- swclong %>%
    filter(species_geno == i)%>% 
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(POSWC, na.rm = TRUE), 
      se = (sd(POSWC)/sqrt(length(POSWC))), 
      len = mean(POSWC),
      Treatment = Treatment,
      Date = Date)
  
  
  POSWCplot<-swclong%>% 
    filter(species_geno == i)%>%
    ggplot(aes(y =POSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ 
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  linetype = "solid",
                  data = distinct(error.poswc))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    scale_y_continuous(name = "SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
    theme(legend.position="bottom")+
    ggtitle(paste("Post- SWC of", i))
      
  print(POSWCplot)
  
  #path to save all files: 
  #ggsave(paste0("fig_output/SWC/POSWC/POSWC",i, ".png"))
  #ggsave(paste0("fig_output/SWC/POSWC/POSWC",i, ".pdf"))
  
  #path to save subset files: 
  #ggsave(paste0("fig_output/SWC/Subset/POSWC/POSWC",i, ".png"))
  #ggsave(paste0("fig_output/SWC/Subset/POSWC/POSWC",i, ".pdf"))
  
}

#Plottig WU Graph -------------

accessions<-unique(swclong$species_geno)

for (i in accessions) {
  
  error.wu <- swclong %>%
    filter(species_geno == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WU, na.rm = TRUE),
      se = (sd(WU)/sqrt(length(WU))),
      len = mean(WU),
      Treatment = Treatment,
      Date = Date)
  
  
  wuplot<-swclong%>%
    filter(species_geno == i)%>%
    ggplot(aes(y =WU, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+
    stat_summary(fun="mean",geom="line",size = 1)+
    #stat_smooth(geom="line",size = 1.5)+
    
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  #linetype = "solid",
                  data = distinct(error.wu))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    scale_y_continuous(name = "WU (L)", limits=c(0.0, 2.8), breaks = seq(0.0, 2.8, by = 0.2))+ 
    theme(legend.position="bottom")+
    ggtitle(paste("WU of", i))
  print(wuplot)
  
  #path to save all files: 
  #ggsave(paste0("fig_output/SWC/WU/WU",i, ".png"))
  #ggsave(paste0("fig_output/SWC/WU/WU",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/SWC/Subset/WU/WU",i, ".png"))
  ggsave(paste0("fig_output/SWC/Subset/WU/WU",i, ".pdf"))
}

