# Combining Soil Water Content and Water Usage

# General Notes -----------------------------------------------------------
 
    # still need to concider the weight of the bamboo-shoots in the calculations 

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

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data -------------------------------------------------------------

# read file for entire data set
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
            mutate( species_geno = paste(Species, Genotype, sep = "_")))

# read file for subset 
swclong<- read.csv("data/Subset/sub_swc.csv")


# SWC Calculations ---------------------------------------------------------
  #Pre-Water SWC: PWSWC = (PWW/mpot)*100 (Note: Bamboo-Shoots are not in the calculations)
  swclong$PSWC= swclong$PWW/swclong$mPot..kg.

  #to calculate Post-Water SWC: POSWC = (POWW/mpot)*100
  #if no watering happened: pre-water weight used instead of Post-Water Weight
  swclong<-swclong%>%
  mutate(POSWC = ((ifelse(is.na(POWW),PWW,POWW)))/swclong$mPot..kg.)


# Combine SWC Plot --------------------------------------------------------

accessions<-unique(swclong$species_geno)
for (i in accessions) {
  
## Pre-Water SWC Graphs---------------------------------------------------  
      error.pswc <- swclong %>%
      filter(species_geno == i)%>%
      group_by(Date, Treatment) %>%
      summarise(
        sd = sd(PSWC, na.rm = TRUE),
        se = (sd(PSWC)/sqrt(length(PSWC))), 
        len = mean(PSWC), 
        Treatment = Treatment,
        Date = Date)
    
    PSWCplot<-swclong%>% 
      filter(species_geno == i)%>%
      ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
      geom_point()+ 
      stat_summary(fun="mean",geom="line",size = 1)+ # or just points: stat_summary(fun="mean",geom="point",size = 2, shape = 3)+
      geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, 
                        ymin = len-se,
                        ymax = len+se),
                    color = "black",
                    position=position_dodge(width=0.5),
                    size = .3,
                    linetype = "solid",
                    data = distinct(error.pswc))+
      theme_classic()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1))+
      scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
      scale_y_continuous(name = "PSWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
      theme(legend.position="none")+
      xlab(label = "")
    
  
    #print(PSWCplot)
  
## Post-Water SWC Graphs---------------------------------------------------  
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
        scale_y_continuous(name = "POSWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
        theme(legend.position="none")+
        xlab(label = "")
      
      #print(POSWCplot)

##Water Use Graphs------------------------------------------------

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
        scale_y_continuous(name = "WU (L)", limits=c(0.0, 2.1), breaks = seq(0.0, 2.1, by = 0.2))+ 
        theme(legend.position="none")+
        xlab(label = "Date") 
     
       #print(wuplot)

## Merge PSWC,  POSWC,  WU Plot --------------------------------------------

  plot1 <-ggarrange(PSWCplot, NULL, POSWCplot, NULL, wuplot, ncol=1, nrow=5, heights =  c(3, 0.00001, 3, 0.00001, 3), 
                    common.legend = TRUE, legend="bottom",align = "v") #merge the 3 plots from above together

  combined <- annotate_figure(plot1, top = text_grob((paste("SWC & WU  of", i)), color = "black", face = "italic", size = 11)) 
  
  print(combined)

# Save Graph --------------------------------------------------------------

  #path to save entire files:
  #ggsave(paste0("fig_output/SWC/Combined/Combined",i, ".png"))
  #ggsave(paste0("fig_output/SWC/Combined/Combined",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset_small/SWC/Combined/Combined",i, ".png"))
  ggsave(paste0("fig_output/Subset_small/SWC/Combined/Combined",i, ".pdf"))
  
}

# how can I have one combined x-axis? + rremove("xlab")
# how can I decrease the distance between the plots? 
