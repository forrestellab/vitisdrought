
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

#set working directory

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

####Soil Moisture Content Calculations----------

# read file
swc_csv<-read.csv("data/WaterUse_SWC_Clean.csv") 

#to calculate Pre-Water SWC: PWSWC = (PWW/mpot)*100 (Note: Bamboo-Shoots are not in the calculations)

swc_csv$PSWC= swc_csv$PWW/swc_csv$mPot..kg.

#to calculate Post-Water SWC: POSWC = (POWW/mpot)*100

swclong<-swc_csv%>%
  mutate(POSWC = ((ifelse(is.na(POWW),PWW,POWW)))/swc_csv$mPot..kg.) #if no watering happened: pre-water weight used instead of Post-Water Weight (otherwise: easier formula would be (swclong$POSWC= swclong$POWW/swclong$mPot..kg) -> but would give out NA for values not available) 

#### Pre-Water SMC Graphs---------------------------------------------------  

accessions<-unique(swclong$Genotype)

for (i in accessions) {
  
  
  error.pswc <- swclong %>% #create individual error. objects for pswc
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(PSWC, na.rm = TRUE), 
      se = (sd(PSWC)/sqrt(length(PSWC))), 
      len = mean(PSWC), 
      Treatment = Treatment,
      Date = Date)
  
  PSWCplot<-swclong%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  linetype = "dotted",
                  data = distinct(error.pswc))+
    theme_classic()+
    theme(legend.position="none",  #no legend shown since we want to combine all at the bottom
    axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+ 
    xlab(label = "")  #tricked the x-axis in an "empty" field so I only print it once But I am sure, there is a better solution

    
  
  error.poswc <- swclong %>%
    filter(Genotype == i)%>% 
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(POSWC, na.rm = TRUE), 
      se = (sd(POSWC)/sqrt(length(POSWC))), 
      len = mean(POSWC),
      Treatment = Treatment,
      Date = Date)
  
  
  POSWCplot<-swclong%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =POSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ 
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  linetype = "dotted",
                  data = distinct(error.poswc))+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = " ")
  
 

###Water Use Graphs------------------------------------------------

  error.wu <- swclong %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WU, na.rm = TRUE),
      se = (sd(WU)/sqrt(length(WU))),
      len = mean(WU),
      Treatment = Treatment,
      Date = Date)
  
  wuplot<-swclong%>%
    filter(Genotype == i)%>%
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
                  #linetype = "dotted",
                  data = distinct(error.wu))+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date") #here I actually print the label so I can see it just once bit it doesn't change the distance between the plots later on

  
###--merging the plots 
  
  plot1 <-ggarrange(PSWCplot, POSWCplot, wuplot, ncol=1, nrow=3, common.legend = TRUE, legend="bottom") #merge the 3 plots from above together

  combined <- annotate_figure(plot1, top = text_grob((paste("SWC & WU  of", i)), color = "black", face = "italic", size = 13)) #add title as genotype
                  
  print(combined)
  
  ggsave(paste0("fig_output/SWC/Combined/Combined",i, ".png"))
  ggsave(paste0("fig_output/SWC/Combined/Combined",i, ".pdf"))
}



  


