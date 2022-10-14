
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
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 

#to calculate Pre-Water SWC: PWSWC = (PWW/mpot)*100 (Note: Bamboo-Shoots are not in the calculations)

swclong$PSWC= swclong$PWW/swclong$mPot..kg.

#to calculate Post-Water SWC: POSWC = (POWW/mpot)*100
# swclong$POSWC= swclong$POWW/swclong$mPot..kg

#if no watering happened: pre-water weight used instead of Post-Water Weight
swclong<-swclong%>%
  mutate(POSWC = ((ifelse(is.na(POWW),PWW,POWW)))/swclong$mPot..kg.)

#how can I get WU in the same graph too ? maybe use 2 different line/point forms? 

#### Pre-Water SMC Graphs---------------------------------------------------  

accessions<-unique(swclong$Genotype) # unique() used to eliminate or delete the duplicate values/ rows present

for (i in accessions) {
  
  error.df <- swclong %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(PSWC, na.rm = TRUE),
      se = (sd(PSWC)/sqrt(length(PSWC))), #   #how to calculate standard error? (sd(PSWC)/sqrt(length(PSWC)))
      len = mean(PSWC), #what does this part stand for?
      Treatment = Treatment,
      Date = Date)
  
  
  PSWCplot<-swclong%>% 
    filter(Genotype == i)%>%
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
                linetype = "dotted",
                data = distinct(error.df))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+  #is there a way to name it day x rather than the actual date?
    xlab(label = "Date")+
    ggtitle(i) # how to add "Time Series of Soil Water Content by Treatment" and genotype? 
  print(PSWCplot)

}
