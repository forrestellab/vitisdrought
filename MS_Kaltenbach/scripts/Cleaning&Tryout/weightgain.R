# Weight Gain

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
#swclong<- read.csv("data/Subset/sub_swc.csv")


# SWC Calculations ---------------------------------------------------------
  #Pre-Water SWC: PWSWC = (PWW/mpot)*100 (Note: Bamboo-Shoots are not in the calculations)
  swclong$PSWC= swclong$PWW/swclong$mPot..kg.

  #to calculate Post-Water SWC: POSWC = (POWW/mpot)*100
  #if no watering happened: pre-water weight used instead of Post-Water Weight
  swclong<-swclong%>%
  mutate(POSWC = ((ifelse(is.na(POWW),PWW,POWW)))/swclong$mPot..kg.) %>%
  subset(select = -c(midday.WT,POWW.NOTES, PD.WT) )
  
  
  

# Weight Gain Calculations --------------------------------------------------------

  # dry weight - 
  weigth  <-swclong%>%
                mutate(weightdry = (swclong$PWW - swclong$Dry.weight.of.soil..kg.)) %>%
                group_by(ID)%>%
                mutate(weightdiff= weightdry-lag(weightdry,default=first(weightdry)))  
          


# Plot Graph --------------------------------------------------------------

accessions<-unique(weight$species_geno)
  
for (i in accessions) {
  
      error.pswc <- weight %>%
      filter(species_geno == i)%>%
      group_by(Date, Treatment) %>%
      summarise(
        sd = sd(weightdiff, na.rm = TRUE),
        se = (sd(weightdiff)/sqrt(length(weightdiff))), 
        len = mean(), 
        Treatment = Treatment,
        Date = Date)
    
    weightplot<-weight%>% 
      filter(species_geno == i)%>%
      ggplot(aes(y =weightdiff, x = as.Date(Date,"%m/%d"), color = Treatment))+
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
      scale_y_continuous(name = "weight gain" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
      theme(legend.position="none")+
      ggtitle(i)+
      xlab(label = "")
    
    print(PSWCplot)
  


# Save Graph --------------------------------------------------------------

  #path to save entire files:
  #ggsave(paste0("fig_output/Weight/gainedweight/gainedweight",i, ".png"))
  #ggsave(paste0("fig_output/Weight/gainedweight/gainedweight",i, ".pdf"))
  
  #path to save subset files: 
  #ggsave(paste0("fig_output/Subset/Weight/gainedweight/gainedweight",i, ".png"))
  #ggsave(paste0("fig_output/Subset/Weight/gainedweight/gainedweight",i, ".pdf"))
  
}

# how can I have one combined x-axis? + rremove("xlab")
# how can I decrease the distance between the plots? 
