
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

accessions<-unique(swclong$Genotype) # unique() used to eliminate or delete the duplicate values/ rows present

for (i in accessions) {
  
  error.df <- swclong %>%
    filter(Genotype == i)%>% #filter by the genotype
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(PSWC, na.rm = TRUE), # calculate standard deviation
      se = (sd(PSWC)/sqrt(length(PSWC))), # calculate standard error
      len = mean(PSWC), #what does this part stand for?
      Treatment = Treatment,
      Date = Date)
  
  
  PSWCplot<-swclong%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ #use point instead of "line", how can I plot the mean and error
    # how to plot regression curve? and mean sqrt?
    stat_summary(fun="mean",geom="line",size = 1)+ # or just points: stat_summary(fun="mean",geom="point",size = 2, shape = 3)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, #include standard error as error bars
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
    ggtitle(paste("SWC Pre-Watering of", i)) #use the paste function to merge the name and the objcet "i"
  print(PSWCplot)
  #ggsave(paste0("fig_output/SWC/PSWC/PSWC",i, ".png")) #safe plots in output folder and under a certain name as a .png
  #ggsave(paste0("fig_output/SWC/PSWC/PSWC",i, ".pdf")) #safe plots in output folder and under a certain name as a .pdf (I have noticed, that I have to save it as an .png first in order to create a .pdf)
}

#### Post-Water SMC Graphs---------------------------------------------------  

accessions<-unique(swclong$Genotype) 

for (i in accessions) {
  
  error.df <- swclong %>%
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
                  data = distinct(error.df))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    ggtitle(paste("SWC Post-Watering of", i))
  print(POSWCplot)
  #ggsave(paste0("fig_output/SWC/POSWC/POSWC",i, ".png")) #safe plots in output folder and under a certain name as a .png
  #ggsave(paste0("fig_output/SWC/POSWC/POSWC",i, ".pdf")) #safe plots in output folder and under a certain name as a .pdf
}

#### Combining Pre- and Post-Watering Soil-Water-Content--------

#lengthen data
swclong1<- pivot_longer(swclong, c(PSWC, POSWC), names_to = "WCType", values_to = "WC") #combine columns PSWC and POSWC under the column WCTape (Water Content Type) and the Values in WC = Water Content

swc_combined<- mutate(swclong1, options = paste0(WCType, Treatment) ) #combine the colums of WCType and the Treatment into one columns 

#### Plotting the Soil Water Content Pre-and Post-Watering

accessions<-unique(swc_combined$Genotype)

for (i in accessions) {
  
  error.df <- swc_combined %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WC, na.rm = TRUE),
      se = (sd(WC)/sqrt(length(WC))), 
      len = mean(WC),
      Treatment = options, #name how the combination of POSWC and PSWC for Control and Drought should be called (Treatments)
      Date = Date)
  
  
  WCplot<-swc_combined%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =WC, x = as.Date(Date,"%m/%d"), color = options))+
    geom_point(aes(shape = options), size = 2.5)+ #add the shape = function to ensure seeing different shapes in the plot for Pre- and Post
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, 
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.5),
                  size = .3,
                  linetype = "dotted",
                  data = distinct(error.df))+
    theme_classic()+
    scale_color_manual(values=c("red", "blue", "red", "blue"))+ #use the scale_color_manual function to set the colors of the different treatments blue = drought, red = control. By default, they are organized in alphabetical order for now. 
    scale_shape_manual(values=c(1,1,4,4))+ #use the scale_shape_manual function to set the shapes according to Pre- and Post watering. same here: it is organized by alphabet. The 1 stands for the circle and Post-Water, 4 is a cross, for Pre-Water. 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+ 
    xlab(label = "Date")+
    ggtitle(paste("Pre- & Post-Watering SWC of ", i)) 
  print(WCplot)
  #ggsave(paste0("fig_output/SWC/WC/WC",i, ".pdf"))
  #ggsave(paste0("fig_output/SWC/WC/WC",i, ".png"))
  
} #make sure to include this bracket for running the code: it is to show R where the loop
 
###Water Use Graphs------------------------------------------------

accessions<-unique(swclong$Genotype)

for (i in accessions) {
  
  error.df <- swclong %>%
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
                  data = distinct(error.df))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    ggtitle(paste("Water Use by Treatment of ", i))
  print(wuplot)
  ggsave(paste0("fig_output/WU/WU",i, ".png"))
  ggsave(paste0("fig_output/WU/WU",i, ".pdf"))
}


####------ Combining Water Usage and Pre- and Post-Watering SWC and Water Usage

#lengthen data
swc_wu<- pivot_longer(swclong, c(PSWC, POSWC, WU), names_to = "WC_WUType", values_to = "WC_WU") #combine columns PSWC, POSWC and WU under the column WCType (Water Content Type, Water Usage) and the Values in WC = Water Content/ Water Usage

swc_wu_combined<- mutate(swc_wu, options = paste0(WC_WUType, Treatment) ) #combine the colums of WC_WUType and the Treatment into one columns 


#### Plotting the Soil Water Content Pre-and Post-Watering and Water_Usage

accessions<-unique(swc_wu_combined$Genotype)

for (i in accessions) {
  
  error.df <- swc_wu_combined %>%
    filter(Genotype == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WC_WU, na.rm = TRUE),
      se = (sd(WC_WU)/sqrt(length(WC_WU))), 
      len = mean(WC_WU),
      Treatment = options, #name how the combination of POSWC and PSWC for Control and Drought should be called (Treatments)
      Date = Date)
  
  
  SWC_WUplot<-swc_wu_combined%>% 
    filter(Genotype == i)%>%
    ggplot(aes(y =WC_WU, x = as.Date(Date,"%m/%d"), color = options))+
    geom_point(aes(shape = options), size = 2.5)+ #add the shape = function to ensure seeing different shapes in the plot for Pre- and Post
    stat_summary(fun="mean",geom="line",size = 1)+
    #geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, 
     #                 ymin = len-se,
      #                ymax = len+se),
       #           color = "black",
        #          position=position_dodge(width=0.5),
         #         size = .3,
          #        linetype = "dotted",
           #       data = distinct(error.df))+
    theme_classic()+
    scale_color_manual(values=c("red", "blue", "green", "red", "green", "black"))+ #use the scale_color_manual function to set the colors of the different treatments blue = drought, red = control. By default, they are organized in alphabetical order for now. 
    scale_shape_manual(values=c(1,1,4,4,5,5))+ #use the scale_shape_manual function to set the shapes according to Pre- and Post watering. same here: it is organized by alphabet. The 1 stands for the circle and Post-Water, 4 is a cross, for Pre-Water. 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+ 
    xlab(label = "Date")+
    ggtitle(paste("Pre-&Post-WA SWC & WU by Treatment of ", i)) 
  print(SWC_WUplot)
  #ggsave(paste0("fig_output/WU/WU_SWC/WU_SWC",i, ".png"))
  #ggsave(paste0("fig_output/WU/WU_SWC/WU_SWC",i, ".pdf"))
} 

# Questions I have still: 

# how can I change the date to day x? 
# how can I get WU in the same graph too ? maybe use 2 different line/point forms? --> probably use the pivot_longer-function 
# or is there a way to have two different y-axis for WU and SWC? 
# how can I plot: PSWC, PSWC and WU in 3 different Plots but one figure? Either next to each other or above each other? 
