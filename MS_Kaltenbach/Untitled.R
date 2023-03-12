
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/Master_Thesis/Master_Thesis")

##Leaf Water Potential## MUTATION

waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame

WP_plot1<-waterpotentials_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
  filter(!is.na(LWP))%>%
  filter(!is.na(Genotype))%>%
  #mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>% #the problem here is, that the dates look so much different: how can I solve this? it has something to do with the "." 
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))%>% #i think this can be the solution!
  
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
  mutate(LWP = as.numeric(gsub(",", ".", LWP))  )#%>% # #---->>>#when running this code: a warning appears and the LWP columns are all empty in the LWP plot object
# but in the waterpotentials_gathered data.frame the values are still there but with commas instead of points. how can I change that? 
# I have to do sth with this column. otherwise I will have this error: Error in -LWP : invalid argument to unary operator #the solution could be to substitute the "," with a "." 


##Stem Water Potential## MUtation in Pipe
  WP_plot2<-  WP_plot1%>% 
  filter(!is.na(SWP))%>%
  filter(!is.na(Genotype))%>%
  filter(!Genotype == "V37-96")%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))%>%
  #mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(SWP = as.numeric(gsub(",", ".", SWP))) #%>%

##Predawn Water Potential## -----Mutation in pipe
 WP_plot3<-  WP_plot2%>% 
  filter(!is.na(PD))%>%
  filter(!is.na(Genotype))%>%
  filter(!Genotype == "V37-96")%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))%>%
  #mutate(Date = format(as.Date(Date, "%m.%d"),"%m/%d"))%>%
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


#print(WP_plot)

genos<-unique(WP_plot3$Genotype)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")


  

for (i in genos) {
  
  #Leaf Water Potential Graph##
  
  graphLWP<-WP_plot3%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = is.na(LWP), fill = Treatment))+ #does the is.na function help here?
    geom_boxplot(aes(y =-LWP))+
    ylim(-15,-4)+
    theme_classic()+
    scale_x_discrete(limits = order1)+
    ggtitle(i)
  #print(graphLWP)
  
  #Stem Water Potential Graph##
  
  graphSWP<-WP_plot3%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
    geom_boxplot(aes(y =-SWP))+
    ylim(-20,-5)+
    theme_classic()+
    ggtitle(i)
  #print(graphSWP)
  
  ##Predawn Water Potential Graph ##
  
  graphPD<-WP_plot3%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = PD, fill = Treatment))+
    geom_boxplot(aes(y =-PD))+
    ylim(-20,-1)+
    theme_classic()+
    ggtitle(i)
  #print(graphPD)

  ###--merging the plots 
  
  plot1 <-ggarrange(graphLWP, graphSWP, graphPD, ncol=3, nrow=1, common.legend = TRUE, legend="bottom") #merge the 3 plots from above together
  
  combined <- annotate_figure(plot1, top = text_grob((paste("WP of", i)), color = "black", face = "italic", size = 13)) #add title as genotype
  
  print(combined)
  
  ggsave(paste0("fig_output/WP/Combined/Combined",i, ".png"))
  ggsave(paste0("fig_output/WP/Combined/Combined",i, ".pdf"))
} 

#as of rn, I have not piped them all. I would need to have to figure out how to correctly do that 
