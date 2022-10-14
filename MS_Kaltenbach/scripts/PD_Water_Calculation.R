library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


##Predawn Water Potential## 

#NOTES: 11/18 was ALS


waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame

PD_plot<-waterpotentials_gathered%>% #NA introduced by coercion ok
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
  mutate(PD = as.numeric(PD)) # this is where error occurs!!!
#View(PD_plot) 
genos<-unique(PD_plot$Genotype)

for (i in genos) {
  graph<-PD_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = PD, fill = Treatment))+
    geom_boxplot(aes(y =-PD))+
    ylim(-20,-1)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".pdf"))
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".png"))
}
# shows same warning as in LWP e.g. 28: Removed 28 rows containing non-finite values (stat_boxplot). --> error is related to   mutate(PD = as.numeric(PD))