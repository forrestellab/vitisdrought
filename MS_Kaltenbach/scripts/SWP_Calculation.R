
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Leaf Water Potential##

waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame


##Stem Water Potential##
SWP_plot<-waterpotentials_gathered%>%
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
  mutate(SWP = as.numeric(gsub(",", ".", SWP)))

genos<-unique(SWP_plot$Genotype)

for (i in genos) {
  graph<-SWP_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = SWP, fill = Treatment))+
    geom_boxplot(aes(y =-SWP))+
    ylim(-20,-5)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
  ggsave(paste0("fig_output/WP/SWP/SWP",i, ".png"))
  ggsave(paste0("fig_output/WP/SWP/SWP",i, ".pdf"))
}

## shows same warning as in LWP e.g. 28: Removed 28 rows containing non-finite values (stat_boxplot). --> error is related to   mutate(PD = as.numeric(PD))
