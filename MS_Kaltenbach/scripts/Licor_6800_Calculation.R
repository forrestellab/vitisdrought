
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

###Licor 6800 Point Measurement Graphs-------------------------------------------------
# see second version below!

gswclean_6800<-read.csv("data/gsw_6800_cleaned.csv")

#Looking at A variation between control and drought


A_plot<-gswclean_6800%>%
  filter(!is.na(A))%>%
  filter(!is.na(Genotype))%>%
  filter(!Genotype == "V37-96")%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))%>%
  mutate(Date= recode(Date, "10/26" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/27" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/28" = "10/26-29"))%>%
  mutate(Date= recode(Date, "10/29" = "10/26-29"))%>%
  mutate(Date= recode(Date, "11/12" = "11/12-13"))%>%
  mutate(Date= recode(Date, "11/13" = "11/12-13"))%>%
  mutate(Date= recode(Date, "12/06" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/07" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/08" = "12/06-09"))%>%
  mutate(Date= recode(Date, "12/09" = "12/06-09"))



#View(A_plot%>%filter(Genotype == "Vru42"))
genos<-unique(A_plot$Genotype)


for (i in genos) {
  graph<-A_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = A, fill = Treatment))+
    geom_boxplot(aes(y =A))+
    #ylim(0,1)+
    theme_classic()+
    ggtitle(i)
  print(graph)
  
  ggsave(paste0("fig_output/Licor6800/controlvsdrought/controlvsdrought",i, ".png"))
  ggsave(paste0("fig_output/Licor6800/controlvsdrought/controlvsdrought",i,".pdf"))
}

# 2nd version found in Master R script 
# I don't understand why this one is a histogram and what should it tell me?

#Read in licor 6800 point measurements with obvious outliers removed 
pointsclean<-read.csv("data/gsw_6800_cleaned.csv")
#View(pointsclean)

#look at histograms of gsw for each treatment of each genotype on each day
pointsclean<-add_column(pointsclean, Geno_Date_Treat = paste(pointsclean$Genotype,pointsclean$Treatment,pointsclean$Date,sep = "_"), .after = "Date")
pointsclean$Geno_Date_Treat
codedates<-unique(pointsclean$Geno_Date_Treat)

for (i in codedates) {
  a<-pointsclean%>%filter(Geno_Date_Treat == i)%>%
    ggplot()+
    geom_histogram(aes(x = gsw_6800,fill = Treatment), bins = 3)+
    ggtitle(i)
  print(a)
  
  ggsave(paste0("fig_output/Licor6800/histogram/histogram",i, ".png"))
  ggsave(paste0("fig_output/Licor6800/histogram/histogram",i,".pdf"))
}
# if necessary, maybe I can find a way to combine all the plots on one page? but for what do I need those? and is it always 9 measurement points? 
