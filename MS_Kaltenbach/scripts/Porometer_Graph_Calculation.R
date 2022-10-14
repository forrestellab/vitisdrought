
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

###Porometer Graphs----------------------------------------------------------------

#Notes : we have more than two reads per plant right now. they are all equivalently weighted. What does this do to the stats?
#also I have to figure out on what bounds to bin sample days.


setwd("~/Documents/Master_Thesis/Master_Thesis")

gswclean<-read.csv("data/gsw_porometer_cleaned.csv")

GSW_plot<-gswclean%>%
  filter(!is.na(gsw_porometer))%>%
  filter(!is.na(Genotype))%>%
  mutate(Date = format(as.Date(as.character(Date),"%m.%d"),"%m/%d"))

#mutate(Date= recode(Date, "10/26" = "10/26-27"))%>%
#mutate(Date= recode(Date, "10/27" = "10/26-27"))%>%
#mutate(Date= recode(Date, "11/16" = "11/16-17"))%>%
#mutate(Date= recode(Date, "11/17" = "11/16-17"))%>%
#mutate(Date= recode(Date, "11/23" = "11/23-24"))%>%
#mutate(Date= recode(Date, "11/24" = "11/23-24"))%>%
#mutate(Date= recode(Date, "11/30" = "11/30-12/2"))%>%
#mutate(Date= recode(Date, "12/02" = "11/30-12/2"))



#View(GSW_plot%>%filter(Genotype == "Vru42"))
genos<-unique(GSW_plot$Genotype)


for (i in genos) {
  graph<-GSW_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = gsw_porometer, fill = Treatment))+
    geom_boxplot(aes(y =gsw_porometer))+
    ylim(0,1)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    ggtitle(i)
  print(graph)
  
  ggsave(paste0("fig_output/Porometer/Porometer",i, ".png"))
  ggsave(paste0("fig_output/Porometer/Porometer",i,".pdf"))
}

#showed following warning: Removed 1 rows containing non-finite values (stat_boxplot). (15 in total)
