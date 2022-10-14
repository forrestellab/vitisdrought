
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Leaf Water Potential##

waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame

LWP_plot<-waterpotentials_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
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
  mutate(LWP = as.numeric(gsub(",", ".", LWP))) # #---->>>#when running this code: a warning appears and the LWP columns are all empty in the LWP plot object
# but in the waterpotentials_gathered data.frame the values are still there but with commas instead of points. how can I change that? 
# I have to do sth with this column. otherwise I will have this error: Error in -LWP : invalid argument to unary operator #the solution could be to substitute the "," with a "." 



#View(LWP_plot)  


genos<-unique(LWP_plot$Genotype)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")


for (i in genos) {
  graph<-LWP_plot%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Date, Y = is.na(LWP), fill = Treatment))+ #does the is.na function help here?
    geom_boxplot(aes(y =-LWP))+
    ylim(-15,-4)+
    theme_classic()+
    scale_x_discrete(limits = order1)+
    ggtitle(i)
  print(graph)
  
  ggsave(paste0("fig_output/WP/LWP/LWP",i, ".png"))
  ggsave(paste0("fig_output/WP/LWP/LWP",i, ".pdf"))
} 

#there is still a warning message: saying that there were rows containing missing values removed