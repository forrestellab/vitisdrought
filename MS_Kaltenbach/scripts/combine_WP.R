#Waterpotentials Combined


# General Notes -----------------------------------------------------------

  #1. some of the dates where excluded in Nicos Code. Why would he do that? Or do we not wantthose days? 
  #2. some of the genotypes don't have all the data points/ PD was not measured --> how are we handling those?

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

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data -------------------------------------------------------------

# Entire File
WP_gathered <- read.csv("data/Clean_WP_TAB.csv")
WP_gathered <- WP_gathered %>% mutate( species_geno = Genotype)
WP_gathered <- WP_gathered[,c(1,11,3:7)]

# Subset
WP_gathered <-read.csv("data/Subset/sub_wp.csv")
WP_gathered <- WP_gathered[,c(2,12,4:8)]


# Clean Data --------------------------------------------------------------

WP_gathered$Date <- as.factor(WP_gathered$Date)
WP_gathered$Month <- substr(WP_gathered$Date,1,2)
WP_gathered$Day <- substr(WP_gathered$Date,4,5)

WP_gathered$Date <- paste0(WP_gathered$Month,"/",WP_gathered$Day)

WP_gathered <- WP_gathered[,c(1:7)]

WP_gathered$LWP <- as.numeric(gsub(",", ".", WP_gathered$LWP))
WP_gathered$SWP <- as.numeric(gsub(",", ".", WP_gathered$SWP))
WP_gathered$PD <- as.numeric(gsub(",", ".", WP_gathered$PD))


WP_long <- WP_gathered %>% gather(key = "WPType", value = "WP", LWP,SWP,PD)

WP_long <- na.omit(WP_long)

# ifelse for Date------------------------------------------------------------------


WP_long$Date_group <- ifelse(WP_long$Date=="11/9"|WP_long$Date=="11/11","Nov 9 to 11",
                             ifelse(WP_long$Date=="11/16"|WP_long$Date=="11/17","Nov 16 to 17",
                             ifelse(WP_long$Date=="12/1"|WP_long$Date=="12/2","Dec 01 to 02",
                             ifelse(WP_long$Date=="12/11"|WP_long$Date=="12/15"|WP_long$Date=="12/16"|WP_long$Date=="12/17"|
                                    WP_long$Date=="12/18","Dec 11 to 18",WP_long$Date))))

# TO ELIMINATE Unwanted Days
# WP_long <- WP_long %>% group_by(Date_group) %>% filter(!any(world == c("all","the","nonwanted),"dates))


# Plot Graph --------------------------------------------------------------

order <- c("11/1","Nov 9 to 11","11/12","11/13","Nov 16 to 17","11/18" ,"Dec 01 to 02","12/3" ,"Dec 11 to 18")
genosWP<-unique(WP_long$species_geno)

for (i in genosWP) {
  
  new_plot<-WP_long%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date_group, Y = WP, fill = Treatment))+
    geom_boxplot(aes(y =-WP))+
    #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
    theme_classic()++
    xlab("Date")+
    theme(legend.position="none") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    facet_grid(facets = WPType ~ .)+
    scale_x_discrete(limits = order)+
    ggtitle(i)
  print(new_plot)
  
  #path to save all files:
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".png"))
  #ggsave(paste0("fig_output/WP/PD/PD",i, ".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset/WP/WPcombined/WPcombined",i, ".png"))
  ggsave(paste0("fig_output/Subset/WP/WPcombined/WPcombined",i, ".pdf"))
}
