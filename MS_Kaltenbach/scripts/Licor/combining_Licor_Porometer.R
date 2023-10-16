#Combine Porometer and Licor Data

# General Notes -----------------------------------------------------------


setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


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

# Import data -------------------------------------------------------------

Licor <- read.csv("data/Subset/new/Licor.csv")
Porometer <- read.csv("data/Subset/new/Porometer.csv")


# Subset data -------------------------------------------------------------

Licor <- Licor %>%
          select(c(Genotype, gsw_6800,Treatment, ID, Date))%>%
          mutate(Measurement = "L") %>%
          mutate( Measurement_Date = paste(Date, Measurement, sep = "_"))


Porometer <- Porometer %>%
              select(-c(X, Code_Date, species_geno)) %>%
              mutate(Measurement = "P") %>%
              mutate(Measurement_Date = paste(Date, Measurement, sep = "_"))


combined_gsw <- merge(Porometer, Licor , by= c("ID", "Genotype", "Treatment", "Measurement", "Measurement_Date" ), all = TRUE) %>%
                select(-c(Date.x, Date.y))


colnames(combined_gsw)

combined_gsw_long <-  gather(combined_gsw, key = "MeasurementType", value = "value", "gsw_porometer", "gsw_6800") %>%
                     na.omit()



genos<-unique(combined_gsw_long$Genotype)

for (i in genos) {
  graph<-combined_gsw_long%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = Measurement_Date, Y = value, fill = Treatment))+
    geom_boxplot(aes(y =value))+
    theme_classic()+
    theme(legend.position="bottom")+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))+
    ylab("")+
    xlab("")+
    ggtitle("Gs of", i)
  
  print(graph)
  
}

