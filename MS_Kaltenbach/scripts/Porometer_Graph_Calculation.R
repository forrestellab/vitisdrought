#Porometer Graphs 


# General Notes -----------------------------------------------------------

# Notes from Miriam:    

    #1. I excluded the "ylim(0,1)" since it cut of some values while plotting the boxplot.
    #3. why did Nico exclude some of the dates in his plots and mutated only some?
    #4. why did Nico mutate 11/30 but i can't find any measurements for this day in the data?

#Notes from Nico : we have more than two reads per plant right now. they are all equivalently weighted. What does this do to the stats?
 #also I have to figure out on what bounds to bin sample days

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

# Entire File
gswclean<-read.csv("data/gsw_porometer_cleaned.csv")
gswclean<- gswclean%>%
            mutate(species_geno = Genotype)

# Subset File
gswclean <-read.csv("data/Subset/sub_gswc.csv")

# Clean Data --------------------------------------------------------------

gswclean<-gswclean%>%
  filter(!is.na(gsw_porometer))%>%
  filter(!(gsw_porometer>1))%>%
  filter(!is.na(species_geno))%>%
  mutate(Date = format(as.Date(as.character(Date),"%m.%d"),"%m/%d"))

# Average Data on Plant Level per day (combine reps) --------------------------------------------------------------

gswclean<-gswclean%>%
  group_by(Code_Date, ID, Date, Treatment, species_geno, Genotype) %>%
  summarise_at(vars(gsw_porometer), mean)



# ifelse for Date ---------------------------------------------------------

gswclean$Date <- ifelse(gswclean$Date=="10/26"|gswclean$Date=="10/27","10/26-27",
                             ifelse(gswclean$Date=="11/16"|gswclean$Date=="11/17","11/16-17",
                                    ifelse(gswclean$Date=="11/02"|gswclean$Date=="11/03", "11/02-03",
                                    ifelse(gswclean$Date=="11/23"|gswclean$Date=="11/24", "11/23-24",
                                    ifelse(gswclean$Date=="11/30"|gswclean$Date=="12/2","11/30-12/2",
                                           gswclean$Date)))))



#write.csv(gswclean, file= "data/Subset/new/Porometer.csv")

# Plot Graph --------------------------------------------------------------

genos<-unique(gswclean$species_geno)
order <- c("10/26-27", "11/02-03","11/06","11/11","11/16-17","11/23-24" ,"12/02")

for (i in genos) {
  GSW_plot<-gswclean%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = gsw_porometer, fill = Treatment))+
    geom_boxplot(aes(y =gsw_porometer))+
    ylim(0,1)+
    #scale_x_discrete(limits = order)+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    ylab("Gs (mmol m−2 s−1)")+
    ggtitle(i)
  print(GSW_plot)
  


# Save Plot ---------------------------------------------------------------

  #path to save all files: 
  #ggsave(paste0("fig_output/Porometer/Porometer",i, ".png"))
  #ggsave(paste0("fig_output/Porometer/Porometer",i,".pdf"))
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset_small/Porometer/Porometer",i, ".png"))
  ggsave(paste0("fig_output/Subset_small/Porometer/Porometer",i, ".pdf"))
}

