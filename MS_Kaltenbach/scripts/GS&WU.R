# Combining Porometer Data and WaterUsage

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
library(grid)

# Import data -------------------------------------------------------------
# read file for subset Water Usage Data
swclong<- read.csv("data/Subset/sub_swc.csv")

swclong1<-swclong%>%
  select((c(ID, Genotype, Treatment, Date, WU)))


# read file for Stomatal conductance
gswclean <-read.csv("data/Subset/sub_gswc.csv")

# Clean Data --------------------------------------------------------------

gswclean<-gswclean%>%
  filter(!is.na(gsw_porometer))%>%
  filter(!(gsw_porometer>1))%>%
  filter(!is.na(species_geno))%>%
  mutate(Date = format(as.Date(as.character(Date),"%m.%d"),"%m/%d"))

# Average Data on Plant Level per day (combine reps) --------------------------------------------------------------

gswclean<-gswclean%>%
  group_by(Code_Date, ID, Date, Treatment, Genotype) %>%
  summarise_at(vars(gsw_porometer), mean)


# ifelse for Date ---------------------------------------------------------

 # used last day of sampling as the date in graph 

gswclean$Date <- ifelse(gswclean$Date=="10/26"|gswclean$Date=="10/27","10/27",
                        ifelse(gswclean$Date=="11/16"|gswclean$Date=="11/17","11/17",
                               ifelse(gswclean$Date=="11/02"|gswclean$Date=="11/03", "11/03",
                                      ifelse(gswclean$Date=="11/23"|gswclean$Date=="11/24", "11/24",
                                             ifelse(gswclean$Date=="11/30"|gswclean$Date=="12/2","12/2",
                                                    gswclean$Date)))))
# Combine WU and Porometer data ---------------------------------------------------------

joined_df<- merge(gswclean, swclong1, by= c("Genotype", "Treatment", "ID", "Date"), all = TRUE) %>%
            select(-c(Code_Date))%>%
            rename("WU (L)" = "WU", "Gs (mmol m−2 s−1)" = "gsw_porometer")


joined_df <-  gather(joined_df, key = "Type", value = "value", "WU (L)", "Gs (mmol m−2 s−1)")
joined_df <- drop_na(joined_df)

# Plot Graph ---------------------------------------------------------

genosWP<-unique(joined_df$Genotype)

for (i in genosWP) {
  
  error.combined <- joined_df%>%
    filter(Genotype == i)%>%
    group_by(Date , Treatment, Type) %>%
    summarise(
      sd = sd(value, na.rm = TRUE),
      se = (sd(value)/sqrt(length(value))),
      len = mean(value),
      Treatment = Treatment,
      Date = Date)
  
  
  plot<-joined_df%>%
    filter(Genotype == i)%>%
    ggplot(aes(x = as.Date(Date ,"%m/%d"), y = value, color = Treatment))+
    geom_point()+
    stat_summary(fun="mean",geom="line",size = 1)+
    geom_errorbar(aes(x = as.Date(Date ,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.9),
                  size = .3,
                  data = distinct(error.combined))+
    theme_classic()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="5 days")+
    xlab(label = "Date")+
    ylab("")+
    theme(legend.position="bottom")+
    facet_grid(facets = Type ~ ., scales = "free")+
    ggtitle(i)
  
  print(plot)
  
  #path to save subset files: 
  ggsave(paste0("fig_output/Subset/GS_WU_Combined/GS_WU_Combined",i, ".png"))
  ggsave(paste0("fig_output/Subset/GS_WU_Combined/GS_WU_Combined",i, ".pdf"))
  
}


