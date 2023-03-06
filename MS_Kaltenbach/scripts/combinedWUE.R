# Water Use Efficiency 

# General Notes -----------------------------------------------------------

# Water Use Efficiency (WUE) = net assimilation-photosynthetic rate (An) / stomatal conductance (gs)

setwd("~/Documents/GitHub/vitisdrought")

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
gswclean_6800<-read.csv("input/gsw_6800_pre-clean.csv")
gswclean_6800<- gswclean_6800%>%
  mutate(species_geno = Genotype)

# Subset File


gswclean_6800 <-(gswclean_6800%>%
                   filter(Genotype %in% c("9018", "T52", "b40-14", "9031", "9035", "b42-34", "SC2", 
                                          "TX6704", "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

# Clean Data --------------------------------------------------------------

gswclean_6800<-gswclean_6800%>%
  filter(!is.na(A))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d")) %>%
  mutate(A = A/1000000)

#adjust units: in the original file, A = µmol m⁻² s⁻¹ and E = mol m⁻² s⁻¹, gsw is mol m⁻² s⁻¹. Therefore, I decided to adjust the units of A


# ifelse for Date ---------------------------------------------------------

gswclean_6800$Date <- ifelse(gswclean_6800$Date=="10/26"|gswclean_6800$Date=="10/27" |
                               gswclean_6800$Date=="10/28" |gswclean_6800$Date=="10/29","10/26-29",
                             ifelse(gswclean_6800$Date=="11/12"|gswclean_6800$Date=="11/13","11/12-13",
                                    ifelse(gswclean_6800$Date=="12/06"|gswclean_6800$Date=="12/07"
                                           |gswclean_6800$Date=="12/08" |gswclean_6800$Date=="12/09","12/06-09",
                                           gswclean_6800$Date)))


# intrinsic WUE Calculation ---------------------------------------------------------

gswclean_6800 <- gswclean_6800 %>%
  mutate(intrWUE = A / gsw_6800)

# instantaneous WUE Calculation ---------------------------------------------------------

gswclean_6800 <- gswclean_6800 %>%
                  mutate(instWUE= A / E)

gswclean_6800 <- gswclean_6800 %>%
                  rename("instWUE (A/E)" = "instWUE", "intrWUE (A/Gs)" = "intrWUE")

#combine WUE 

joined_WUE <-  gather(gswclean_6800, key = "TypeWUE", value = "value", "instWUE (A/E)", "intrWUE (A/Gs)")



#write.csv(joined_WUE , file= "MS_Kaltenbach/data/Subset/new/joined_WUE.csv")

# Plot Graph --------------------------------------------------------------

genos<-unique(joined_WUE$species_geno)

for (i in genos) {
  graph<-joined_WUE%>%
    filter(species_geno == i)%>%
    ggplot(aes(x = Date, Y = value, fill = Treatment))+
    geom_boxplot(aes(y = value))+
    theme_classic()+
    facet_grid(facets = TypeWUE ~ ., scales = "free")+
    ylab("")+
    ggtitle(i)
  print(graph)
  
  # Save Plot ---------------------------------------------------------------
  
  
  #path to save subset files: 
       #ggsave(paste0("fig_output/Subset/combinedWUE/combinedWUE",i, ".png"))
       #ggsave(paste0("fig_output/Subset/combinedWUE/combinedWUE",i, ".pdf"))
        
}