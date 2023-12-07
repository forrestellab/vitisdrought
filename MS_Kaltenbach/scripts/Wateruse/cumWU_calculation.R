## Cumulative Water Use for Individuals 

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

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data -------------------------------------------------------------



# read file for subset 
swclong<- read.csv("data/Subset/sub_swc.csv")
swclong<- (swclong%>%
               filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                      "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))
# Cumulative Wateruse -----------------------------------------------------

cumWU <- swclong %>% 
        group_by(ID, Species, Genotype, Treatment, species_geno) %>% 
        mutate(csum = cumsum(WU))  %>%
        summarise(sumwu = sum(WU))  

write.csv(cumWU, file= "data/Subset/cumWU.csv")
