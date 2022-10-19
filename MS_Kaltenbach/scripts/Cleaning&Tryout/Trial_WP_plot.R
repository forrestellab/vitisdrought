

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

All_WP <-  read.csv("data_output/joined_WP_long.csv")


WP_gathered <-read.csv("data/Subset/sub_wp.csv")
WP_gathered <- WP_gathered[,c(2,12,4:8)]


# Cleaning data -----------------------------------------------------------

# All_WP <- All_WP[,c("ID","species_geno","Treatment","Date","WPType","WP")]
# 
# All_WP <- All_WP %>% group_by(Date) %>% filter(!any(Date == "11/01"))
# 
# 
# WP_spread <- All_WP
# 
# WP_spread$id <- paste0(WP_spread$ID,"_",WP_spread$species_geno,"_",WP_spread$Treatment,"_",WP_spread$Date)
# 
# WP_spread <- WP_spread[,c("id","WPType","WP")]
# 
# WP_spread <- spread(All_WP,key=WPType,value=WP)


# Convert dates -----------------------------------------------------------
WP_gathered<- WP_gathered %>% mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))
#----- Mutation Leaf Water Potential----------

LWP_data<-WP_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
  filter(!is.na(LWP))%>%
  filter(!is.na(species_geno))%>%
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
  mutate(LWP = as.numeric(gsub(",", ".", LWP))) # changed to substitute the "," with a "." so code is running, otherwise invalid data

#View(LWP_plot)  

##------Stem Water Potential Mutation ----##
SWP_data<-WP_gathered%>%
  filter(!is.na(SWP))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(SWP = as.numeric(gsub(",", ".", SWP)))

##-----Predawn Water Potential## 

#NOTES: 11/18 was ALS

PD_data<-WP_gathered%>% #NA introduced by coercion ok
  filter(!is.na(PD))%>%
  filter(!is.na(species_geno))%>%
  filter(!species_geno == "V37-96")%>%
  mutate(Date= recode(Date, "11/11" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/12" = "11/11-13"))%>%
  mutate(Date= recode(Date, "11/13" = "11/11-13"))%>%
  mutate(Date= recode(Date, "12/01" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/02" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/03" = "12/01-03"))%>%
  mutate(Date= recode(Date, "12/11" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/15" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/16" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/17" = "12/11-18"))%>%
  mutate(Date= recode(Date, "12/18" = "12/11-18"))%>%
  mutate(PD = as.numeric(gsub(",", ".", PD))) #%>%
#subset(select = -c(SWP, LWP, LWP.area, SWP.area) )



# df by variable ----------------------------------------------------------

WP_gathered <- WP_gathered %>% group_by(Date) %>% filter(!any(Date == "11/01"))



LWP <- WP_gathered[,c(1:5)]
SWP <- WP_gathered[,c(1:4,6)]
PD <- WP_gathered[,c(1:4,7)]



# Create id ---------------------------------------------------------------

WP_gathered$id <- paste0(WP_gathered$ID,"_",WP_gathered$species_geno,"_",WP_gathered$Treatment,"_",WP_gathered$Date)
LWP_data$id <- paste0(LWP_data$ID,"_",LWP_data$species_geno,"_",LWP_data$Treatment,"_",LWP_data$Date)
LWP_data <- LWP_data[,c("id","LWP")]
PD_data$id <- paste0(PD_data$ID,"_",PD_data$species_geno,"_",PD_data$Treatment,"_",PD_data$Date)
PD_data <- PD_data[,c("id","PD")]
SWP_data$id <- paste0(SWP_data$ID,"_",SWP_data$species_geno,"_",SWP_data$Treatment,"_",SWP_data$Date)
SWP_data <- SWP_data[,c("id","SWP")]


joined <- WP_gathered[,"id"]
joined <- left_join(joined,LWP_data,by="id")

# Boxplot -----------------------------------------------------------------

data <- data.frame(
  name=c( rep("A",500), rep("B",500), rep("B",500), rep("C",20), rep('D', 100)  ),
  value=c( rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(20, 25, 4), rnorm(100, 12, 1) )
)

All_WP %>%
  ggplot( aes(x=WPType, y=WP, fill=Treatment)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")






