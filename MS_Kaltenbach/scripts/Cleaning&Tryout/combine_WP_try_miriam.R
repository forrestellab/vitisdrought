## Water Potentials Cambined in 1 Plot#----------
#make sure to check if entire data set or just subset (different .csv files and saving paths!

library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# ---- read file for entire data set
waterpotentials_gathered <-read.csv("data/Clean_WP.csv", header = TRUE, sep = ";") #had to read it in like this to make it look like a table and not just like a data.frame 
#---- read file for subset 
#waterpotentials_gathered <-read.csv("data/Subset/sub_wp.csv")

# CODE STARTS HERE: 
waterpotentials_gathered<- (waterpotentials_gathered%>%
                              mutate( species_geno = Genotype))%>%# unfortunately no "species" column 
  # to merge together, should've been picked too!
  mutate(Date = format(as.Date(as.character(Date), "%m.%d"),"%m/%d"))

##  Mutations

#----- Mutation Leaf Water Potential----------

      LWP_data<-waterpotentials_gathered%>% # since it took a few days to completely sample all plants, here, they are consolidated to a data point
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
  SWP_data<-waterpotentials_gathered%>%
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
      
      PD_data<-waterpotentials_gathered%>% #NA introduced by coercion ok
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
        
# Modifying Data Sets
      LWP_short = subset(LWP_data, select = -c(PD, SWP, SWP.area, PD.area) )
      SWP_short = subset(SWP_data, select = -c(PD, LWP, LWP.area, PD.area) )
      PD_short = subset(PD_data, select = -c(SWP, LWP, LWP.area, SWP.area) )



      #Combining Data Sets  
      
      joined_LWP_SWP <- left_join(LWP_short, SWP_short, 
                               by = c("ID" = "ID", "Genotype" = "Genotype", "Treatment" = "Treatment",
                                        "species_geno" = "species_geno", "Date" = "Date"))
      
      joined_WP <- left_join(joined_LWP_SWP, PD_short, 
                                 by = c("ID" = "ID", "Genotype" = "Genotype", "Treatment" = "Treatment",
                                        "species_geno" = "species_geno", "Date" = "Date"))
      joined_WP <-arrange(joined_WP, ID)
     
#lengthen data
      joined_WP_long<- pivot_longer(joined_WP, c(SWP, LWP, PD), names_to = "WPType", values_to = "WP") 
    
      joined_WP_long <- na.omit(joined_WP_long)

# -------  Plotting WP
genosWP<-unique(joined_WP_long$species_geno)
order1<-c("11/9-11","11/16-17","12/01-02","12/11-18")
order2<-c("12/11-18")





for (i in genosWP) {

# Plotting WP
LWP_plot<-joined_WP_long%>%
          filter(species_geno == i) %>%
          filter(WPType == "LWP" ) %>% 
          ggplot(aes(x = Date, Y = WP, fill = Treatment))+ 
          geom_boxplot(aes(y =-WP))+
          theme_classic()+
          #scale_x_discrete(limits = order1)+
          #scale_y_continuous(name = "WP (in bar)", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
          xlab(NULL)+
          theme(axis.text.x = element_text(angle = 60, hjust = 1))+
          theme(legend.position="none")+
          ggtitle(i)
#print(LWP_plot)



SWP_plot<-joined_WP_long%>%
  
          filter(species_geno == i)%>%
          filter(WPType == "SWP" ) %>% 
          ggplot(aes(x = Date, Y = WP, fill = Treatment))+
          geom_boxplot(aes(y = -WP))+
          #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
          #scale_x_discrete(limits = order2)+
          theme_classic()+
          xlab(NULL)+
          theme(legend.position="none")+
          ggtitle(i)

#print(SWP_plot)


PD_plot<-joined_WP_long%>%
          filter(species_geno == i)%>%
          filter(WPType == "PD" ) %>% 
          ggplot(aes(x = Date, Y = WP, fill = Treatment))+
          geom_boxplot(aes(y =- WP))+
          theme_classic()+
          scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 5))+
          xlab(NULL)+
          #scale_x_discrete(limits = order2)+
          theme(legend.position="none")+
          ggtitle(i)
print(PD_plot)

###--Merging the PSWC, POSWC and WU plots 

plot1 <-ggarrange( c(LWP_plot, SWP_plot, PD_plot), ncol=3, nrow=1, widths =  c(3, 3, 3), common.legend = TRUE, legend="bottom",align = "v", labels = "LWP", "SWP", "PD") #merge the 3 plots from above together


combined <- annotate_figure(plot1, top = text_grob((paste(" WP  of", i)), color = "black", face = "italic", size = 11)) #add title as genotype 

print(combined)

}

write.csv(joined_WP_long,"data_output/joined_WP_long.csv")
