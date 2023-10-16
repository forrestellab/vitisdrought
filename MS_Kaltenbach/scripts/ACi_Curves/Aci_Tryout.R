library(tidyverse)
library(stringr)
library(stringi)
library(dplyr)
library(readxl)
library(data.table)
library(ggplot2)
library(writexl)
library(utils)
library(base)

setwd("~/Documents/Master_Thesis/ACi-Curves/all-excel-files/modified/CSV")

# modify & combine individual csv files -----------------------------------
filenames <- list.files(pattern="*.csv", full.names=TRUE)

  for (filename in filenames) {
    list<-read_csv(filename, skip = 13)[-1:-2,]
    
    list$A <- as.numeric(list$A)
    list$Ca <- as.numeric(list$Ca)
    list$Ci <- as.numeric(list$Ci)
    list$E <- as.numeric(list$E)
    list$CO2_s <- as.numeric(list$CO2_s)
    list$CO2_r <- as.numeric(list$CO2_r)
    list$Qin <- as.numeric(list$Qin)
    list$gsw <- as.numeric(list$ gsw)
    list$gtc <- as.numeric(list$gtc)
    list$Pa <- as.numeric(list$Pa)
    list$RHcham <- as.numeric(list$RHcham)
    list$Tleaf <- as.numeric(list$Tleaf)
    list$Adark <- as.numeric(list$Adark)

    subset_csv<-  list %>%
      select( "date","A","Ca", "Ci","E", "CO2_s", "CO2_r", "Qin", "gsw","gtc", "Pa","Qin","RHcham","Tleaf",
              "PhiPS2", "Fo", "Fm", "Fv/Fm", "Adark", "Fs", "Fm") %>%
      mutate("date" = str_split_fixed(list$date, "[:blank:](?=\\d)", 2)[,1])%>%
      mutate("date" = str_sub(date,start = 6))
    
   include_name <- cbind(filename, subset_csv)
   
  completeA6800<- if(exists('completeA6800') == TRUE) rbind(completeA6800, include_name) else(include_name)
  }

# Edit & Add Column Names -------------------------------------------------
#extract file name data as columns for graphing
completeA6800$filename<- gsub(".csv", "", completeA6800$filename)
completeA6800[c('Date','Species', 'VineNumber')] <- str_split_fixed(completeA6800$filename, '_', 3)
completeA6800[c('Month', 'Date')] <- str_split_fixed(completeA6800$date, '-', 2)

# assign treatment
DF <- completeA6800[grep("1|2|3|4|5|6|7|8|9|10", completeA6800$VineNumber),]
DF$Treatment <- ifelse(grepl("1|2|3|4|5", DF$VineNumber), "D", "C")

#view(completeA6800)
#view(DF)

# Discard Negative gsw and Ci -------------------------------------------------
dd_wreps <- do.call(bind_rows, DF)
Nonegs<-dd_wreps%>%filter(gsw >0 & Ci>0)

# Establish &Remove Outliers Using Quartiles  ---------------------------------
CiQ1 <- quantile(Nonegs$Ci, probs=.25, na.rm = TRUE) #"type 7" default
CiQ3 <- quantile(Nonegs$Ci, probs=.75, na.rm = TRUE)
AQ1 <- quantile(Nonegs$A, probs=.25, na.rm = TRUE)
AQ3 <- quantile(Nonegs$A, probs=.75, na.rm = TRUE)
IQRCi <- IQR(Nonegs$Ci, na.rm=TRUE, type = 7)
IQRA <- IQR(Nonegs$A, na.rm=TRUE)

no_outliers <- subset(Nonegs, Nonegs$Ci > (CiQ1 - 1.5*IQRCi) 
                      & Nonegs$Ci< (CiQ3 + 1.5*IQRCi) 
                      & Nonegs$A > (AQ1 - 1.5*IQRA) 
                      & Nonegs$A < (AQ3 + 1.5*IQRA))

no_outliers$MonthSpecies <- paste(no_outliers$Species, no_outliers$Month,  sep="_")
no_outliers$LeafID <- paste(no_outliers$MonthSpecies, no_outliers$VineNumber,no_outliers$Treatment,sep="_")
no_outliers$Species_Treatment <- paste(no_outliers$Species, no_outliers$Treatment, sep="_")

# Sub-setting Data For Graphing -------------------------------------------

November <- no_outliers[which(no_outliers$Month == "11"),]
December <- no_outliers[which(no_outliers$Month == "12"),]

November$NSpecies_Treatment <- paste(November$Species, November$Treatment, sep="_")
December$DSpecies_Treatment <- paste(December$Species, December$Treatment, sep="_")

# Facet Plot --------------------------------------------------------------

individal_curves_facet <- ggplot(no_outliers, 
                                 aes(x = Ci, y = A, color = Treatment, group = LeafID)) +
  facet_wrap(~ LeafID) +
  geom_point() + 
  geom_line() +
  theme(legend.position="none")+
  theme_bw(base_size = 14)+
  scale_color_manual(values = c("C" = "red", "D" = "green"))
individal_curves_facet

#ggsave(paste0("fig_output/facet", ".png"))
#ggsave(paste0("fig_output/facet", ".pdf"))

# Average ggplots ---------------------------------------------------------

ggplot(data=no_outliers, aes(x=Ci, y=A, col= as.factor(Species_Treatment)))+
  geom_point()+
  geom_smooth()+
  labs(title = "ACi combined Control v.s. Drought by Species")

species_order<-unique(no_outliers$Species)
for (i in species_order) {
ACiPlot<- no_outliers%>%
    filter(Species== i)%>%
    ggplot(aes(x = Ci, y = A, color = LeafID))+
    geom_point()+
    geom_line()+
  ggtitle(paste("ACi-Curves of", i))
  print(ACiPlot) 
  
  ggsave(paste0("fig_output/ACi",i, ".png"))
  ggsave(paste0("fig_output/ACi",i, ".pdf"))
} 

# Fitting ACi Curves ------------------------------------------------------

library(plantecophys)
library(devtools)

# create individual tibbles for extraction --------------------------------

tibblegroup <- split(no_outliers, no_outliers$LeafID)
names(tibblegroup) #use names to find names of tibbles
#summary(tibblegroup)
df1 <- names(tibblegroup)


#f <- fitaci(no_outliers, fitmethod="bilinear")


for (fitfile in tibblegroup) {

fit_fitfile <- fitaci(fitfile, varnames=list(ALEAF="A", Ci="Ci", Tleaf="Tleaf", PPFD="Qin"))
#plot(fit_fitfile, addlegend = TRUE)
#fit_fitfile$Photosyn(600)
#fit_fitfile$Ci(0) #compensation point
#coef(fit_fitfile)
} 


# find AMax ---------------------------------------------------------------

Amax <- 
  no_outliers %>%
  group_by(LeafID) %>%
  summarise(max(A))

View(Amax)
#write_xlsx(Amax, "Amax.xlsx")



