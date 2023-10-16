# Combining Soil Water Contents and Water Usage over all species of Subset

# Packages ----------------------------------------------------------------
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(tidyverse)
#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data -------------------------------------------------------------
swc_sub_small<- read.csv("data/Subset/sub_small_swc.csv")

# SWC Calculations --------------------------------------------------------

# SWC individual Species in one plot --------------------------------------
# Pre-Water SWC Graphs---------------------------------------------------  
PSWCplot<-swc_sub_small%>% 
  ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = species_geno, 
             shape = Treatment, group= interaction(species_geno, Treatment)))+
  geom_point()+ 
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  scale_y_continuous(name = "Pre-Watering SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
  xlab(label = "")

print(PSWCplot)

#ggsave(paste0("fig_output/Subset_small/SWC/PSWCplot_byspecies", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/PSWCplot_byspecies", ".pdf"))


# Post-Watering SWC -------------------------------------------------------

POSWCplot<-swc_sub_small%>% 
  ggplot(aes(y =POSWC, x = as.Date(Date,"%m/%d"), color = species_geno, 
             shape = Treatment, group= interaction(species_geno, Treatment)))+
  geom_point()+ 
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  scale_y_continuous(name = "Post-Watering SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
  xlab(label = "")
print(POSWCplot)

#ggsave(paste0("fig_output/Subset_small/SWC/POSWCplot_byspecies", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/POSWCplot_byspecies", ".pdf"))


# Water Usage -------------------------------------------------------------

wuplot<-swc_sub_small%>% 
  ggplot(aes(y =WU, x = as.Date(Date,"%m/%d"), color = species_geno, 
             shape = Treatment, group= interaction(species_geno, Treatment)))+
  geom_point()+
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  scale_y_continuous(name = "WU (L)", limits=c(0.0, 2.1), breaks = seq(0.0, 2.1, by = 0.2))+ 
  xlab(label = "")
print(wuplot)

#ggsave(paste0("fig_output/Subset_small/SWC/WUplot_byspecies", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/WUplot_byspecies", ".pdf"))

# Merge PSWC & POSWC ------------------------------------------------------

plot1 <-ggarrange(PSWCplot, NULL, POSWCplot, ncol=1, nrow=3, heights =  c(3, 0.00001, 3), 
                  common.legend = TRUE, legend="bottom",align = "v")
combined <- annotate_figure(plot1, top = text_grob((paste("SWC before and after Watering by Species")), color = "black", face = "italic", size = 11)) 

print(combined)
#ggsave(paste0("fig_output/Subset_small/SWC/combinedSWCplot_byspecies", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/combinedSWCplot_byspecies", ".pdf"))

# over all species together

# SWC Over all Species ---------------------------------------------------------
# Pre-Water SWC Graphs---------------------------------------------------  
PSWCplot_all<-swc_sub_small%>% 
  ggplot(aes(y =PSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
  geom_point()+ 
  geom_line()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  scale_y_continuous(name = "Pre-Watering SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
  scale_color_manual(values = c("Control" = rgb(2, 40, 80, maxColorValue = 255), "Drought" = rgb(255, 204, 0, maxColorValue = 255)))+
  xlab(label = "")

print(PSWCplot_all)

#ggsave(paste0("fig_output/Subset_small/SWC/PSWCplot_all", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/PSWCplot_all", ".pdf"))

# Post-Water SWC Graphs--------------------------------------------------- 
POSWCplot_all<-swc_sub_small%>% 
  ggplot(aes(y =POSWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
  geom_point()+ 
  geom_point()+
  geom_line()+
  geom_line()+
  theme_classic()+
  #theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  coord_fixed(ratio = 30)+
  scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
  scale_y_continuous(name = "Post-Watering SWC" , labels = scales::percent, limits=c(0.6, 1.15), breaks = seq(0.6, 1.15, by = 0.1))+
  scale_color_manual(values = c("Control" = rgb(2, 40, 80, maxColorValue = 255), "Drought" = rgb(255, 204, 0, maxColorValue = 255)))+
  xlab(label = "")

print(POSWCplot_all)

#ggsave(paste0("fig_output/Subset_small/SWC/POSWCplot_all", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/POSWCplot_all", ".pdf"))

# Merge PSWC,  POSWC over all species --------------------------------------------

plot_all <-ggarrange(PSWCplot_all, NULL, POSWCplot_all, ncol=1, nrow=3, 
                     heights =  c(3, 0.00001, 3), 
                     common.legend = TRUE, legend="bottom",align = "v")

combined_all <- annotate_figure(plot_all, top = text_grob((
  paste("SWC before & after watering over time")), 
  color = "black", face = "italic", size = 11)) 

print(combined_all)
#ggsave(paste0("fig_output/Subset_small/SWC/SWCplot_allcombined", ".png"))
#ggsave(paste0("fig_output/Subset_small/SWC/SWCplot_allcombined", ".pdf"))



WP_final <-read.csv("data/Subset/new/combined_WP.csv")
WP_final<- (WP_final%>%
              filter(species_geno %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

# to select Wanted Days -------------------------------------------------------------

WP_final1 <- WP_final%>% 
  group_by(Date_group) %>%
  filter(any(Date_group == c("Dec 11 to 18")))


# Plot Graph --------------------------------------------------------------

new_plot<-WP_final1%>%
  ggplot(aes(x = species_geno, Y = WP, fill = Treatment))+
  geom_boxplot(aes(y =-WP))+
  #scale_y_continuous(name = "", limits=c(-20, -1), breaks = seq(-20, -1, by = 2))+
  theme_classic()+
  xlab("Genotype")+
  ylab("WP (MPa)")+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  facet_grid(facets = WPType ~ .)+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  ggtitle("WP Dec 11 to 18")
print(new_plot)


# Save Graph --------------------------------------------------------------
#path to save final day files: 
# ggsave(paste0("fig_output/Subset_small/bygenotype/WPcombined/WPcombined1", ".png"))
# ggsave(paste0("fig_output/Subset_small/bygenotype/WPcombined/WPcombined1", ".pdf"))



# all A combined  ----------------------------------------------------------

genos<-unique(gswclean_6800$species_geno)

str(gswclean_6800)

gswclean_6800 <- gswclean_6800%>% 
  group_by(Date) %>%
  filter(any(Date == c("11/12-13")))

new_plot1<-gswclean_6800%>%
  ggplot(aes(x = species_geno, Y = gsw_6800, fill = Treatment))+
  geom_boxplot(aes(y =gsw_6800))+
  theme_classic()+
  ylab("Gs")+
  xlab("")+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  ggtitle("")
print(new_plot)



new_plot2<-gswclean_6800%>%
  ggplot(aes(x = species_geno, Y = A, fill = Treatment))+
  geom_boxplot(aes(y =A))+
  theme_classic()+
  ylab("A")+
  xlab("")+
  theme(legend.position="none") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  ggtitle("")
print(new_plot2)



gsw_data_long <- gswclean_6800 %>% gather(key = "Type", value = "value", A, gsw_6800  )



gsw_names <- c(
  `A` = " A" ,
  `gsw_6800` = "Gs"
)


gsw_data_long_plot<-gsw_data_long %>%
  ggplot(aes(x = Genotype, y = value, fill = Treatment))+
  geom_boxplot(aes(y = value))+
  theme_classic()+
  theme(legend.position="bottom")+
  #theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_grid(facets = Type ~ ., scales = "free", labeller = as_labeller(gsw_names))+
  scale_fill_manual(values = c("Control" = 
                                 rgb(2, 40, 80, maxColorValue = 255), "Drought" = rgb(255, 204, 0, maxColorValue = 255)))+
  xlab("")+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  ylab("")+
  ggtitle("")

print(gsw_data_long_plot)



# Import Data -------------------------------------------------------------
gswclean_6800_combined<- read.csv("data/Subset/new/Licor.csv")

gswclean_6800_combined<- (gswclean_6800_combined%>%
                            filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                                   "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

gswclean_combined<- read.csv("data/Subset/sub_small_Licor.csv")

str(gswclean_combined)
# Plot Graph --------------------------------------------------------------

gswclean_combined_plot<-gswclean_combined%>%
  ggplot(aes(y =A, x = Genotype, color = Treatment))+
  geom_boxplot(aes(y =A))+
  geom_point()+
  theme_classic()+
  xlab("Genotype and Date")+
  ylab("A (mol m−2 s−1)")+
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("A all dates by Species")
print(gswclean_combined_plot)


# Save Graph --------------------------------------------------------------
#path to save final day files: 
ggsave(paste0("fig_output/Subset_small/Licoralldates_combined", ".png"))
ggsave(paste0("fig_output/Subset_small/Licoralldates_combined", ".pdf"))


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
  group_by(Code_Date, ID, Date, Treatment, Genotype) #%>%
#summarise_at(vars(gsw_porometer), mean)


# ifelse for Date ---------------------------------------------------------

# used last day of sampling as the date in graph 

gswclean$Date <- ifelse(gswclean$Date=="10/26"|gswclean$Date=="10/27","10/27",
                        ifelse(gswclean$Date=="11/16"|gswclean$Date=="11/17","11/17",
                               ifelse(gswclean$Date=="11/02"|gswclean$Date=="11/03", "11/03",
                                      ifelse(gswclean$Date=="11/23"|gswclean$Date=="11/24", "11/24",
                                             ifelse(gswclean$Date=="11/30"|gswclean$Date=="12/2","12/2",
                                                    gswclean$Date)))))


# Plot Graph ---------------------------------------------------------


gswclean_plot<-gswclean%>%
  ggplot(aes(y =gsw_porometer, x = as.Date(Date,"%m/%d"), color = Genotype, 
             shape = Treatment, group= interaction(Genotype, Treatment)))+
  geom_line()+
  geom_point()+
  theme_classic()+
  xlab("Genotype and Date")+
  ylab("Gs (mmol m−2 s−1)")+
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  ggtitle("Gs all dates by Species")

print(gswclean_plot)

#path to save subset files: 
ggsave(paste0("fig_output/Subset_small/GS_alldates_combined", ".png"))
ggsave(paste0("fig_output/Subset_small/GS_alldates_combined", ".pdf"))




# Biomass -----------------------------------------------------------------

joined_harvestdf <- read.csv("data/Subset/joined_harvest.csv")
str(joined_harvestdf)

# Canopy Biomass ----------------------------------------------------------

canopy_plot <- joined_harvestdf %>%
  ggplot(aes(x=Genotype,y=canopy_biomass, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Canopy Biomass by Species")+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  theme(legend.position = 'none') 

print(canopy_plot)

ggsave(paste0("fig_output/Subset_small/canopy_plot", ".png"))
ggsave(paste0("fig_output/Subset_small/canopy_plot", ".pdf"))


# Root Biomass ----------------------------------------------------------

root_plot <- joined_harvestdf %>%
  ggplot(aes(x=Genotype,y=root_biomass, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Root Biomass by Species")+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  theme(legend.position = 'none') 

print(root_plot)

ggsave(paste0("fig_output/Subset_small/root_plot", ".png"))
ggsave(paste0("fig_output/Subset_small/root_plot", ".pdf"))      

# Leaf Area Biomass ----------------------------------------------------------

leaf_area_plot <- joined_harvestdf %>%
  ggplot(aes(x=Genotype,y=TotalLeafArea, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Total Leaf Area by Species")+
  scale_fill_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  theme(legend.position = 'none') 

print(leaf_area_plot)

ggsave(paste0("fig_output/Subset_small/leaf_area_plot", ".png"))
ggsave(paste0("fig_output/Subset_small/leaf_area_plot", ".pdf")) 


# Biomass Combined --------------------------------------------------------
biomass <-  read.csv("data/Subset/gathered_harvest_biomass.csv")

biomass_names <- c(
  `canopy_biomass` = "canopy biomass",
  `root_biomass` = "root biomass",
  `TotalLeafArea` = "total leaf area"
)

biomass_plot<-biomass%>%
  ggplot(aes(x = Genotype, y = value, fill = Treatment))+
  geom_boxplot(aes(y = value))+
  theme_classic()+
  theme(legend.position="bottom")+
  #theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_grid(facets = Type ~ ., scales = "free", labeller = as_labeller(biomass_names))+
  scale_fill_manual(values = c("Control" = 
                                  rgb(2, 40, 80, maxColorValue = 255), "Drought" = rgb(255, 204, 0, maxColorValue = 255)))+
  xlab("")+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  ylab("")+
  ggtitle("")

print(biomass_plot)

#ggsave(paste0("fig_output/Subset_small/biomass_plot_combined", ".png"))
#ggsave(paste0("fig_output/Subset_small/biomass_plot_combined", ".pdf"))

# Segmentation ------------------------------------------------------------


segmentation_data<- read.csv("data/Subset/segmentation_all.csv")
str(segmentation_data)


porosity_plot <- segmentation_data %>%
  ggplot(aes(x=Species,y=Porosity, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  #stat_compare_means(method = "anova", label.y = 0.55)+
  #stat_compare_means(comparisons = Treatment, method = "t.test", label.y =0.7)+
  ggtitle("Porosity by Species")+
  scale_fill_manual(values = c("C" = "blue", "D" = "orange"))+
  theme(legend.position = 'none') 

print(porosity_plot)

#ggsave(paste0("fig_output/Subset_small/porosity_plot", ".png"))
#ggsave(paste0("fig_output/Subset_small/porosity_plot", ".pdf"))

MS_TM_plot <- segmentation_data %>%
  ggplot(aes(x=Species,y=Mesophyll_S.V.Total.mesophyll.V, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Spongy Mesophyll Volume vs. Total Mesophyll Volume by Species")+
  scale_fill_manual(values = c("C" = "blue", "D" = "orange"))+
  theme(legend.position = 'none') 

print(MS_TM_plot)

#ggsave(paste0("fig_output/Subset_small/MS_TM_plot", ".png"))
#ggsave(paste0("fig_output/Subset_small/MS_TM_plot", ".pdf"))

MP_TM_plot <- segmentation_data %>%
  ggplot(aes(x=Species,y=Mesophyll_P.V.Total.mesophyll.V, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Palisade Mesophyll Volume vs. Total Mesophyll Volume by Species")+
  scale_fill_manual(values = c("C" = "blue", "D" = "orange"))+
  theme(legend.position = 'none') 

print(MP_TM_plot)

#ggsave(paste0("fig_output/Subset_small/MP_TM_plot", ".png"))
#ggsave(paste0("fig_output/Subset_small/MP_TM_plot", ".pdf"))


MS_MP_plot <- segmentation_data %>%
  ggplot(aes(x=Species,y=Mesophyll_SV.Mesophyll_PV, fill = Treatment )) + 
  geom_boxplot()+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  theme_classic()+
  xlab("")+
  ggtitle("Spongy Mesophyll Volume / Palisade Mesophyll Volume by Species")+
  scale_fill_manual(values = c("C" = 
                                 rgb(2, 40, 80, maxColorValue = 255), "D" = rgb(255, 204, 0, maxColorValue = 255)))+
  theme(legend.position = 'none') 

print(MS_MP_plot)

#ggsave(paste0("fig_output/Subset_small/MS_MP_plot", ".png"))
#ggsave(paste0("fig_output/Subset_small/MS_MP_plot", ".pdf"))


plot_segmentation_all <-ggarrange(porosity_plot, MS_MP_plot, MP_TM_plot, MS_TM_plot, ncol=2, nrow=2, 
                                  heights =  c(3, 3), 
                                  common.legend = TRUE, legend="bottom",align = "v")


combined_segmentation_all <- annotate_figure(plot_all, top = text_grob((
  paste("SWC before & after watering over time")), 
  color = "black", face = "italic", size = 11)) 

print(combined_segmentation_all)

#ggsave(paste0("fig_output/Subset_small/combined_segmentation_all", ".png"))
#ggsave(paste0("fig_output/Subset_small/combined_segmentation_all", ".pdf"))


# combine porosity and spongy/palisade ------------------------------------
str(segmentation_data)

segmentation_names <- c(
  `Mesophyll_SV.Mesophyll_PV` = "Spongy/ Palisade Volume",
  `Porosity` = "Porosity")

segmentation_data_long <- segmentation_data %>% gather(key = "SegType", value = "Seg", Porosity,Mesophyll_SV.Mesophyll_PV )

segmentation_data_long <- na.omit(segmentation_data_long)



combi_plot<-segmentation_data_long%>%
  ggplot(aes(x = Species, y = Seg, fill = Treatment))+
  geom_boxplot(aes(y = Seg))+
  theme_classic()+
  theme(legend.position="bottom")+
  #theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_grid(facets = SegType ~ ., scales = "free", labeller = as_labeller(segmentation_names))+
  scale_fill_manual(values = c("C" = 
                                 rgb(2, 40, 80, maxColorValue = 255), "D" = rgb(255, 204, 0, maxColorValue = 255)))+
  xlab("")+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  ylab("")+
  ggtitle("")

print(combi_plot)







