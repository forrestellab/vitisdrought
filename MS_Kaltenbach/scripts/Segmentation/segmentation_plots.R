library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(hrbrthemes)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Read in Data ------------------------------------------------------------
segmentation_data <- read.csv("data_output/segmentation/segmentation_data.csv")

colnames(segmentation_data)


# Porosity ----------------------------------------------------------------
porosity_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_Porosity, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_Porosity - sem_Porosity, 
                    ymax = mean_Porosity + sem_Porosity), width = 0.2) +
  ggtitle("Porosity by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_Porosity < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(porosity_plot)

#ggsave(paste0("fig_output/segmentation/porosity_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/porosity_plot", ".pdf"))

Airspace_V_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_Airspace_V, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_Airspace_V - sem_Airspace_V, 
                    ymax = mean_Airspace_V + sem_Airspace_V), width = 0.2) +
  ggtitle("Airspace_V by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_Airspace_V < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(Airspace_V_plot)

#ggsave(paste0("fig_output/segmentation/Airspace_V_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/Airspace_V_plot", ".pdf"))

MS_V_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_MS_V, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_MS_V - sem_MS_V, 
                    ymax = mean_MS_V + sem_MS_V), width = 0.2) +
  ggtitle("MS_V by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_MS_V < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(MS_V_plot)

#ggsave(paste0("fig_output/segmentation/MS_V_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/MS_V_plot", ".pdf"))


MP_V_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_MP_V, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_MP_V - sem_MP_V, 
                    ymax = mean_MP_V + sem_MP_V), width = 0.2) +
  ggtitle("MP_V by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_MP_V < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(MP_V_plot)

#ggsave(paste0("fig_output/segmentation/MP_V_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/MP_V_plot", ".pdf"))

M_SP_V_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_M_SP_V, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_M_SP_V - sem_M_SP_V, 
                    ymax = mean_M_SP_V + sem_M_SP_V), width = 0.2) +
  ggtitle("M_SP_V by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_M_SP_V < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(M_SP_V_plot)

#ggsave(paste0("fig_output/segmentation/M_SP_V_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/M_SP_V_plot", ".pdf"))

M_SPA_V_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_M_SPA_V, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_M_SPA_V - sem_M_SPA_V, 
                    ymax = mean_M_SPA_V + sem_M_SPA_V), width = 0.2) +
  ggtitle("M_SPA_V by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_M_SPA_V < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(M_SPA_V_plot)

#ggsave(paste0("fig_output/segmentation/M_SPA_V_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/M_SPA_V_plot", ".pdf"))

MSV_by_TMV_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_MSV_by_TMV, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_MSV_by_TMV - sem_MSV_by_TMV, 
                    ymax = mean_MSV_by_TMV + sem_MSV_by_TMV), width = 0.2) +
  ggtitle("MSV_by_TMV by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_MSV_by_TMV < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(MSV_by_TMV_plot)

#ggsave(paste0("fig_output/segmentation/MSV_by_TMV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/MSV_by_TMV_plot", ".pdf"))

MPV_by_TMV_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_MPV_by_TMV, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_MPV_by_TMV - sem_MPV_by_TMV, 
                    ymax = mean_MPV_by_TMV + sem_MPV_by_TMV), width = 0.2) +
  ggtitle("MPV_by_TMV by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_MPV_by_TMV < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(MPV_by_TMV_plot)

#ggsave(paste0("fig_output/segmentation/MPV_by_TMV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/MPV_by_TMV_plot", ".pdf"))

MSV_by_MPV_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_MSV_by_MPV, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_classic()+
  xlab("")+
  geom_errorbar(aes(ymin = mean_MSV_by_MPV - sem_MSV_by_MPV, 
                    ymax = mean_MSV_by_MPV + sem_MSV_by_MPV), width = 0.2) +
  ggtitle("MSV_by_MPV by Species")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_MSV_by_MPV < 0.05, "*", "ns"),
                x = Genotype, y = 0.45),
            size = 4,  color = "black")

print(MSV_by_MPV_plot)

#ggsave(paste0("fig_output/segmentation/MSV_by_MPV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/MSV_by_MPV_plot", ".pdf"))





plot_segmentation_all <-ggarrange(MSV_by_MPV_plot, MPV_by_TMV_plot, MSV_by_TMV_plot, 
                                  M_SPA_V_plot, M_SP_V_plot, MP_V_plot, MS_V_plot, 
                                  Airspace_V_plot, porosity_plot, 
                                  ncol=3, nrow=3, 
                                  heights =  c(3, 3), 
                                  common.legend = TRUE, legend="bottom",align = "v")


combined_segmentation_all <- annotate_figure(plot_segmentation_all, top = text_grob((
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
  ggplot(aes(x = Genotype, y = Seg, fill = Treatment))+
  geom_boxplot(aes(y = Seg))+
  theme_classic()+
  theme(legend.position="bottom")+
  #theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  facet_grid(facets = SegType ~ ., scales = "free", labeller = as_labeller(segmentation_names))+
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  xlab("")+
  geom_point(position = position_jitterdodge(),size = 1,shape = 1 )+
  ylab("")+
  ggtitle("")

print(combi_plot)







