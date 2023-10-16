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
  ggplot(aes(x=paste(Species, Genotype) ,y=mean_Porosity, color = Treatment )) + 
  geom_point(size = 4 )+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")) +
  xlab("")+
  ylab("Porosity")+
  geom_errorbar(aes(ymin = mean_Porosity - sem_Porosity, 
                    ymax = mean_Porosity + sem_Porosity), width = 0.2) +
  ggtitle("Porosity")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_Porosity < 0.05, "*", "ns"),
                x = paste(Species, Genotype), y = 0.2),
            size = 4,  color = "black")

print(porosity_plot)

#ggsave(paste0("fig_output/segmentation/porosity_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/porosity_plot", ".pdf"))

SpongyV_TotalMesophyllV_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_SpongyV_TotalMesophyllV, color = Treatment )) + 
  geom_point(size = 4 )+
  theme_bw()+
  theme(
    strip.background = element_rect(fill = "white")) +
  xlab("")+
  ylab("Spongy/ Total Mesophyll")+
  geom_errorbar(aes(ymin = mean_SpongyV_TotalMesophyllV - sem_SpongyV_TotalMesophyllV, 
                    ymax = mean_SpongyV_TotalMesophyllV + sem_SpongyV_TotalMesophyllV), width = 0.2) +
  ggtitle("Spongy / Total Mesophyll")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_SpongyV_TotalMesophyllV < 0.05, "*", "ns"),
                x = Genotype, y = 0.1),
            size = 4,  color = "black")

print(SpongyV_TotalMesophyllV_plot)

#ggsave(paste0("fig_output/segmentation/SpongyV_TotalMesophyllV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/SpongyV_TotalMesophyllV_plot", ".pdf"))

PalisadeV_Total_MesophyllV_plot <- segmentation_data %>%
  ggplot(aes(x=Genotype ,y=mean_PalisadeV_TotalMesophyllV, color = Treatment )) + 
  geom_point(size = 3 )+
  theme_bw()+
  theme(
    strip.background = element_rect(fill = "white")) +
  xlab("")+
  ylab("Palisade/ Total Mesophyll")+
  geom_errorbar(aes(ymin = mean_PalisadeV_TotalMesophyllV - sem_PalisadeV_TotalMesophyllV, 
                    ymax = mean_PalisadeV_TotalMesophyllV + sem_PalisadeV_TotalMesophyllV), width = 0.2) +
  ggtitle("Palisade/ Total Mesophyll")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_text(aes(label = ifelse(p_value_PalisadeV_TotalMesophyllV < 0.05, "*", "ns"),
                x = Genotype, y = 0.1),
            size = 4,  color = "black")

print(PalisadeV_Total_MesophyllV_plot)

#ggsave(paste0("fig_output/segmentation/PalisadeV_Total_MesophyllV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/PalisadeV_Total_MesophyllV_plot", ".pdf"))

SpongyV_PalisadeV_plot <- segmentation_data %>%
  ggplot(aes(x = paste(Species, Genotype), y = mean_SpongyV_PalisadeV, color = Treatment)) + 
  geom_point(size = 4) +
  theme_bw() +  # Set background to blank
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")) +
  xlab("") +
  ylab("Spongy/Palisade")+
  geom_errorbar(
    aes(ymin = mean_SpongyV_PalisadeV - sem_SpongyV_PalisadeV, 
        ymax = mean_SpongyV_PalisadeV + sem_SpongyV_PalisadeV),
    width = 0.2 ) +
  ggtitle("Spongy/Palisade Mesophyll") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_text(
    aes(label = ifelse(p_value_SpongyV_PalisadeV < 0.05, "*", "ns"), 
        x = paste(Species, Genotype), y = 0.45),
    size = 4, color = "black"
  )

print(SpongyV_PalisadeV_plot)


#ggsave(paste0("fig_output/segmentation/SpongyV_TotalMesophyllV_plot", ".png"))
#ggsave(paste0("fig_output/segmentation/SpongyV_TotalMesophyllV_plot", ".pdf"))



# Leaf Thickness ----------------------------------------------------------

Leaf_Width_um_plot <- segmentation_data %>%
  ggplot(aes(x = paste(Species, Genotype), y = mean_Leaf_Width_um, color = Treatment)) + 
  geom_point(size = 4) +
  theme_bw() +  # Set background to blank
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")) +
  xlab("") +
  ylab("Leaf Thiclness (um)")+
  geom_errorbar(
    aes(ymin = mean_Leaf_Width_um - sem_Leaf_Width_um, 
        ymax = mean_Leaf_Width_um + sem_Leaf_Width_um),
    width = 0.2 ) +
  ggtitle("Leaf Thickness") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_text(
    aes(label = ifelse(p_value_Leaf_Width_um < 0.05, "*", "ns"), 
        x = paste(Species, Genotype), y = 0.45),
    size = 4, color = "black"
  )

print(Leaf_Width_um_plot)


Mesophyll_Width_um_plot <- segmentation_data %>%
  ggplot(aes(x = paste(Species, Genotype), y = mean_Mesophyll_Width_um, color = Treatment)) + 
  geom_point(size = 4) +
  theme_bw() +  # Set background to blank
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")) +
  xlab("") +
  ylab("Mesophyll Thickness (um)")+
  geom_errorbar(
    aes(ymin = mean_Mesophyll_Width_um - sem_Mesophyll_Width_um, 
        ymax = mean_Mesophyll_Width_um + sem_Mesophyll_Width_um),
    width = 0.2 ) +
  ggtitle("Mesophyll Thickness") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  geom_text(
    aes(label = ifelse(p_value_Mesophyll_Width_um < 0.05, "*", "ns"), 
        x = paste(Species, Genotype), y = 0.45),
    size = 4, color = "black"
  )

print(Mesophyll_Width_um_plot)



plot_segmentation_all <-ggarrange(SpongyV_PalisadeV_plot, 
                                   porosity_plot, 
                                  ncol=2, nrow=1, 
                                  heights =  c(3, 3), 
                                  common.legend = TRUE, legend="bottom",align = "v")


combined_segmentation_all <- annotate_figure(plot_segmentation_all, top = text_grob((
  paste("Morphological Changes by Species and Treatment")), 
  color = "black", face = "italic", size = 11)) 

print(combined_segmentation_all)

#ggsave(paste0("fig_output/Subset_small/combined_segmentation_all", ".png"))
#ggsave(paste0("fig_output/Subset_small/combined_segmentation_all", ".pdf"))


# combine porosity and spongy/palisade ------------------------------------
library(tidyr)
library(ggplot2)
library(ggpubr)

segmentation_names <- c(
  `SpongyV_TotalMesophyllV` = "Spongy/Total MP",
  `Porosity` = "Porosity",
  `PalisadeV_TotalMesophyllV` = "Palisade/Total MP", 
  `SpongyV_PalisadeV` = "Spongy/Palisade MP"
)

segmentation_data_long <- segmentation_data %>%
  gather(key = "SegType", value = "Seg", Porosity, SpongyV_TotalMesophyllV, PalisadeV_TotalMesophyllV, SpongyV_PalisadeV) %>%
  mutate(p_value = case_when(
    SegType == "Porosity" ~ p_value_Porosity,
    SegType == "SpongyV_TotalMesophyllV" ~ p_value_SpongyV_TotalMesophyllV,
    SegType == "PalisadeV_TotalMesophyllV" ~ p_value_PalisadeV_TotalMesophyllV,
    SegType == "SpongyV_PalisadeV" ~ p_value_SpongyV_PalisadeV
  )) %>%
  na.omit()

segmentation_data_summary <- segmentation_data_long %>%
  group_by(SegType, Genotype, Treatment, p_value) %>%
  summarize(
    mean_Seg = mean(Seg),
    sem_Seg = sd(Seg) / sqrt(n())
  )

combi_plot <- segmentation_data_summary %>%
  ggplot(aes(x = Genotype, y = mean_Seg, color = Treatment)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean_Seg - sem_Seg, ymax = mean_Seg + sem_Seg), width = 0.2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  facet_grid(facets = SegType ~ ., scales = "free", labeller = as_labeller(segmentation_names)) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  geom_text(aes(label = ifelse(p_value < 0.05, "*", "ns"), y = 0.2), size = 4, color = "black")

print(combi_plot)
