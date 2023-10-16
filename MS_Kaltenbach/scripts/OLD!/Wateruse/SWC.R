library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(broom)

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# read file for entire data set & modify data 
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
             mutate( species_geno = paste(Species, Genotype, sep = "_"))) 

swclong <- swclong[, c("Genotype", "Species", "Treatment", "WU", "SWC", "Date", "species_geno")]

#### SWC Graphs INDIVIDUAL---------------------------------------------------  
accessions<-unique(swclong$species_geno)

for (i in accessions) {
            error.swc <- swclong %>%
              filter(species_geno == i)%>%
              group_by(Date, Treatment) %>%
              summarise(
                sd = sd(SWC, na.rm = TRUE),
                se = (sd(SWC)/sqrt(length(SWC))),
                len = mean(SWC),
                Treatment = Treatment,
                Date = Date)

  SWCplot<-swclong%>% 
    filter(species_geno == i)%>%
    ggplot(aes(y =SWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ 
    stat_summary(fun="mean",geom="line",size = 1)+ 
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.1),
                  size = .2,
                  linetype = "solid",
                  data = distinct(error.swc))+
    theme_classic()+
    scale_color_manual(values = c("Control" = "blue", "Drought" = "orange"))+
    stat_smooth(data = . %>% filter(Treatment == "Control"), 
                method = "loess", se = FALSE, color = "blue") +
    stat_smooth(data = . %>% filter(Treatment == "Drought"), 
                method = "loess", se = FALSE, color = "orange") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    scale_y_continuous(name = "SWC" , labels = scales::percent, limits=c(0, 1.15), 
                       breaks = seq(0, 1.15, by = 0.1))+
    theme(legend.position="bottom")+
    ggtitle(paste("SWC of", i))
  
  
  print(SWCplot)

  #path to save all files: 
  #ggsave(paste0("fig_output/SWC/SWC/SWC_",i, ".png"))
  #ggsave(paste0("fig_output/SWC/SWC/SWC_",i, ".pdf"))
  
  #path to save subset files: 
  #ggsave(paste0("fig_output/Subset_small/SWC/PSWC1/PSWC1",i, ".png"))
  #ggsave(paste0("fig_output/Subset_small/SWC/PSWC1/PSWC1",i, ".pdf"))
  
  
  #### WU Graphs INDIVIDUAL--------------------------------------------------- 
  
  error.WU <- swclong %>%
    filter(species_geno == i)%>%
    group_by(Date, Treatment) %>%
    summarise(
      sd = sd(WU, na.rm = TRUE),
      se = (sd(WU)/sqrt(length(WU))),
      len = mean(WU),
      Treatment = Treatment,
      Date = Date)
  
  WUplot<-swclong%>% 
    filter(species_geno == i)%>%
    ggplot(aes(y =WU, x = as.Date(Date,"%m/%d"), color = Treatment))+
    geom_point()+ 
    stat_summary(fun="mean",geom="line",size = 1)+ 
    geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len,
                      ymin = len-se,
                      ymax = len+se),
                  color = "black",
                  position=position_dodge(width=0.1),
                  size = .2,
                  linetype = "solid",
                  data = distinct(error.WU))+
    theme_classic()+
    scale_color_manual(values = c("Control" = "blue", "Drought" = "orange"))+
    stat_smooth(data = . %>% filter(Treatment == "Control"), 
                method = "loess", se = FALSE, color = "blue") +
    stat_smooth(data = . %>% filter(Treatment == "Drought"), 
                method = "loess", se = FALSE, color = "orange") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    scale_x_date(date_labels="%m/%d",date_breaks  ="7 days")+
    xlab(label = "Date")+
    scale_y_continuous(name = "WU" , labels = scales::percent, limits=c(0, 1.15), 
                       breaks = seq(0, 1.15, by = 0.1))+
    theme(legend.position="bottom")+
    ggtitle(paste("Water Usage of", i))
  
  
  print(WUplot)
  
  #path to save all files: 
    #   ggsave(paste0("fig_output/SWC/WU/WU_",i, ".png"))
    #  ggsave(paste0("fig_output/SWC/WU/WU_",i, ".pdf"))
  

  

  # Combine SWC & WU plot by species ----------------------------------------

  plot <-ggarrange(SWCplot, NULL, WUplot, ncol=1, nrow=3, heights =  c(3, 0.00001, 3), 
                    common.legend = TRUE, legend="bottom",align = "v")
  combined_byspecies <- annotate_figure(plot, top = text_grob((paste("SWC & WU  of", i)), 
                                                    color = "black", face = "italic", size = 11)) 
  
  print(combined_byspecies)
  
  #path to save all files: 
 # ggsave(paste0("fig_output/SWC/combined/combined_",i, ".png"))
#  ggsave(paste0("fig_output/SWC/combined/combined_",i, ".pdf"))
  
}


# Plot SWC across ALL Species ---------------------------------------------

# Calculate quartiles for each Treatment
swc_quartiles <- swclong %>% 
        group_by(Treatment) %>% 
        summarize(q1 = quantile(SWC, 0.25, na.rm = TRUE),
                  q3 = quantile(SWC, 0.75, na.rm = TRUE))

# Define upper and lower limits for each Treatment
swc_limits <- swc_quartiles %>% 
        mutate(IQR = q3 - q1,
         lower_limit = q1 - 1.5*IQR,
         upper_limit = q3 + 1.5*IQR)

# Filter out outliers by Treatment
swc_filtered <- swclong %>% 
      left_join(swc_limits, by = "Treatment") %>% 
      filter(SWC >= lower_limit & SWC <= upper_limit)

error.all.swc <- swclong %>%
      group_by(Date, Treatment) %>%
      summarise(
        sd = sd(SWC, na.rm = TRUE),
        se = (sd(SWC)/sqrt(length(SWC))),
        len = mean(SWC), 
        Treatment = Treatment,
        Date = Date)

# Plot SWC without outliers by Treatment
SWC_all_plot <- swc_filtered %>% 
  ggplot(aes(y = SWC, x = as.Date(Date,"%m/%d"), color = Treatment))+
  geom_point(na.rm = TRUE)+ 
  stat_summary(fun = mean, geom = "line", size = 1, na.rm = TRUE)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.1, na.rm = TRUE)+
  theme_classic()+
  geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, #is standard error here correct written?
                    ymin = len-se,
                    ymax = len+se),
                color = "black",
                position=position_dodge(width=0.2),
                size = .3,
                linetype = "solid",
                data = distinct(error.all.swc))+
  stat_smooth(data = . %>% filter(Treatment == "Control"), 
              method = "loess", se = FALSE, color = "blue") +
  stat_smooth(data = . %>% filter(Treatment == "Drought"), 
              method = "loess", se = FALSE, color = "orange") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
  xlab(label = "Date")+
  scale_color_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  scale_y_continuous(name = "SWC", labels = scales::percent, 
                     limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
  theme(legend.position = "bottom")+
  ggtitle(paste("SWC across all Vitis species over time"))

print(SWC_all_plot)

#path to save all files: 
   # ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_", ".png"))
   # ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_", ".pdf"))

##Water Use Graphs ALL Species------------------------------------------------

# Calculate quartiles for each Treatment
WU_quartiles <- swclong %>% 
  group_by(Treatment) %>% 
  summarize(q1 = quantile(WU, 0.25, na.rm = TRUE),
            q3 = quantile(WU, 0.75, na.rm = TRUE))

# Define upper and lower limits for each Treatment
WU_limits <- WU_quartiles %>% 
  mutate(IQR = q3 - q1,
         WU_lower_limit = q1 - 1.5*IQR,
         WU_upper_limit = q3 + 1.5*IQR)

# Filter out outliers by Treatment
WU_filtered <- swclong %>% 
  left_join(WU_limits, by = "Treatment") %>% 
  filter(SWC >= WU_lower_limit & SWC <= WU_upper_limit)

error.all.WU <- swclong %>%
  group_by(Date, Treatment) %>%
  summarise(
    sd = sd(WU, na.rm = TRUE),
    se = (sd(WU)/sqrt(length(WU))),
    len = mean(WU), 
    Treatment = Treatment,
    Date = Date)

# Plot SWC without outliers by Treatment
WU_all_plot <- WU_filtered %>% 
  ggplot(aes(y = WU, x = as.Date(Date,"%m/%d"), color = Treatment))+
  geom_point(na.rm = TRUE)+ 
  stat_summary(fun = mean, geom = "line", size = 1, na.rm = TRUE)+
  stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1), 
               geom = "errorbar", width = 0.1, na.rm = TRUE)+
  theme_classic()+
  geom_errorbar(aes(x = as.Date(Date,"%m/%d"), y = len, #is standard error here correct written?
                    ymin = len-se,
                    ymax = len+se),
                color = "black",
                position=position_dodge(width=0.2),
                size = .3,
                linetype = "solid",
                data = distinct(error.all.WU))+
  stat_smooth(data = . %>% filter(Treatment == "Control"), 
              method = "loess", se = FALSE, color = "blue") +
  stat_smooth(data = . %>% filter(Treatment == "Drought"), 
              method = "loess", se = FALSE, color = "orange") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
  xlab(label = "Date")+
  scale_color_manual(values = c("Control" = "blue", "Drought" = "orange"))+
  scale_y_continuous(name = "WU (L)", limits=c(0.0, 2.1), breaks = seq(0.0, 2.1, by = 0.2))+
  theme(legend.position = "bottom")+
  ggtitle(paste("WU across all Vitis species over time"))

print(WU_all_plot)

#path to save all files: 
    # ggsave(paste0("fig_output/SWC/WU/ALL_WU", ".png"))
    # ggsave(paste0("fig_output/SWC/WU/ALL_WU", ".pdf"))

all_plot_swc_wu <-ggarrange(SWC_all_plot, NULL, WU_all_plot, ncol=1, nrow=3, heights =  c(3, 0.00001, 3), 
                 common.legend = TRUE, legend="bottom",align = "v")
all_combined_swc_wu <- annotate_figure(all_plot_swc_wu, 
                                       top = text_grob((paste("SWC & WU  across all Vitis species"
                                                              )), color = "black", 
                                                       face = "italic", size = 11)) 

print(all_combined_swc_wu)

#path to save all files: 
  #  ggsave(paste0("fig_output/SWC/combined/all_combined_swc_wu_", ".png"))
  #  ggsave(paste0("fig_output/SWC/combined/all_combined_swc_wu", ".pdf"))

