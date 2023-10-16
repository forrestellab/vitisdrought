library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(broom)
library(lubridate)
library(gtExtras)
library(gt)

#set working directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# read file for entire data set & modify data 
swclong<-read.csv("data/WaterUse_SWC_Clean.csv") 
swclong<- (swclong%>%
             filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                    "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))%>%
             mutate( species_geno = paste(Species, Genotype, sep = "_"))) 

swclong <- swclong[, c("Genotype", "Species", "Treatment", "WU", "SWC", "Date", "species_geno")]

# Convert the date column to a Date object
      swclong$Date <- as.Date(swclong$Date, format = "%m/%d")
      swclong$Month <- month(swclong$Date, label = TRUE)
      swclong$Day <- day(swclong$Date)
      swclong$MonthDay <- paste0(swclong$Month, "-", swclong$Day)
      unique_dates <- unique(swclong$MonthDay)

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
    
    # Plot SWC without outliers by Treatment -------------------------------------
        grouped_data_swc <- swc_filtered  %>% group_by(Date, Treatment)
        
        summary_data_swc <- grouped_data_swc%>% 
          summarize(mean_value_swc = mean(SWC),
                    sem_swc = sd(SWC) / sqrt(n()))
        
    
    SWC_all_plot <- ggplot(summary_data_swc, aes(x = Date, y = mean_value_swc, 
                                               color = Treatment)) +
                    geom_point(size = 3, na.rm=TRUE) +
                    geom_errorbar(aes(ymin = mean_value_swc - sem_swc, 
                                      ymax = mean_value_swc + sem_swc), width = 0.2) +
                    labs(x = "Date", y = "SWC") +
                    theme_bw()+
                    theme_classic() +
                    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
                    scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
                    xlab(label = "Date")+
                    scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
                    scale_y_continuous(name = "SWC", labels = scales::percent, 
                                       limits = c(0, 1), breaks = seq(0, 1, by = 0.1))+
                    theme(legend.position = "bottom")+
                    ggtitle(paste("SWC across all Vitis species over time"))
                  
    print(SWC_all_plot)
    
    #path to save all files: 
      ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_dot", ".png"))
       ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_dot", ".pdf"))
    

      # SWC by species ----------------------------------------------------------
  
      grouped_data_swc_species <- swc_filtered  %>% group_by(Date, Treatment, species_geno)
      
      summary_data_swc_species <- grouped_data_swc_species%>% 
        summarize(mean_value_swc = mean(SWC),
                  sem_swc = sd(SWC) / sqrt(n()))
    

      SWC_species_plot <- ggplot(summary_data_swc_species, aes(x = Date, y = mean_value_swc, 
                                                   color = Treatment)) +
        geom_point(size = 3, na.rm=TRUE) +
        geom_errorbar(aes(ymin = mean_value_swc - sem_swc, 
                          ymax = mean_value_swc + sem_swc), width = 0.2) +
        labs(x = "Date", y = "SWC") +
        theme_bw()+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))+
        scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
        xlab(label = "Date")+
        scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
        scale_y_continuous(name = "SWC", labels = scales::percent, 
                           limits = c(0, 1), breaks = seq(0, 1, by = 0.2))+
        theme(legend.position = "bottom")+
        facet_wrap(~ species_geno) +
        ggtitle(paste("SWC across all Vitis species over time"))
      
      print(SWC_species_plot)    

      #path to save all files: 
      ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_species_dot", ".png"))
      ggsave(paste0("fig_output/SWC/SWC/ALL_SWC_species_dot", ".pdf"))      
          
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


# Plot WU without outliers by Treatment -------------------------------------

grouped_data_WU <- WU_filtered  %>% group_by(Date, Treatment)

summary_data_WU <- grouped_data_WU%>% 
  summarize(mean_value_WU = mean(WU),
            sem_WU = sd(WU) / sqrt(n()))


WU_all_plot <- ggplot(summary_data_WU, aes(x = Date, y = mean_value_WU, 
                                             color = Treatment)) +
  geom_point(size = 3, na.rm=TRUE) +
  geom_errorbar(aes(ymin = mean_value_WU - sem_WU, 
                    ymax = mean_value_WU + sem_WU), width = 0.2) +
  labs(x = "Date", y = "WU") +
  theme_bw()+
  theme_classic() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
  xlab(label = "Date")+
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  scale_y_continuous(name = "WU (in L)",
                     limits = c(0, 1.6), breaks = seq(0, 1.6, by = 0.2))+
  theme(legend.position = "bottom")+
  ggtitle(paste("WU across all Vitis species over time"))

print(WU_all_plot)


#path to save all files: 
   ggsave(paste0("fig_output/SWC/WU/ALL_WU_dot", ".png"))
    ggsave(paste0("fig_output/SWC/WU/ALL_WU_dot", ".pdf"))



        
        # WU by species ----------------------------------------------------------
        
        grouped_data_WU_species <- WU_filtered  %>% group_by(Date, Treatment, species_geno)
        
        summary_data_WU_species <- grouped_data_WU_species%>% 
          summarize(mean_value_WU = mean(WU),
                    sem_WU = sd(WU) / sqrt(n()))
        
        WU_species_plot <- ggplot(summary_data_WU_species, 
                                   aes(x = Date, y = mean_value_WU, 
                                                                 color = Treatment)) +
          geom_point(size = 3, na.rm=TRUE) +
          geom_errorbar(aes(ymin = mean_value_WU - sem_WU, 
                            ymax = mean_value_WU + sem_WU), width = 0.2) +
          labs(x = "Date", y = "WU (in L)") +
          theme_bw()+
          theme_classic() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))+
          scale_x_date(date_labels = "%m/%d", date_breaks  = "7 days")+  
          xlab(label = "Date")+
          scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
          scale_y_continuous(name = "WU (in L)",
                             limits = c(0, 1.6), breaks = seq(0, 1.6, by = 0.2))+
          theme(legend.position = "bottom")+
          facet_wrap(~ species_geno) +
          ggtitle(paste("WU across all Vitis species over time"))
        
        print(WU_species_plot)    

        #path to save all files: 
        ggsave(paste0("fig_output/SWC/WU/ALL_WU_species_dot", ".png"))
        ggsave(paste0("fig_output/SWC/WU/ALL_WU_species_dot", ".pdf"))

# Combine SWC & Water Usage -----------------------------------------------


all_plot_swc_wu <-ggarrange(SWC_all_plot, NULL, WU_all_plot, ncol=1, nrow=3, heights =  c(3, 0.00001, 3), 
                 common.legend = TRUE, legend="bottom",align = "v")
all_combined_swc_wu <- annotate_figure(all_plot_swc_wu, 
                                       top = text_grob((paste("SWC & WU  across all Vitis species"
                                                              )), color = "black", 
                                                       face = "italic", size = 11)) 

print(all_combined_swc_wu)

#path to save all files: 
  ggsave(paste0("fig_output/SWC/combined/all_combined_swc_wu_dot", ".png"))
   ggsave(paste0("fig_output/SWC/combined/all_combined_swc_wu_dot", ".pdf"))


# WU TABLE ----------------------------------------------------------------

# Aggregate data by species, genotype, and treatment
water_use <- aggregate(WU ~ Species + Genotype + species_geno + Treatment, 
                       data = swclong, sum)
colnames(water_use)[4] <- "Total_Water_Use"

# create gt table
water_use_gt <- water_use %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Total Water Use")

gtsave(water_use_gt, filename = "fig_output/SWC/WU/water_use.pdf")
gtsave(water_use_gt, filename = "fig_output/SWC/WU/water_use.png")
gtsave(water_use_gt, filename = "fig_output/SWC/WU/water_use.html")

# Save the data as a CSV file in the specified directory
write.csv(water_use, file = "data_output/SWC/water_use_table.csv", row.names = FALSE)