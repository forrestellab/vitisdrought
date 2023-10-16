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
combined_df <- read.csv("data_output/correlation/correlation_last_date")

combined_df <- read.csv("data_output/correlation/correlation_middle_date")
combined_df <- combined_df  %>% 
                select( -c("mean_value_leafwp_predawn", "mean_value_SWP" ))

# Create separate subsets for the control and drought treatments
control_df <- subset(combined_df, Treatment == "Control")
drought_df <- subset(combined_df, Treatment == "Drought")

drop_cols <- grepl("sd|p\\.value|sem|p\\_value", colnames(combined_df))
combined_df <- combined_df[, !drop_cols]

species_order <- combined_df %>% 
  arrange(desc(Annual.Precipitation)) %>% 
  pull(unique(species_geno))


# Create a list of column names FINAL DATE

var_cols <- c("Lat", "Long", "Annual.Mean.Temperature", "Annual.Precipitation", 
              "Temperature.Seasonality..standard.deviation..100.", "Mean.Temperature.of.Coldest.Quarter", 
              "mean_value_TotalLeafArea", "mean_value_canopy_biomass", 
              "mean_value_root_biomass", "mean_value_leafarea.wu", 
              "mean_value_canopy_biomass.wu", "mean_value_root_biomass.wu",
              "mean_value_LWP", "mean_value_PD", "mean_value_SWP", 
              "mean_value_leafwp_predawn", "WU", "mean_value_gsw", 
              "mean_value_A", "mean_value_E", "mean_value_intrWUE_AE", 
              "mean_value_instWUE_AGs", "mean_MS_V", "mean_MP_V", "mean_M_SP_V", "mean_M_SPA_V",
              "mean_MSV_by_TMV", "mean_MPV_by_TMV", "mean_MSV_by_MPV", "mean_Porosity", "mean_Airspace_V")

# Create a list of column names MIDDLE DATE
var_cols <- c("Lat", "Long", "Annual.Mean.Temperature", "Annual.Precipitation", 
              "Temperature.Seasonality..standard.deviation..100.", "Mean.Temperature.of.Coldest.Quarter", 
              "mean_value_TotalLeafArea", "mean_value_canopy_biomass", 
              "mean_value_root_biomass", "mean_value_leafarea.wu", 
              "mean_value_canopy_biomass.wu", "mean_value_root_biomass.wu",
              "mean_value_LWP", "mean_value_PD", "WU", "mean_value_gsw", 
              "mean_value_A", "mean_value_E", "mean_value_intrWUE_AE", 
              "mean_value_instWUE_AGs", "mean_MS_V", "mean_MP_V", "mean_M_SP_V", "mean_M_SPA_V",
              "mean_MSV_by_TMV", "mean_MPV_by_TMV", "mean_MSV_by_MPV", "mean_Porosity", "mean_Airspace_V")

# Correlation Different by Treatment --------------------------------------
      cor_control <- cor(control_df[, var_cols], use = "pairwise.complete.obs")
      cor_control_df <- as.data.frame(as.table(cor_control))
      names(cor_control_df) <- c("Variable1", "Variable2", "Correlation")
      
      # Create Control treatment plot
      ggplot(cor_control_df, aes(Variable1, Variable2, fill = Correlation)) + 
        geom_tile() +
        geom_text(aes(label = round(Correlation, 2)), color = "black", size = 4) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(title = "Control treatment correlation plot", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      # Save Plot
      ggsave(paste0("fig_output/Correlations/correlation_C_table", ".png"))
      ggsave(paste0("fig_output/Correlations/correlation_C_table", ".pdf"))


# Calculate correlations for Drought treatment and variables
cor_drought <- cor(drought_df[, var_cols], use = "pairwise.complete.obs")
      
      # Convert correlation matrix to data frame
      cor_drought_df <- as.data.frame(as.table(cor_drought))
      names(cor_drought_df) <- c("Variable1", "Variable2", "Correlation")
      
      ggplot(cor_drought_df, aes(Variable1, Variable2, fill = Correlation)) + 
        geom_tile() +
        geom_text(aes(label = round(Correlation, 2)), color = "black", size = 4) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
        labs(title = "Drought treatment correlation plot", x = "", y = "") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
      
      ggsave(paste0("fig_output/Correlations/correlation_D_table", ".png"))
      ggsave(paste0("fig_output/Correlations/correlation_D_table", ".pdf"))
