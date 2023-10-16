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


drop_cols <- grepl("sd|p\\.value|sem|p\\_value|SEM", colnames(combined_df))
combined_df <- combined_df[, !drop_cols]


colnames(combined_df)

var_cols <- c("Lat", "Long", "Annual.Mean.Temperature", "Annual.Precipitation", 
              "Temperature.Seasonality..standard.deviation..100.",
              "Mean.Temperature.of.Coldest.Quarter", "mean_value_TotalLeafArea", 
              "mean_value_canopy_biomass", "mean_value_root_biomass", 
              "mean_Porosity", "mean_SpongyV_TotalMesophyllV", "mean_PalisadeV_TotalMesophyllV", 
              "mean_SpongyV_PalisadeV","mean_value_gsw", "mean_value_A","mean_value_E", "mean_value_intrWUE_AE",
              "mean_value_instWUE_AGs")

var_cols <- c("Lat", "Long", "Annual.Mean.Temperature", "Annual.Precipitation", 
              "Temperature.Seasonality..standard.deviation..100.",
              "Mean.Temperature.of.Coldest.Quarter","mean_value_gsw", "mean_value_A","mean_value_E", "mean_value_intrWUE_AE",
              "mean_value_instWUE_AGs")


control_df <- subset(combined_df, Treatment == "Control")
drought_df <- subset(combined_df, Treatment == "Drought")


# Calculate correlations for Control treatment and variables
cor_control <- cor(control_df[, var_cols], use = "pairwise.complete.obs")

# Convert correlation matrix to data frame
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


