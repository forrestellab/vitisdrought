library(patchwork)
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

colnames(combined_df)

var_pairs  <- list(c("Lat", "mean_SpongyV_TotalMesophyllV"), 
                   c("Annual.Mean.Temperature", "mean_SpongyV_TotalMesophyllV" ), 
                   c("Annual.Precipitation", "mean_SpongyV_TotalMesophyllV"), 
                   c("Temperature.Seasonality..standard.deviation..100.",
                     "mean_SpongyV_TotalMesophyllV"), 
                   c("Mean.Temperature.of.Coldest.Quarter", "mean_SpongyV_TotalMesophyllV")
, 
                   
                   c("Lat", "mean_PalisadeV_TotalMesophyllV"),
                   c("Annual.Mean.Temperature", "mean_PalisadeV_TotalMesophyllV"),
                   c("Annual.Precipitation", "mean_PalisadeV_TotalMesophyllV"),
                   c("Temperature.Seasonality..standard.deviation..100.",
                     "mean_PalisadeV_TotalMesophyllV"),
                   c("Mean.Temperature.of.Coldest.Quarter", "mean_PalisadeV_TotalMesophyllV"),
                   
                   c("Lat", "mean_SpongyV_PalisadeV"),
                   c("Annual.Mean.Temperature", "mean_SpongyV_PalisadeV"),
                   c("Annual.Precipitation", "mean_SpongyV_PalisadeV"),
                   c("Temperature.Seasonality..standard.deviation..100.","mean_SpongyV_PalisadeV"),
                   c("Mean.Temperature.of.Coldest.Quarter", "mean_SpongyV_PalisadeV"),
                   
                   c("Lat", "mean_Porosity"),
                   c("Annual.Mean.Temperature", "mean_Porosity"),
                   c("Annual.Precipitation", "mean_Porosity"),
                   c("Temperature.Seasonality..standard.deviation..100.", "mean_Porosity"),
                   c("Mean.Temperature.of.Coldest.Quarter", "mean_Porosity"))




# Create a new data frame combining drought and control data
combined_df <- rbind(drought_df, control_df)

# Create a new variable to differentiate between drought and control
combined_df$Treatment <- factor(c(rep("Drought", nrow(drought_df)), rep("Control", nrow(control_df))))

# Create a list to hold individual plots
plots_list <- list()

for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  
  # Extract variable names
  var1 <- (pair[1])
  var2 <- str_extract(pair[2], "(?<=mean_).*")
  
  # Create scatterplot with separate regression lines for each treatment
  p <- ggplot(combined_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Genotype, shape = Treatment)) +
    geom_point(size = 4) +
    geom_smooth(data = subset(combined_df, Treatment == "Drought"), method = "lm", se = FALSE, fullrange = TRUE, aes(group = 1), color = "black", linetype = "dashed", size = 0.5) +
    geom_smooth(data = subset(combined_df, Treatment == "Control"), method = "lm", se = FALSE, fullrange = TRUE, aes(group = 1), color = "black", linetype = "solid", size = 0.5) +
    labs(x = pair[1], y = pair[2], color = "Genotype", shape = "Treatment") +
    theme_minimal()
  
  
  # Modify y-axis theme for subsequent plots
  if (i > 1) {
    p <- p + theme(axis.line.y.left = element_line(color = "black", size = 0.5),
                   axis.ticks.y.left = element_line(color = "black"),
                   axis.line.y.right = element_line(color = "black"),
                   axis.text.y.right = element_blank(),
                   axis.text.y.left = element_blank(),
                   axis.ticks.y.right = element_line(color = "black"), 
                   axis.line.x.bottom = element_line(color = "black"))
  }
  # Add y-axis labels and ticks for first plot
  if (i %in% c(1, 6, 11,16)) {
    p <- p + theme(axis.line.y.left = element_line(color = "black", size = 0.5),
                   axis.line.y.right = element_blank(), 
                   axis.ticks.y.left = element_line(color = "black"),
                   axis.line.x.bottom = element_line(color = "black"), 
                   axis.text.y.left = element_text(color = "black"))
    p <- p + scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
    p <- p + labs(y = var2)
  }
  
  if (i %in% c(16:21)) {
    p <- p + labs(x = var1)
  }
  
  plots_list[[length(plots_list) + 1]] <- p
}

# Combine all the plots
p_combined <- ggarrange(plotlist = plots_list, ncol = 5, nrow = 4, common.legend = TRUE, legend = "bottom")

# Add a title to the combined plot
p_combined <- annotate_figure(p_combined, top = text_grob("Correlation with Geoclimate Data of Drought Treatment", color = "black", face = "italic", size = 11))

# Print the combined plot
print(p_combined)

