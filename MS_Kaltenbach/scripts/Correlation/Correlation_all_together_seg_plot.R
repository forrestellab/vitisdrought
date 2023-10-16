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

                  # Create a list of the variable pairs to plot
                  var_pairs <- list(c("Lat","mean_value_A"), 
                                    c("Annual.Mean.Temperature","mean_value_A"), 
                                    c("Temperature.Seasonality..standard.deviation..100.", "mean_value_A"), 
                                    c("Annual.Precipitation", "mean_value_A"), 
                                    c("Mean.Temperature.of.Coldest.Quarter", "mean_value_A"), 
                                    c("Lat","mean_value_gsw"), 
                                    c("Annual.Mean.Temperature","mean_value_gsw"), 
                                    c("Temperature.Seasonality..standard.deviation..100.", "mean_value_gsw"), 
                                    c("Annual.Precipitation", "mean_value_gsw"), 
                                    c("Mean.Temperature.of.Coldest.Quarter", "mean_value_gsw"),
                                    c("Lat","mean_value_E"), 
                                    c("Annual.Mean.Temperature","mean_value_E"), 
                                    c("Temperature.Seasonality..standard.deviation..100.", "mean_value_E"), 
                                    c("Annual.Precipitation", "mean_value_E"), 
                                    c("Mean.Temperature.of.Coldest.Quarter", "mean_value_E"),
                                    c("Lat","mean_value_intrWUE_AE"), 
                                    c("Annual.Mean.Temperature","mean_value_intrWUE_AE"), 
                                    c("Temperature.Seasonality..standard.deviation..100.", "mean_value_intrWUE_AE"), 
                                    c("Annual.Precipitation", "mean_value_intrWUE_AE"), 
                                    c("Mean.Temperature.of.Coldest.Quarter", "mean_value_intrWUE_AE"), 
                                    c("Lat","mean_value_instWUE_AGs"), 
                                    c("Annual.Mean.Temperature","mean_value_instWUE_AGs"), 
                                    c("Temperature.Seasonality..standard.deviation..100.", "mean_value_instWUE_AGs"), 
                                    c("Annual.Precipitation", "mean_value_instWUE_AGs"), 
                                    c("Mean.Temperature.of.Coldest.Quarter", "mean_value_instWUE_AGs"))
                
                                 
                                                         
                  var_pairs  <- list(c("Lat", "mean_SpongyV_TotalMesophyllV"), 
                                     c("Annual.Mean.Temperature", "mean_SpongyV_TotalMesophyllV" ), 
                                     c("Annual.Precipitation", "mean_SpongyV_TotalMesophyllV"), 
                                     c("Temperature.Seasonality..standard.deviation..100.",
                                       "mean_SpongyV_TotalMesophyllV"), 
                                     c("Mean.Temperature.of.Coldest.Quarter", "mean_SpongyV_TotalMesophyllV"), 
                                       
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

                                  # "mean_MS_V" --> could also be used                                         
colnames(drought_df)
# Create list to hold individual plots
plots_list <- list()

for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  
  # Extract variable names
  var1 <- (pair[1])
  var2 <- str_extract(pair[2], "(?<=mean_).*")
  
  # Create scatterplot with regression line
  p <- ggplot(drought_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Species)) +
    geom_point(shape=16, size = 4) +
    geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, aes(group = 1), color = "black", size = 0.5) +
    scale_color_viridis_d(option = "viridis") +
    labs(x = pair[1],  y = pair[2], color = "species_geno") +
    ylab(label = "") +
    xlab(label = "") +
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
  
  # Add linear regression equation and R-squared value
  model <- lm(get(pair[2]) ~ get(pair[1]), data = control_df)
  rsq <- summary(model)$r.squared
  eq <- paste0("y = ", round(coefficients(model)[[2]], 2), "x + ", round(coefficients(model)[[1]], 2))
  p <- p + 
    annotate("text", x = max(control_df[[pair[1]]]), y = max(control_df[[pair[2]]]), 
             label = paste0("R-squared = ", round(rsq, 3), "\n", eq), hjust = 1, vjust = 1, size = 3)
  

  plots_list[[length(plots_list) + 1]] <- p
}

p_combined <- ggarrange(plotlist = plots_list, ncol = 5, nrow = 4,  common.legend = TRUE, legend = "bottom")
p_combined <- annotate_figure(p_combined,   top = text_grob((paste("Correlation with geoclimate Data of Drought Treatment"
                                       )), color = "black", 
                                       face = "italic", size = 11)) 
print(p_combined)


rm(plots_list)


# Save Plot
#ggsave(paste0("fig_output/Correlations/correlation_C_geoclimate_seg1", ".png"))
#ggsave(paste0("fig_output/Correlations/correlation_C_geoclimate_seg1", ".pdf"))


for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  
  # Extract variable names
  var1 <- (pair[1])
  var2 <- str_extract(pair[2], "(?<=mean_).*")
  
  # Print variable names to check for any issues
  print(paste0("var1: ", var1))
  print(paste0("var2: ", var2)) 
  
 }
  