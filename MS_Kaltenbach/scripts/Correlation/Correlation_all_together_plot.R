library(patchwork)
library(ggpubr)
colnames(combined_df)

                  
                  var_cols <- c("Lat", "Long", "Annual.Mean.Temperature", "Annual.Precipitation", 
                                "Temperature.Seasonality..standard.deviation..100.", "Mean.Temperature.of.Coldest.Quarter", 
                                "mean_value_TotalLeafArea", "mean_value_canopy_biomass", 
                                "mean_value_root_biomass", "mean_value_leafarea.wu", 
                                "mean_value_canopy_biomass.wu", "mean_value_root_biomass.wu",
                                "mean_value_LWP", "mean_value_PD", "mean_value_SWP", 
                                "mean_value_leafwp_predawn", "WU", "mean_value_gsw", 
                                "mean_value_A", "mean_value_E", "mean_value_intrWUE_AE", 
                                "mean_value_instWUE_AGs")
                  
                  
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
                  
                  
# Create list to hold individual plots
plots_list <- list()

for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  
  # Extract variable names
  var1 <- (pair[1])
  var2 <- str_extract(pair[2], "(?<=mean_value_).*")
  
  # Create scatterplot with regression line
  p <- ggplot(control_df, aes(x = .data[[pair[1]]], y = .data[[pair[2]]], color = Species)) +
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
  if (i %in% c(1, 6, 11, 16, 21)) {
    p <- p + theme(axis.line.y.left = element_line(color = "black", size = 0.5),
                   axis.line.y.right = element_blank(), 
                   axis.ticks.y.left = element_line(color = "black"),
                   axis.line.x.bottom = element_line(color = "black"), 
                   axis.text.y.left = element_text(color = "black"))
    p <- p + scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
p <- p + labs(y = var2)
  }
  
  if (i %in% c(21:25)) {
    p <- p + labs(x = var1)
  }
  
  # Add linear regression equation and R-squared value
  model <- lm(get(pair[2]) ~ get(pair[1]), data = control_df)
  rsq <- summary(model)$r.squared
  eq <- paste0("y = ", round(coefficients(model)[[2]], 2), "x + ", round(coefficients(model)[[1]], 2))
  p <- p + 
    annotate("text", x = max(drought_df[[pair[1]]]), y = max(drought_df[[pair[2]]]), 
             label = paste0("R-squared = ", round(rsq, 3), "\n", eq), hjust = 1, vjust = 1, size = 3)
  
  # Add SEM error bars to plot
  sem_var <- paste0("sem_", var2)
  p <- p + geom_errorbar(aes(ymin = .data[[pair[2]]] - .data[[sem_var]], ymax = .data[[pair[2]]] + .data[[sem_var]]), 
                         width = 0.5, size = 0.5)

  plots_list[[length(plots_list) + 1]] <- p
}

p_combined <- ggarrange(plotlist = plots_list, ncol = 5, nrow = 5,  common.legend = TRUE, legend = "bottom")
p_combined <- annotate_figure(p_combined, 
                                       top = text_grob((paste("Correlation with geoclimate Data of Control Treatment"
                                       )), color = "black", 
                                       face = "italic", size = 11)) 
print(p_combined)


rm(plots_list)


# Save Plot
ggsave(paste0("fig_output/Correlations/correlation_C_geoclimate_over_all", ".png"))
ggsave(paste0("fig_output/Correlations/correlation_C_geoclimate_over_all", ".pdf"))


for (i in seq_along(var_pairs)) {
  pair <- var_pairs[[i]]
  
  # Extract variable names
  var1 <- (pair[1])
  var2 <- str_extract(pair[2], "(?<=mean_value_).*")
  
  # Print variable names to check for any issues
  print(paste0("var1: ", var1))
  print(paste0("var2: ", var2)) 
  
 }
  