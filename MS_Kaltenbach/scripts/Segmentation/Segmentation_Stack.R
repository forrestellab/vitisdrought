

# libraries ---------------------------------------------------------------

# Load the dplyr package
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(ggplot2)
remove.packages("multcomp")

library(ggplot2)
library(ggsignif)
library(ggpubr)
library(patchwork)

# Modify data -------------------------------------------------------------

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

combined_seg_data<- read.csv("data_output/segmentation/combined_seg_data.csv")
combined_seg_data<- read.csv("data_output/segmentation/species_seg_anova_data.csv")


str(combined_seg_data)

# Plot individual --------------------------------------------------------------------
porosity_box_plot <- ggplot(combined_seg_data, aes(x = paste(Species, Genotype), y = Porosity, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")
  ) +
  xlab("") +
  ylab("Porosity") +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 6) +
  geom_text(data = combined_seg_data %>% 
              group_by(Species, Treatment, Genotype, Tukey_Porosity) %>% 
              summarise(mean_Porosity = max(Porosity)),
            aes(x = paste(Species, Genotype), y = mean_Porosity, label = Tukey_Porosity),
            position = position_dodge(width = 0.75), size = 3, vjust = -0.5, hjust = 0.5, colour = "gray25")

porosity_box_plot



# Plot in loop ---------------------------------------------------------------------


plots <- list()

plot_size <- 10

# Columns to plot
columns <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")


for (i in seq_along(columns)) {
  col <- columns[i]
  
  # Set theme for the plot
  theme_plot <- theme_bw() +
    theme(
      strip.background = element_rect(fill = "white")
    )
  
  # Set axis.text.x theme element
  if (i == length(columns)) {
    theme_plot <- theme_plot + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  } else {
    theme_plot <- theme_plot + theme(axis.text.x = element_blank())
  }
  
  # Create the plot
  plot <- ggplot(combined_seg_data, aes(x = paste(Species, Genotype), y = .data[[col]], fill = Treatment)) +
    geom_boxplot() +
    theme_plot +
    xlab("") +
    ylab(col) +
    scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
    stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 7) +
    geom_text(data = combined_seg_data %>% 
                group_by(Species, Treatment, Genotype) %>% 
                summarize(y = max(.data[[col]]), label = first(.data[[paste0("Tukey_", col)]])),
              aes(x = paste(Species, Genotype), y = y, label = label),
              position = position_dodge(width = 0.75), size = 4, vjust = -0.3, hjust = 0.5, colour = "gray25")
  
  plot <- plot + theme(plot.margin = margin(0.00005, 0.05, 0.05, 0.05, "cm"))
  
  plots[[col]] <- plot
}

final_plot <- wrap_plots(plots, nrow = length(plots)) +
  plot_layout(heights = rep(unit(plot_size, "cm"), length(plots)), guides = 'collect')

# Display the final plot


final_plot <- wrap_plots(plots, nrow = length(plots)) +
  plot_layout(widths = unit(10, "cm"), heights = unit(11, "cm"), guides = 'collect')

final_plot <- final_plot + theme(plot.margin = margin(0, 0.0, 0.0, 0.0, "cm"))

ggsave("final_plot.pdf", final_plot, width = 20, height = 73, units = "cm")




# new ---------------------------------------------------------------------

# bebbe -------------------------------------------------------------------
plots <- list()

# Columns to plot
columns <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

# Custom y-axis labels
labels <- c("Porosity", "Spongy Mesophyll Volume/ Total Mesophyll Volume", "Palisade Volume/ Total Mesophyll Volume",
            "Spongy Mesophyll Volume/ Palisade Mesophyll Volume", "Leaf Width (µm)", "Mesophyll Width (µm)")

for (i in seq_along(columns)) {
  col <- columns[i]
  label <- labels[i]  # Get the corresponding label
  
  # Set theme for the plot
  theme_plot <- theme_bw() +
    theme(
      strip.background = element_rect(fill = "white")
    )
  
  # Set axis.text.x theme element
  if (i == length(columns)) {
    theme_plot <- theme_plot + theme(axis.text.x = element_text(angle = 30, hjust = 1))
  } else {
    theme_plot <- theme_plot + theme(axis.text.x = element_blank())
  }
  
  # Create the plot
  plot <- ggplot(combined_seg_data, aes(x = paste(Species, Genotype), y = .data[[col]], fill = Treatment)) +
    geom_boxplot() +
    theme_plot +
    xlab("") +
    ylab(label) +  # Use the custom label
    scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
    stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 7) +
    geom_text(data = combined_seg_data %>% 
                group_by(Species, Treatment, Genotype) %>% 
                summarize(y = max(.data[[col]]), label = first(.data[[paste0("Tukey_", col)]])),
              aes(x = paste(Species, Genotype), y = y, label = label),
              position = position_dodge(width = 0.75), size = 4, vjust = -0.3, hjust = 0.5, colour = "gray25")
  
  plot <- plot + theme(plot.margin = margin(0.00005, 0.05, 0.05, 0.05, "cm"))
  
  plots[[col]] <- plot
}

final_plot <- wrap_plots(plots, nrow = length(plots)) +
  plot_layout(widths = unit(10, "cm"), heights = unit(11, "cm"), guides = 'collect')

final_plot <- final_plot + theme(plot.margin = margin(0, 0.0, 0.0, 0.0, "cm"))

ggsave("segmentation_plot_labels.pdf", final_plot, width = 20, height = 73, units = "cm")


