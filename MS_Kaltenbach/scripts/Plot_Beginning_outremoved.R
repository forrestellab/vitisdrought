
# Beginning Day
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

combined_beg_data<- read.csv("data_output/anova/species_beginning_anova_data_outremoved")

symnum.args = list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))

# Plot individual --------------------------------------------------------------------
A_box_plot <- ggplot(combined_beg_data, aes(x = paste(Species, Genotype), y = A, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    strip.background = element_rect(fill = "white")
  ) +
  xlab("") +
  ylab("A") +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 6, symnum.args = symnum.args) +
  geom_text(data = combined_beg_data %>% 
              group_by(Species, Treatment, Genotype, Tukey_A) %>% 
              summarise(mean_A = max(A)),
            aes(x = paste(Species, Genotype), y = mean_A, label = Tukey_A),
            position = position_dodge(width = 0.75), size = 3, vjust = -0.5, hjust = 0.5, colour = "gray25")

A_box_plot


# new ---------------------------------------------------------------------

str(combined_beg_data)
# bebbe -------------------------------------------------------------------
plots <- list()

# Columns to plot
columns <- c("A", "Ci", "E", "gsw", "E", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP" )

# Custom y-axis labels
labels <- c("A", "Ci", "E", "gsw", "E", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP" )

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
  plot <- ggplot(combined_beg_data, aes(x = paste(Species, Genotype), y = .data[[col]], fill = Treatment)) +
    geom_boxplot() +
    theme_plot +
    xlab("") +
    ylab(label) +  # Use the custom label
    scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
    stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 6, symnum.args = symnum.args) +
    geom_text(data = combined_beg_data %>% 
                group_by(Species, Treatment, Genotype) %>% 
                summarize(y = max(.data[[col]]), label = first(.data[[paste0("Tukey_", col)]])),
              aes(x = paste(Species, Genotype), y = y, label = label),
              position = position_dodge(width = 0.75), size = 4, vjust = -0.3, hjust = 0.5, colour = "gray25")
  
  plot <- plot + theme(plot.margin = margin(0.00005, 0.05, 0.05, 0.05, "cm"))
  
  plots[[col]] <- plot
}

final_plot <- wrap_plots(plots, nrow = length(plots)) +
  plot_layout(widths = unit(10, "cm"), heights = unit(11, "cm"), guides = 'collect')

final_plot_outremoved <- final_plot + theme(plot.margin = margin(0, 0.0, 0.0, 0.0, "cm"))

ggsave("beginning_plot_labels_outremoved.pdf", final_plot_outremoved, width = 20, height = 103, units = "cm")



# new_filtered ------------------------------------------------------------

plots <- list()

# Columns to plot
columns <- c("A", "Ci", "E", "gsw", "E", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP")

labels <- c(expression(A[n]*" (µmol CO"[2]*" m"^{-2}* " s"^{-1}*")"), expression(C[i] * " (µmol" * " m"^{-1}*")"), expression(E*" (mmol"*" m"^{-2}* " s"^{-1}*")"), expression(g[s]*" (mol"*" m"^{-2}* " s"^{-1}*")"), "Leaf Temperature (°C)", expression("WUE"*" (A/E)"*" (mmol"* " mol"^{-1}*")"), expression(iWUE*" (A/g"[s]*")"*" (µmol"* " mol"^{-1}*")"), expression(Psi[leaf]*" (MPa)"))


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
  
  # Filter rows with non-NA values
  filtered_data <- combined_beg_data[complete.cases(combined_beg_data[, c("Species", "Genotype", col, "Treatment")]), ]
  
  # Create the plot
  plot <- ggplot(filtered_data, aes(x = paste(Species, Genotype), y = .data[[col]], fill = Treatment)) +
    geom_boxplot() +
    theme_plot +
    xlab("") +
    ylab(label) +  # Use the custom label
    scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
    stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 6, symnum.args = symnum.args) +
    geom_text(data = filtered_data %>%
                group_by(Species, Treatment, Genotype) %>%
                summarize(y = max(.data[[col]]), label = first(.data[[paste0("Tukey_", col)]])),
              aes(x = paste(Species, Genotype), y = y, label = label),
              position = position_dodge(width = 0.75), size = 4, vjust = -0.3, hjust = 0.5, colour = "gray25")
  
  plot <- plot + theme(plot.margin = margin(0.00005, 0.05, 0.05, 0.05, "cm"))
  
  plots[[col]] <- plot
}

final_plot <- wrap_plots(plots, nrow = length(plots)) +
  plot_layout(widths = unit(10, "cm"), heights = unit(11, "cm"), guides = 'collect')

final_plot_outremoved <- final_plot + theme(plot.margin = margin(0, 0.0, 0.0, 0.0, "cm"))

ggsave("beginning_plot_labels2_outremoved.pdf", final_plot_outremoved, width = 20, height = 103, units = "cm")


