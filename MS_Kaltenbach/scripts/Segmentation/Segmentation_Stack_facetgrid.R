

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
library(lemon)
library(ggh4x)
# Modify data -------------------------------------------------------------

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

combined_seg_data<- read.csv("data_output/segmentation/combined_seg_data.csv")
combined_seg_data<- read.csv("data_output/segmentation/species_seg_anova_data.csv")


str(combined_seg_data)

symnum.args = list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))

# Columns to plot
columns <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

# Reshape the data to long format for non-Tukey values
long_data <- combined_seg_data %>%
  gather(key = "Variable", value = "Value", all_of(columns)) %>%
  select(-starts_with("Tukey_")) %>%
  select(-c(species_geno, Treatment_factor))

desired_order <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

long_data$Variable <- factor(long_data$Variable, levels = desired_order)

# Filter columns that start with "Tukey_"
tukey_columns <- grep("^Tukey_", colnames(combined_seg_data), value = TRUE)

# Reshape the data to long format for Tukey values
tukey_data <- combined_seg_data %>%
  select(Treatment, Species, starts_with("Tukey_")) %>%
  gather(key = "Variable", value = "Tukey_Value", -Treatment, -Species) %>%
  mutate(Variable = str_replace(Variable, "^Tukey_", "")) %>%
  distinct()

# Left join long_data and tukey_data
combined_data <- left_join(long_data, tukey_data, by = c("Species", "Treatment", "Variable"))

desired_order <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

combined_data$Variable <- factor(combined_data$Variable, levels = desired_order)

print(combined_data)


# new ---------------------------------------------------------------------

# Columns to plot
columns <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

columns <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")


colnames(combined_seg_data)

# Reshape the data to long format
long_data <- combined_seg_data %>%
  gather(key = "Variable", value = "Value", all_of(columns))

desired_order <- c("Porosity", "SpongyV_TotalMesophyllV", "PalisadeV_TotalMesophyllV", "SpongyV_PalisadeV", "Leaf_Width_um", "Mesophyll_Width_um")

# Convert the Variable column to a factor with the desired order
long_data$Variable <- factor(long_data$Variable, levels = desired_order)

# Define labels for Species/Genotype combinations
species_labels <- c(
  "acerifolia" = "V. acerifolia 9018",
  "aestivalis" = "V. aestivalis T52",
  "arizonica" = "V. arizonica b40-14",
  "cinerea" = "V. cinerea b42-34",
  "mustangensis" = "V. mustangensis T48",
  "hybrid" = "V. riparia NY1",
  "riparia" = "hybrid TXNM0821",
  "rupestris" = "V. rupestris Vru42",
  "vulpina" = "V. vulpina V60-96"
)

variable_labels <- c(
  `Porosity` = "theta[IAS]~~~(m^3~m^-3)",
  `SpongyV_TotalMesophyllV` = "V[sp-cell]/~V[mes-cell]~~~(m^3~m^-3)",
  `PalisadeV_TotalMesophyllV` = "V[pa-cell]/~V[mes-cell]~~~(m^3~m^-3)",
  `SpongyV_PalisadeV` = "V[sp-cell]/~V[pa-cell]~~~(m^3~m^-3)",
  `Leaf_Width_um` = "L[leaf]~~~(µm)",
  `Mesophyll_Width_um` = "L[mes]~~~(µm)"
)


# Label on top ---------------------------------------------------------------------

final_plot <- ggplot(combined_data, aes(x = Species, y = Value, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 7),
  ) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  stat_compare_means(method = "t.test", label = "p.signif", hide.ns = TRUE, size = 5, symnum.args = symnum.args, label.y.npc = "top", bracket.size = 0.3) +
geom_text(data = combined_data %>% 
              group_by(Species, Treatment, Genotype, Variable) %>% 
              summarize(y = max(Value), label = first(Tukey_Value)),
          aes(x = Species, y = y, label = label),
          position = position_dodge(width = 0.75), size = 3, vjust = -0.6, hjust = 0.5, colour = "gray25") +
  facet_wrap(
    ~Variable, scales = "free_y", ncol = 2,
    labeller = labeller(Variable = as_labeller(variable_labels, label_parsed)) # Use label_parsed to interpret expressions
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(), panel.grid.major.x = element_line(color = "gray", size= 0.2),
    strip.text = element_text(size = 8, face = "bold", color = "black", hjust = 0)
  ) +
  scale_x_discrete(labels = species_labels, breaks = unique(combined_data$Species))
  
final_plot

# Label on side -----------------------------------------------------------

final_plot <- ggplot(combined_data, aes(x = Species, y = Value, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 7),
  ) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  stat_compare_means(
    method = "t.test", label = "p.signif", hide.ns = TRUE,
    size = 5, symnum.args = symnum.args, label.y.npc = "top", bracket.size = 0.3
  ) +
  geom_text(data = combined_data %>% 
              group_by(Species, Treatment, Genotype, Variable) %>% 
              summarize(y = max(Value), label = first(Tukey_Value), .groups = "drop"),
            aes(x = Species, y = y, label = label),
            position = position_dodge(width = 0.75), size = 3, vjust = -0.6, hjust = 0.5, colour = "gray25"
  ) +
  facet_wrap(
    ~Variable, scales = "free_y", ncol = 2, strip.position = "left",
    labeller = labeller(Variable = as_labeller(variable_labels, label_parsed)) # Use label_parsed to interpret expressions
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(), panel.grid.major.x = element_line(color = "gray", size= 0.2),
    strip.text = element_text(size = 8, hjust = 0.5, face = "bold", color = "black"),
    axis.text.y = element_text(size = 7), strip.placement = "outside"
  )+
  scale_x_discrete(labels = species_labels, breaks = unique(combined_data$Species))

final_plot 



# label  ------------------------------------------------------------------

facet_labels <- data.frame(
  Variable = unique(combined_data$Variable),
  label = paste0("(", LETTERS[1:length(unique(combined_data$Variable))], ")")
)

final_plot <- ggplot(combined_data, aes(x = Species, y = Value, fill = Treatment)) +
  geom_boxplot() +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    axis.text.x = element_text(angle = 25, hjust = 1, size = 10),
  ) +
  xlab("") +
  ylab("") +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  stat_compare_means(
    method = "t.test", label = "p.signif", hide.ns = TRUE,
    size = 5, symnum.args = symnum.args, label.y.npc = "top") +
  geom_text(data = combined_data %>% 
              group_by(Species, Treatment, Genotype, Variable) %>% 
              summarize(y = max(Value), label = first(Tukey_Value), .groups = "drop"),
            aes(x = Species, y = y, label = label),
            position = position_dodge(width = 0.75), size = 3.5, vjust = -0.6, hjust = 0.5, colour = "gray25") +
  facet_wrap(
    ~Variable, scales = "free_y", ncol = 2, strip.position = "left",
    labeller = labeller(Variable = as_labeller(variable_labels, label_parsed))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + 
  geom_text(data = facet_labels, aes(x = -Inf, y = Inf, label = label), 
            size = 3.5,   hjust = -0.0011, vjust = 1.5, color = "black",fontface = "bold", inherit.aes = FALSE) +  
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    strip.background = element_blank(), panel.grid.major.x = element_line(color = "gray", size= 0.2),
    strip.text = element_text(size = 10, hjust = 0.5, face = "bold", color = "black"),
    axis.text.y = element_text(size = 11), strip.placement = "outside")+
  scale_x_discrete(labels = species_labels, breaks = unique(combined_data$Species))

final_plot
