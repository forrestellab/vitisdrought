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

combined_data<- read.csv("data_output/WP/WP_combined.csv")


combined_data<- combined_data %>%
  select(-c(species_geno, Month, Day, Date, X, SWP.area, PD.area, LWP.area))%>%
  distinct()

symnum.args <- list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))

columns <- c( "LWP", "SWP", "PD", "leafwp_predawn")


desired_order <- c("LWP", "SWP", "PD", "leafwp_predawn")

combined_data <- combined_data%>%
  gather(key = "Variable", value = "Value", all_of(columns)) %>%
  mutate(Variable = factor(Variable, levels = desired_order))%>%
  filter(!is.na(Value))%>%
  distinct()

combined_data$Variable <- factor(combined_data$Variable, levels = desired_order)

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
  `sumwu` = " W[tot]~~~(L)",
  `canopy_biomass` = "m[canopy]~~~(g)",
  `root_biomass` = "m[root]~~~(g)",
  `ratio_canopy_root` = "m[canopy]/~m[root]~~~(g~g^-1)",
  `TotalLeafArea` = "A[leaf]~~~(cm^2)",
  `leafarea_wu` = "A[leaf]/~W[tot]~~~(cm^2~L^-1)", 
  `canopy_biomass_wu` = "m[canopy]/~W[tot]~~~(g~L^-1)",
  `root_biomass_wu` = "m[root]/~W[tot]~~~(g~L^-1)"
)


# label  ------------------------------------------------------------------

facet_labels <- data.frame(
  Variable = unique(combined_data$Variable),
  label = paste0("(", LETTERS[1:length(unique(combined_data$Variable))], ")")
)

final_plot <- ggplot(combined_data, aes(x = Species, y = Value, fill = Treatment)) +
  geom_boxplot(na.rm = FALSE) +
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
    size = 5, symnum.args = symnum.args, label.y.npc = "top", na.rm = TRUE) +
  facet_grid(
    Date_group~Variable, scales = "free_y",
   ) +
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


summary_data <- combined_data %>%
  group_by(Species, Treatment, Variable) %>%
  summarise(Count = n())

# Print the summary data
print(summary_data)


