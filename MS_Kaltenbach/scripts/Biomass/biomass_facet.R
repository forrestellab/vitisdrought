# Load necessary libraries
library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)
library(ggplot2)
library(ggsignif)
library(patchwork)
library(lemon)
library(ggh4x)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

biomass_anova <- read.csv("data_output/biomass/biomass_anova_1.csv")

biomass_anova<- biomass_anova %>%
  select(-starts_with("mean_"), -starts_with("sd_")) %>%
  select(-c(Treatment_factor, species_geno))%>%
  distinct()

symnum.args <- list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))

columns <- c("sumwu", "canopy_biomass", "root_biomass", "ratio_canopy_root", "TotalLeafArea", "leafarea_wu", "canopy_biomass_wu", "root_biomass_wu")

desired_order <- c("sumwu", "canopy_biomass", "root_biomass", "ratio_canopy_root", "TotalLeafArea", "leafarea_wu", "canopy_biomass_wu", "root_biomass_wu")

# Reshape the data to long format for non-Tukey values
long_data <- biomass_anova %>%
  gather(key = "Variable", value = "Value", all_of(columns)) %>%
  select(-starts_with("Tukey_"), -starts_with("mean_"), -starts_with("sd_")) %>%
 # filter(Species != "vulpina") %>%
  mutate(Variable = factor(Variable, levels = desired_order))%>%
  distinct()

# Reshape the data to long format for Tukey values
tukey_data <- biomass_anova %>%
  select(Treatment, Species, starts_with("Tukey_")) %>%
  gather(key = "Variable", value = "Tukey_Value", -Treatment, -Species) %>%
  mutate(Variable = str_replace(Variable, "^Tukey_", "")) %>%
  distinct()

# Left join long_data and tukey_data
combined_data <- left_join(long_data, tukey_data, by = c("Species", "Treatment", "Variable"))
combined_data <- combined_data %>%
  filter(!is.na(Value))%>%
  distinct()


combined_data$Variable <- factor(combined_data$Variable, levels = desired_order)


# define labels -----------------------------------------------------------
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


# species_labels <- c(
#   "acerifolia" = "V. acerifolia 9018",
#   "aestivalis" = "V. aestivalis T52",
#   "arizonica" = "V. arizonica b40-14",
#   "cinerea" = "V. cinerea b42-34",
#   "mustangensis" = "V. mustangensis T48",
#   "hybrid" = "V. riparia NY1",
#   "riparia" = "hybrid TXNM0821",
#   "rupestris" = "V. rupestris Vru42"
# )
# 

columns <- c("sumwu", "canopy_biomass", "root_biomass", "ratio_canopy_root", "TotalLeafArea", "leafarea_wu", "canopy_biomass_wu", "root_biomass_wu")

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


summary_data <- combined_data %>%
  group_by(Species, Treatment, Variable) %>%
  summarise(Count = n())

# Print the summary data
print(summary_data)

#(n=3-5 excpt. for vulpina n=1)



# tryout ------------------------------------------------------------------
