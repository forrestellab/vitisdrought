
# Middle Day
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

combined_mid_data<- read.csv("data_output/anova/anova_middle")

combined_mid_data<- combined_mid_data %>%
  select(-starts_with("mean_"), -starts_with("sd_")) %>%
  mutate(LWP = -LWP)    %>%
  distinct()

symnum.args = list(cutpoints = c(0, 0.05, 1), symbols = c("*", "ns"))

# Columns to plot

columns <- c("A", "Ci", "E", "gsw", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP" )

# Reshape the data to long format for non-Tukey values
long_data <- combined_mid_data %>%
  gather(key = "Variable", value = "Value", all_of(columns)) %>%
  select(-starts_with("Tukey_"))%>%
  distinct()

desired_order <- c("A", "Ci", "E", "gsw", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP")

long_data$Variable <- factor(long_data$Variable, levels = desired_order)

# Filter columns that start with "Tukey_"
tukey_columns <- grep("^Tukey_", colnames(combined_mid_data), value = TRUE)

# Reshape the data to long format for Tukey values
tukey_data <- combined_mid_data %>%
  select(Treatment, Species, starts_with("Tukey_")) %>%
  gather(key = "Variable", value = "Tukey_Value", -Treatment, -Species) %>%
  mutate(Variable = str_replace(Variable, "^Tukey_", "")) %>%
  distinct()

# Left join long_data and tukey_data
combined_data <- left_join(long_data, tukey_data, by = c("Species", "Treatment", "Variable"))


combined_data$Variable <- factor(combined_data$Variable, levels = desired_order)
combined_data <- combined_data %>%
  filter(!is.na(Value)) %>%
  distinct()


print(combined_data)


# new ---------------------------------------------------------------------

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

# Columns to plot
columns <- c("A", "Ci", "E", "gsw", "E", "Tleaf", "intrWUE_AE", "instWUE_AGs", "LWP")

variable_labels <- c(
  `A` = "A[n]~~~(µmol~CO[2]~m^-2~s^-1)",
  `Ci` = "C[i]~~~(µmol~mol^-1)",
  `E` = "E~~~(mol~m^-2~s^-1)",
  `gsw` = "g[s]~~~(mol~m^-2~s^-1)",
  `Tleaf` = "T[leaf]~~~(~C)",
  `intrWUE_AE` = "WUE[inst]~~(µmol~CO[2]~mol^{-2}~H[2]~O)", 
  `instWUE_AGs` = "WUE[i]~~(µmol~CO[2]~mol^{-2}~H[2]~O)", 
  `LWP` = "Psi[MD]~~~(MPa)"
)


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
  distinct()%>%
  summarise(Count = n())

# Print the summary data
print(summary_data)



summary_data <- combined_data %>%
  group_by(Species, Treatment, Variable) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  filter(Count <= 3)

# Print the summary data for groups with a count of 1
print(summary_data)

#all n=3 for Ci of arizonica drought, everything else bigger for LWP n=2-3
