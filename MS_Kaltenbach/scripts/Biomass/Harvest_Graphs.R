
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(kableExtra)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

##Harvest Graphs####

harvest<-read_csv("data/2020_GHDD_Harvest_Data.csv")

# Import data
cumWU <- read.csv("data/Subset/cumWu.csv")
harvest <- read.csv("data/Subset/Harvest.csv")
leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

class(harvest)

# Subset and merge data sets
harvest <- dplyr::select(harvest, ID, Treatment, Genotype, canopy_biomass, root_biomass)
cumWU <- dplyr::select(cumWU, -X)
leaf <- dplyr::select(leaf, -X)

biomass_df <- merge(cumWU, harvest, by = c("Genotype", "Treatment", "ID"), all = TRUE) 
biomass_all <- merge(biomass_df, leaf, by = c("ID", "Genotype", "Treatment", "Species"), all = TRUE)
biomass_all$Species[biomass_all$Genotype == "TXNM0821"] <- "hybrid"


# Reshape data for plotting
biomass_long <- gather(biomass_all, key = "MassType", value = "value", 
                       "root_biomass", "canopy_biomass", "TotalLeafArea") %>%
             drop_na(value)

biomass_long<- distinct(biomass_long)
# Remove outliers
# biomass_long <- biomass_long %>%
  # group_by(MassType) %>%
  # filter(between(value, quantile(value, 0.05), quantile(value, 0.95))) %>%
  # ungroup()

# Create a new column indicating whether a category is the last one or not
biomass_long <- biomass_long %>% 
  group_by(MassType) %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()

# Plot
biomass_plot_all <- biomass_long %>%
  ggplot(aes(x =paste(Species, Genotype), y = value, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(facets = MassType ~ ., scales = "free_y", 
             labeller = labeller(MassType = c(root_biomass = "Root Biomass (g)", 
                                              canopy_biomass = "Canopy Biomass (g)", 
                                              TotalLeafArea = "Total leaf area (cm2)"))) +
  labs(x = "", y = "") +
  theme_classic() +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_rect(fill = "gray")) +
  geom_vline(data = ~subset(., !is_last),
             aes(xintercept = as.numeric(factor(species_geno)) + 0.5), 
             linetype = "dashed", color = "grey") + 
  geom_hline(yintercept = Inf, linetype = "dashed", color = "grey")+
  ggtitle(paste("Biomass and Total Leaf area across all Vitis species over time")) 

print(biomass_plot_all)

# Save plot
  # ggsave("fig_output/Biomass/Biomass_Harvest/Biomass_Harvest.png", biomass_plot_all)
  # ggsave("fig_output/Biomass/Biomass_Harvest/Biomass_Harvest.pdf", biomass_plot_all)

# Plot with Significance --------------------------------------------------

# Create a new column indicating whether a category is the last one or not
biomass_long <- biomass_long %>% 
  group_by(MassType) %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()

# Perform t-test for each MassType and each species_geno
p_value <- biomass_long %>%
  filter(!((MassType == "TotalLeafArea" & species_geno == "cinerea_b42-34") | 
             (MassType == "root_biomass" & species_geno == "vulpina_V60-96"))) %>%
  group_by(MassType, Species, Genotype) %>%
  summarise(p.value = if(n() >= 2) t.test(value ~ Treatment)$p.value else NA) %>%
  left_join(biomass_long %>% distinct(MassType, Species, Genotype, Treatment), by = c("MassType", "Species", "Genotype"))




# Get the maximum value for each MassType
max_val <- biomass_long %>% 
  group_by(MassType) %>% 
  summarise(max_val = max(value))

# Join max_pval with wixon by MassType
p_value_max_val <- left_join(p_value, max_val, by = "MassType")

alpha_str <- "\u03B1"

# Plot
biomass_plot_all_sig <- biomass_long %>%
  ggplot(aes(x = paste(Species, Genotype), y = value, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(facets = MassType ~ ., scales = "free_y", 
             labeller = labeller(MassType = c(root_biomass = "Root Biomass (g)", 
                                              canopy_biomass = "Canopy Biomass (g)", 
                                              TotalLeafArea = "Total leaf area (cm2)"))) +
  labs(x = "", y = "",
       y = "Biomass (g) or Total leaf area (cm2)") +
  theme_classic() +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_rect(fill = "gray")) +
  ggtitle(paste("Biomass and Total Leaf area across all Vitis species over time [", alpha_str, "= 0.05]")) +
  geom_text(data = p_value_max_val %>% filter(p.value < 0.05),
            aes(label = "*", x = species_geno, y = 0.9 * max_val),
            fontface = "bold") +
  geom_text(data = p_value_max_val %>% filter(p.value >= 0.05),
            aes(label = "ns", x = paste(Species, Genotype), y = 0.9 * max_val),
            fontface = "bold") +
  geom_vline(data = ~subset(., !is_last),
             aes(xintercept = as.numeric(factor(species_geno)) + 0.5), 
             linetype = "dashed", color = "grey") + 
  geom_hline(yintercept = Inf, linetype = "dashed", color = "grey")

# Print the plot
print(biomass_plot_all_sig)

# Save plot
 #ggsave("fig_output/Biomass/Biomass_Harvest/Biomass_Harvest_sig.png", biomass_plot_all_sig)
 #ggsave("fig_output/Biomass/Biomass_Harvest/Biomass_Harvest_sig.pdf", biomass_plot_all_sig)

combined_df <- biomass_long %>%
  left_join(p_value, by = c("MassType", "species_geno", "Treatment")) %>%
  select(ID, Genotype, Treatment, Species, species_geno, sumwu, MassType, value, p.value)

biomass_table <- combined_df %>%
  filter(MassType %in% c("TotalLeafArea", "root_biomass", "canopy_biomass")) %>%
  group_by(species_geno, Treatment, MassType) %>%
  summarise(mean_value = mean(value),
            sd_value = sd(value),
            p.value = mean(p.value)) %>%
  pivot_wider(names_from = MassType, values_from = c(mean_value, sd_value, p.value)) %>%
  ungroup()

biomass_table <- biomass_table %>%
  select(species_geno, Treatment, 
         mean_value_TotalLeafArea, sd_value_TotalLeafArea, p.value_TotalLeafArea, 
         mean_value_canopy_biomass, sd_value_canopy_biomass, p.value_canopy_biomass, 
         mean_value_root_biomass, sd_value_root_biomass, p.value_root_biomass)


# Save the data as a CSV file in the specified directory
write.csv(biomass_table, file = "data_output/biomass/biomass_table.csv", row.names = FALSE)


print(biomass_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Biomass by Species")  %>% 
  gtsave(filename = "biomass_table.html", path = "fig_output/Biomass/Biomass_Harvest/")



