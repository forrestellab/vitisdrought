# Load Required Packages
library(tidyverse)
library(readxl)
library(ggpubr)
library(hrbrthemes)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data
cumWU <- read.csv("data/Subset/cumWu.csv")
harvest <- read.csv("data/Subset/Harvest.csv")
leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

# Subset and Combine Data Sets
harvest <- harvest %>%
  select(ID.Order, ID, Date, Treatment, Genotype, canopy_biomass, root_biomass, species_geno)

# Calculate Biomass Ratio
biomass_ratio <- harvest %>%
  mutate(ratio_canopy_root = canopy_biomass / root_biomass) %>%
  filter(!is.na(ratio_canopy_root)) %>%
  group_by(species_geno, Treatment) %>%
  filter(ratio_canopy_root >= quantile(ratio_canopy_root, 0.05) &
           ratio_canopy_root <= quantile(ratio_canopy_root, 0.95)) %>%
  ungroup()

#write.csv(biomass_ratio, file= "data/Subset/sub_small_biomass_ratio.csv")

# Plot Biomass Ratio
biomass_ratio_plot <- biomass_ratio %>%
  ggplot(aes(x = species_geno, y = ratio_canopy_root, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Ratio of Canopy to Root Biomass") +
  theme_pubclean() +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom")+
  xlab(label = "")+
  ylab(label = "Ratio of Canopy to Root Biomass")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Show Plot
biomass_ratio_plot

# Save Plot
 ggsave(paste0("fig_output/Biomass/Biomass_Ratio/Biomass_Root_Canopy_Ratio", ".png"))
 ggsave(paste0("fig_output/Biomass/Biomass_Ratio/Biomass_Root_Canopy_Ratio", ".pdf"))


# Plot with Significance --------------------------------------------------

# Create a new column indicating whether a category is the last one or not
biomass_ratio <- biomass_ratio %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()

# Perform t-test for each MassType and each species_geno
p_value <- biomass_ratio %>%
  group_by(species_geno) %>%
  filter(!((species_geno == "V. vulpina_V60-96"))) %>%
  summarise(p.value = if(n() >= 2) t.test(ratio_canopy_root ~ Treatment)$p.value else NA) %>%
  left_join(biomass_ratio %>% distinct(species_geno, Treatment), by = c( "species_geno"))

# Get the maximum value for each MassType
max_val <- biomass_ratio%>% 
  summarise(max_val = max(ratio_canopy_root))

# Join max_pval with wixon by MassType
p_value_max_val <- left_join(p_value, max_val)

alpha_str <- "\u03B1"

# Plot
biomass_ratio_all_sig <- biomass_ratio %>%
  ggplot(aes(x = species_geno, y = ratio_canopy_root, fill = Treatment)) +
  geom_boxplot() +
  labs(x = "", y = "",
       y = "Ratio of Canopy to Root Biomass") +
  theme_classic() +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle(paste("Biomass and Total Leaf area across all Vitis species over time [", alpha_str, "= 0.05]")) +
  geom_text(data = p_value %>% filter(p.value < 0.05),
            aes(label = "*", x = species_geno, y = 10.1),
            fontface = "bold") +
  geom_text(data = p_value %>% filter(p.value >= 0.05),
            aes(label = "ns", x = species_geno, y = 10.1),
            fontface = "bold") +
  geom_vline(data = ~subset(., !is_last),
             aes(xintercept = as.numeric(factor(species_geno)) + 0.5), 
             linetype = "dashed", color = "grey") 

# Print the plot
print(biomass_ratio_all_sig)

# Save Plot
   ggsave(paste0("fig_output/Biomass/Biomass_Ratio/biomass_ratio_all_sig", ".png"))
  ggsave(paste0("fig_output/Biomass/Biomass_Ratio/biomass_ratio_all_sig", ".pdf"))



# SIGNIFICANCE TABLE ------------------------------------------------------

combined_ratio_df <- biomass_ratio %>%
      left_join(p_value, by = c( "species_geno", "Treatment")) %>%
      select(ID, Genotype, Treatment, species_geno, p.value, ratio_canopy_root)

biomass_ratio_table <- combined_ratio_df %>%
      group_by(species_geno, Treatment) %>%
      summarise(mean_value = mean(ratio_canopy_root),
                sd_value = sd(ratio_canopy_root),
                p.value = mean(p.value)) %>%
      ungroup()

# Save the data as a CSV file in the specified directory
write.csv(biomass_ratio_table_gt, file = "data_output/biomass/biomass_ratio_table_gt", row.names = FALSE)

print(biomass_ratio_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Biomass Ration by Species")  %>% 
  gtsave(filename = "biomass_ratio_table.html", path = "fig_output/Biomass/")

