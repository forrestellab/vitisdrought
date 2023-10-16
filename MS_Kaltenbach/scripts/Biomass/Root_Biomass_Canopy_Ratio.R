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

leaf<- leaf %>%
  select(Genotype, Species)


# Subset and Combine Data Sets
harvest <- harvest %>%
  select(ID.Order, ID, Date, Treatment, Genotype, canopy_biomass, root_biomass, species_geno)

harvest2 <- left_join(harvest, leaf, by = "Genotype")

harvest2$Species[harvest2$Genotype == "TXNM0821"] <- "hybrid"
# Calculate Biomass Ratio
biomass_ratio <- harvest2 %>%
  mutate(ratio_canopy_root = root_biomass/ canopy_biomass) %>%
  filter(!is.na(ratio_canopy_root)) %>%
  group_by(Species, Genotype, Treatment) %>%
  # filter(ratio_canopy_root >= quantile(ratio_canopy_root, 0.05) &
  #          ratio_canopy_root <= quantile(ratio_canopy_root, 0.95)) %>%
  ungroup()

#write.csv(biomass_ratio, file= "data/Subset/sub_small_biomass_ratio.csv")

biomass_ratio_summary <- biomass_ratio %>%
  group_by(Genotype, Treatment, Species) %>%
  summarise(`mean_ratio_canopy_root` = mean(ratio_canopy_root), `sem_ratio_canopy_root` = sd(`ratio_canopy_root`) / sqrt(n())) %>%
  ungroup()

combined_data_ratio <- left_join(biomass_ratio, biomass_ratio_summary, by = c("Genotype", "Treatment", "Species"))

# Plot Biomass Ratio
biomass_ratio_plot <- combined_data_ratio %>%
  ggplot(aes(x = paste(Species, Genotype), y = mean_ratio_canopy_root, color = Treatment)) +
  geom_point(size=4) +
  labs(title = "Ratio Root to Canopy Biomass") +
  theme_pubclean() +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  geom_errorbar(aes(ymin = `mean_ratio_canopy_root` - `sem_ratio_canopy_root`, 
                    ymax = `mean_ratio_canopy_root` + `sem_ratio_canopy_root`), width = 0.2) +
  theme(legend.position = "bottom")+
  xlab(label = "")+
  ylab(label = "Ratio of Root to Canopy Biomass")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# Show Plot
biomass_ratio_plot

# Save Plot
 #ggsave(paste0("fig_output/Biomass/Biomass_Ratio/Biomass_Root_Canopy_Ratio", ".png"))
 #ggsave(paste0("fig_output/Biomass/Biomass_Ratio/Biomass_Root_Canopy_Ratio", ".pdf"))


# Plot with Significance --------------------------------------------------

# Create a new column indicating whether a category is the last one or not
biomass_ratio <- biomass_ratio %>% 
  mutate(is_last = Species == last(Species)) %>% 
  ungroup()

# Perform t-test for each MassType and each species_geno
p_value <- biomass_ratio %>%
  group_by(Species, Genotype) %>%
  filter(!((Species == "vulpina"))) %>%
  summarise(p.value = if(n() >= 2) t.test(ratio_canopy_root ~ Treatment)$p.value else NA) %>%
  left_join(biomass_ratio %>% distinct(Species, Treatment), by = c( "Species"))

ratio_data_2 <- left_join(biomass_ratio_summary, p_value , by = c("Species", "Genotype", "Treatment"))

# Get the maximum value for each MassType
max_val <- biomass_ratio%>% 
  summarise(max_val = max(ratio_canopy_root))


# Plot
biomass_ratio_plot <- ratio_data_2  %>%
  ggplot(aes(x = paste(Species, Genotype), y = `mean_ratio_canopy_root`, color = Treatment)) +
  geom_point(size = 4) +
  theme_classic() +
  labs(x = "", 
       y = "Ratio of Root to Canopy Biomass") +
  geom_errorbar(aes(ymin = `mean_ratio_canopy_root` - `sem_ratio_canopy_root`, 
                    ymax = `mean_ratio_canopy_root` + `sem_ratio_canopy_root`), width = 0.3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  ylim(0, 2)+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Ratio of Root to Canopy Biomass across all Vitis species over time") +
  geom_text(aes(label = ifelse(p.value < 0.05, "*", "ns"),
                x = paste (Species, Genotype), y = 2),
            size = 5,  color = "black")
# Print the plot
print(biomass_ratio_plot)



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

