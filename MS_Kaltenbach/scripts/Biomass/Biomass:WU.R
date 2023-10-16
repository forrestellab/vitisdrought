# Load required packages
library(tidyverse)
library(ggpubr)

# Import data
cumWU <- read.csv("data/Subset/cumWu.csv")
harvest <- read.csv("data/Subset/Harvest.csv")
leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

# Subset and merge data sets
harvest <- harvest %>%
  select(ID, Treatment, Genotype, canopy_biomass, root_biomass)
cumWU <- cumWU %>%
  select(-X)
leaf <- leaf %>%
  select(-X)

df <- merge(cumWU, harvest, by = c("Genotype", "Treatment", "ID"), all = TRUE) 
biomass_combi <- merge(df, leaf, by = c("ID", "Genotype", "Treatment", "Species"), all = TRUE)

biomass_combi$Species[biomass_combi $Genotype == "TXNM0821"] <- "hybrid"


# Calculate biomass indices
biomass_combi <- biomass_combi %>%
  mutate("leafarea/wu" = TotalLeafArea / sumwu,
         "canopy_biomass/wu" = canopy_biomass / sumwu,
         "root_biomass/wu" = root_biomass / sumwu)

# Reshape data for plotting
biomass_wu_long <- gather(biomass_combi, key = "MassType", value = "value", 
                       "root_biomass/wu", "canopy_biomass/wu", "leafarea/wu") %>%
  select(-c(canopy_biomass, root_biomass, TotalLeafArea, sumwu)) %>%
  drop_na(value)

# Remove outliers
biomass_wu_long <- biomass_wu_long %>%
  group_by(MassType) %>%
  filter(between(value, quantile(value, 0.05), quantile(value, 0.95))) %>%
  ungroup()


biomass_wu_long <- biomass_wu_long  %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()

# Plot
biomass_wu_plot <- biomass_wu_long %>%
  ggplot(aes(x = species_geno, y = value, fill = Treatment)) +
  geom_boxplot() +
  facet_grid(facets = MassType ~ ., scales = "free_y", 
             labeller = labeller(MassType = c("root_biomass/wu" = 
                                                "Root BM/ WU (g/L) ",
                                              "canopy_biomass/wu" = 
                                                "Canopy BM/ WU (g/L)", 
                                              "leafarea/wu" = 
                                                "TLA/ WU (cm2/L)"))) +
  labs(x = "", y = "",
       y = "Biomass (g) or Total leaf area (cm2)") +
  theme_classic() +
  scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.background = element_rect(fill = "gray")) +
  geom_vline(data = ~subset(., !is_last),
             aes(xintercept = as.numeric(factor(species_geno)) + 0.5), 
             linetype = "dashed", color = "grey") + 
  geom_hline(yintercept = Inf, linetype = "dashed", color = "grey")+
  ggtitle(paste("Biomass relative to Water Usage across all Vitis species over time"))

print(biomass_wu_plot)
# Save plot
    ggsave("fig_output/Biomass/Biomass_WU/Biomass_WU.png", biomass_wu_plot)
    ggsave("fig_output/Biomass/Biomass_WU/Biomass_WU.pdf", biomass_wu_plot)

# Plot with Significance --------------------------------------------------

# Create a new column indicating whether a category is the last one or not
biomass_wu_long <- biomass_wu_long %>% 
  group_by(MassType) %>% 
  mutate(is_last = species_geno == last(species_geno)) %>% 
  ungroup()

    # Perform t-test for each MassType, Species, and Genotype
    p_value_wu <- biomass_wu_long %>%
      filter(!((MassType == "leafarea/wu" & species_geno == "mustangensis_T48") | 
                 (MassType == "leafarea/wu" & species_geno == "vulpina_V60-96"))) %>%
      group_by(MassType, Species, Genotype) %>%
      summarise(p.value = if(n() >= 2) t.test(value ~ Treatment)$p.value else NA) %>%
      left_join(biomass_wu_long %>% distinct(MassType, Species, Genotype, Treatment), by = c("MassType", "Species", "Genotype"))
    
    
    
    # Get the maximum value for each MassType
    wu_max_val <- biomass_wu_long %>% 
      group_by(MassType) %>% 
      summarise(max_val = max(value))
    
    # Join max_pval with wixon by MassType
    p_value_wu_max_val <- left_join(p_value_wu, wu_max_val, by = "MassType")
    
    # Plot
    
    biomass_plot_wu_all_sig <- biomass_wu_long %>%
      ggplot(aes(x = paste(Species, Genotype), y = value, fill = Treatment)) +
      geom_boxplot() +
      facet_grid(facets = MassType ~ ., scales = "free_y", 
                 labeller = labeller(MassType = c("root_biomass/wu" = 
                                                    "Root BM/ WU (g/L) ",
                                                  "canopy_biomass/wu" = 
                                                    "Canopy BM/ WU (g/L)", 
                                                  "leafarea/wu" = 
                                                    "TLA/ WU (cm2/L)"))) +
      labs(x = "", y = "",
           y = "Biomass (g) or Total leaf area (cm2)") +
      theme_classic() +
      scale_fill_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 30, hjust = 1),
            strip.background = element_rect(fill = "gray")) +
      ggtitle(paste("Biomass (BM) and Total Leaf area (TLA) relative to Water Usage (WU) across all Vitis species [", alpha_str, "= 0.05]")) +
      geom_text(data = p_value_wu_max_val %>% filter(p.value < 0.05),
                aes(label = "*", x = paste(Species, Genotype), y = 0.9 * max_val),
                fontface = "bold") +
      geom_text(data = p_value_wu_max_val %>% filter(p.value >= 0.05),
                aes(label = "ns", x = paste(Species, Genotype), y = 0.9 * max_val),
                fontface = "bold") +
      geom_vline(data = ~subset(., !is_last),
                 aes(xintercept = as.numeric(factor(paste(Species, Genotype))) + 0.5), 
                 linetype = "dashed", color = "grey") + 
      geom_hline(yintercept = Inf, linetype = "dashed", color = "grey")
    
    # Print the plot
    print(biomass_plot_wu_all_sig)
    
    

# Save plot
 ggsave("fig_output/Biomass/Biomass_WU/Biomass_WU_sig.png", biomass_plot_wu_all_sig)
 ggsave("fig_output/Biomass/Biomass_WU/Biomass_WU_sig.pdf", biomass_plot_wu_all_sig)


# Significance TABLE  -----------------------------------------------------

 
 
combined_wu_df <- biomass_wu_long %>%
  left_join(p_value_wu, by = c("MassType", "species_geno", "Treatment")) %>%
  select(ID, Genotype, Treatment, Species, species_geno, MassType, value, p.value)

biomass_wu_table <- combined_wu_df %>%
  filter(MassType %in% c("leafarea/wu", "canopy_biomass/wu", "root_biomass/wu")) %>%
  group_by(species_geno, Treatment, MassType) %>%
  summarise(mean_value = mean(value),
            sd_value = sd(value),
            p.value = mean(p.value)) %>%
  pivot_wider(names_from = MassType, values_from = c(mean_value, sd_value, p.value)) %>%
  ungroup()

colnames(biomass_wu_table)

biomass_wu_table <- biomass_wu_table %>%
  select(species_geno, Treatment, 
         "mean_value_leafarea/wu", "sd_value_leafarea/wu", "p.value_leafarea/wu", 
         "mean_value_canopy_biomass/wu", "sd_value_canopy_biomass/wu", "p.value_canopy_biomass/wu",
         "mean_value_root_biomass/wu", "sd_value_root_biomass/wu", "p.value_root_biomass/wu")

print(biomass_wu_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Biomass to Water Usage by Species")  %>% 
  gtsave(filename = "biomass_wu_table.html", path = "fig_output/Biomass/Biomass_WU/")



# write csv ---------------------------------------------------------------

# Save the data as a CSV file in the specified directory
write.csv(biomass_wu_table, file = "data_output/biomass/biomass_wu_table.csv", row.names = FALSE)
