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

biomass_combi <- biomass_combi %>%
  mutate("WU/leafarea" = sumwu/TotalLeafArea) %>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")) %>%
  drop_na(`WU/leafarea`) %>%
  filter(between(`WU/leafarea`, quantile(`WU/leafarea`, 0.05), quantile(`WU/leafarea`, 0.95)))



biomass_combi <- biomass_combi %>%
  mutate("WU/leafarea" = sumwu/TotalLeafArea)%>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")) %>%
  drop_na(`WU/leafarea`) 

biomass_combi_summary <- biomass_combi %>%
  group_by(Genotype, Treatment, Species) %>%
  summarise(`mean_WU/leafarea` = mean(`WU/leafarea`), `sem_WU/leafarea` = sd(`WU/leafarea`) / sqrt(n())) %>%
  ungroup()

combined_data <- left_join(biomass_combi, biomass_combi_summary, by = c("Genotype", "Treatment", "Species"))


# Plot
biomass_combi_plot <- combined_data  %>%
  ggplot(aes(x = paste(Species, Genotype), y = `mean_WU/leafarea`, color = Treatment)) +
  geom_point(size = 3) +
  theme_classic()+
  labs(x = "", 
       y = "Water Use per Unit Leaf Area (L/cm2)") +
  geom_errorbar(aes(ymin = `mean_WU/leafarea` - `sem_WU/leafarea`, 
                    ymax = `mean_WU/leafarea` + `sem_WU/leafarea`), width = 0.2) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00"))+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle(paste("Water Usage relative to Leaf Area across all Vitis species over time"))

print(biomass_combi_plot)

# Plot with Significance --------------------------------------------------

# Perform t-test
p_value_wu <- combined_data  %>%
  filter(Treatment %in% c("Drought", "Control")) %>%
  group_by(Genotype, Species) %>%
  summarise(p.value = t.test(`WU/leafarea` ~ Treatment)$p.value)

combined_data_2 <- left_join(biomass_combi_summary, p_value_wu , by = c("Species", "Genotype"))

# Plot
biomass_combi_plot <- combined_data_2  %>%
  ggplot(aes(x = paste(Species, Genotype), y = `mean_WU/leafarea`, color = Treatment)) +
  geom_point(size = 4) +
  theme_classic() +
  theme_bw() +
  labs(x = "", 
       y = "Water Use per Unit Leaf Area (L/cm2)") +
  geom_errorbar(aes(ymin = `mean_WU/leafarea` - `sem_WU/leafarea`, 
                    ymax = `mean_WU/leafarea` + `sem_WU/leafarea`), width = 0.3) +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  ylim(0, 0.005)+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Water Usage relative to Leaf Area across all Vitis species over time") +
geom_text(aes(label = ifelse(p.value < 0.05, "*", "ns"),
              x = paste (Species, Genotype), y = 0.005),
          size = 5,  color = "black")
# Print the plot
print(biomass_combi_plot)


