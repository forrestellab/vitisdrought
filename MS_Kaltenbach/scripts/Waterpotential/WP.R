library(tidyverse)
library(stringr)
library(readxl)
library(hrbrthemes)
library(viridis)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Import Data
WP_gathered <- read.csv("data/Subset/sub_wp.csv") %>% 
  mutate(Date = factor(Date),
         Month = substr(Date, 1, 2),
         Day = substr(Date, 4, 5),
         Date = paste0(Month, "/", Day),
         LWP = as.numeric(str_replace_all(LWP, ",", ".")),
         SWP = as.numeric(str_replace_all(SWP, ",", ".")),
         PD = as.numeric(str_replace_all(PD, ",", ".")))

WP_gathered$Date_group <- case_when(
  WP_gathered$Date %in% c("11/9", "11/11", "11/12", "11/13") ~ "11/13",
  WP_gathered$Date %in% c("11/16", "11/17", "11/18") ~ "11/18",
  WP_gathered$Date %in% c("12/1", "12/2", "12/3") ~ "12/03",
  WP_gathered$Date %in% c("12/11", "12/15", "12/16", "12/17", "12/18") ~ "12/18",
  TRUE ~ WP_gathered$Date)

# Filter out missing values
WP_filtered <- WP_gathered %>%
  mutate("leafwp_predawn" = LWP / PD)%>% 
  filter(!is.na(LWP) | !is.na(PD) | !is.na(SWP))

WP_summary <- WP_filtered %>% 
  group_by(Date_group, Treatment, species_geno) %>% 
  summarize(mean_value_LWP = mean(LWP, na.rm = TRUE),
            sem_LWP = sd(LWP, na.rm = TRUE) / sqrt(sum(!is.na(LWP))),
            mean_value_PD = mean(PD, na.rm = TRUE),
            sem_PD = sd(PD, na.rm = TRUE) / sqrt(sum(!is.na(PD))),
            mean_value_SWP = mean(SWP, na.rm = TRUE),
            sem_SWP = sd(SWP, na.rm = TRUE) / sqrt(sum(!is.na(SWP))),
            mean_value_leafwp_predawn = mean(leafwp_predawn, na.rm = TRUE),
            sem_leafwp_predawn = sd(leafwp_predawn, na.rm = TRUE) / 
              sqrt(sum(!is.na(leafwp_predawn)))) %>% 
            ungroup()%>%
             filter(species_geno %in% c("9018", "T52", "b40-14", "b42-34", 
                                    "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

write.csv(WP_summary, file = "data_output/WP/WP_summary.csv", row.names = FALSE)

# Leaf WP over Predawn ------------------------------------------------------------
# Identify Date_group values with non-missing mean_value_leafwp_predawn values
valid_Date_group_WP <- unique(WP_summary$Date_group[!is.na(WP_summary$mean_value_leafwp_predawn)])

# Filter data to only include valid Date_group values
WP_summary_filtered <- WP_summary %>% 
  filter(Date_group %in% valid_Date_group)

# Create boxplot for filtered data with error bars for SEM
leafwp_predawn_plot <- WP_summary_filtered %>% 
  ggplot(aes(x = species_geno, y = mean_value_leafwp_predawn, color = Treatment)) +
  geom_point(size = 3, na.rm=TRUE) +
  geom_errorbar(aes(ymin = mean_value_leafwp_predawn - sem_leafwp_predawn, 
                    ymax = mean_value_leafwp_predawn + sem_leafwp_predawn), width = 0.2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  facet_grid(facets = Date_group ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("LeafWP over PreDawn WP combined")

# Print the plot
print(leafwp_predawn_plot)

#path to save all files: 
ggsave(paste0("fig_output/WP/leafwp_predawn_plot", ".png"))
ggsave(paste0("fig_output/WP/leafwp_predawn_plot", ".pdf"))


# Leaf WP  ------------------------------------------------------------
# Identify Date_group values with non-missing mean_value_LWP values
valid_Date_group_LWP <- unique(WP_summary$Date_group[!is.na(WP_summary$mean_value_LWP)])

# Filter data to only include valid Date_group values
LWP_summary_filtered <- WP_summary %>% 
  filter(Date_group %in% valid_Date_group_LWP)

# Create boxplot for filtered data with error bars for SEM
LWP_plot <- LWP_summary_filtered %>% 
  ggplot(aes(x = Date_group, y = - mean_value_LWP, color = Treatment)) +
  geom_point(size = 3, na.rm=TRUE) +
  geom_errorbar(aes(ymin = - mean_value_LWP - sem_LWP, 
                    ymax = -mean_value_LWP + sem_LWP), width = 0.2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  facet_wrap(facets = species_geno ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("LeafWP")

# Print the plot
print(LWP_plot)

#path to save all files: 
ggsave(paste0("fig_output/WP/LWP_plot", ".png"))
ggsave(paste0("fig_output/WP/LWP_plot", ".pdf"))


# Leaf WP  ------------------------------------------------------------
# Identify Date_group values with non-missing mean_value_PD values
valid_Date_group_PD <- unique(WP_summary$Date_group[!is.na(WP_summary$mean_value_PD)])

# Filter data to only include valid Date_group values
PD_summary_filtered <- WP_summary %>% 
  filter(Date_group %in% valid_Date_group_PD)

# Create boxplot for filtered data with error bars for SEM
PD_plot <- PD_summary_filtered %>% 
  ggplot(aes(x = Date_group, y = - mean_value_PD, color = Treatment)) +
  geom_point(size = 3, na.rm=TRUE) +
  geom_errorbar(aes(ymin = - mean_value_PD - sem_PD, 
                    ymax = -mean_value_PD + sem_PD), width = 0.2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  facet_wrap(facets = species_geno ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("PD WP")

# Print the plot
print(PD_plot)

ggsave(paste0("fig_output/WP/PD_plot", ".png"))
ggsave(paste0("fig_output/WP/PD_plot", ".pdf"))

# Stem WP  ------------------------------------------------------------
# Identify Date_group values with non-missing mean_value_SWP values
valid_Date_group_SWP <- unique(WP_summary$Date_group[!is.na(WP_summary$mean_value_SWP)])

# Filter data to only include valid Date_group values
SWP_summary_filtered <- WP_summary %>% 
  filter(Date_group %in% valid_Date_group_SWP)

# Create boxplot for filtered data with error bars for SEM
SWP_plot <- SWP_summary_filtered %>% 
  ggplot(aes(x = species_geno, y = - mean_value_SWP, color = Treatment)) +
  geom_point(size = 3, na.rm=TRUE) +
  geom_errorbar(aes(ymin = - mean_value_SWP - sem_SWP, 
                    ymax = -mean_value_SWP + sem_SWP), width = 0.2) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Control" = "#022851", "Drought" = "#FFBF00")) +
  facet_grid(facets = Date_group ~ ., scales = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("SWP")

# Print the plot
print(SWP_plot)

ggsave(paste0("fig_output/WP/SWP_plot", ".png"))
ggsave(paste0("fig_output/WP/SWP_plot", ".pdf"))

