library(dplyr)
library(lubridate)
library(tidyverse)
library(readxl)
library(ggpubr)
library(ggplot2)
library(hrbrthemes)

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")



# Create file final date -------------------------------------------------
biomass_wu_df <- read.csv("data_output/biomass/biomass_wu_table.csv")
biomass_wu_df$species_geno[biomass_wu_df$species_geno == "riparia_TXNM0821"] <- "hybrid_TXNM0821"

geo_df <- read.csv("data_output/Geo_Data/bio_table")
geo_df$Species[geo_df $Genotype == "TXNM0821"] <- "hybrid"
geo_df$species_geno <- paste0(geo_df$Species, "_", geo_df$Genotype)

biomass_df <- read.csv("data_output/biomass/biomass_table.csv")
biomass_df$species_geno[biomass_df$species_geno == "riparia_TXNM0821"] <- "hybrid_TXNM0821"

geo_biomass_df <- merge(geo_df, biomass_df, by = "species_geno", all = TRUE)

water_use_df <- read.csv("data_output/biomass/summary_data_cumWU.csv")
water_use_df$species_geno <- paste0(water_use_df$Species, "_", water_use_df$Genotype)

licor_df <- read.csv("data_output/licor/summary_merged_df_licor.csv") %>%
  mutate(Date = as.Date(Date, format = "%m-%d")) %>%
  filter(Date == as.Date("12-08", format = "%m-%d"))  %>%
  select(-Date)

wp_df <- read.csv("data_output/WP/WP_summary.csv") %>%
  mutate(Date = as.Date(Date_group, format = "%m/%d")) %>%
  filter(Date == as.Date("12/18", format = "%m/%d")) %>%
  rename(Genotype = "species_geno") %>%
  select(!c("Date", "Date_group"))


wu_licor_wp_df <- left_join(wp_df, water_use_df, by = c("Genotype", "Treatment"))
wu_licor_wp_df <- left_join(wu_licor_wp_df, licor_df, by = c("Genotype", "Treatment"))

seg_df<- read.csv("data_output/segmentation/grouped_data_seg.csv")

common_species_geno <- intersect(geo_df$species_geno, biomass_df$species_geno)
combined_df <- data.frame()

for (s in common_species_geno) {
  geo_biomass_subset <- subset(geo_biomass_df, species_geno == s)
  seg_subset <- subset(seg_df, species_geno == s)
  biomass_wu_subset <- subset(biomass_wu_df, species_geno == s)
  wu_licor_wp_df_subset <- subset(wu_licor_wp_df, species_geno == s)
  combined_subset <- cbind(geo_biomass_subset, wu_licor_wp_df_subset, biomass_wu_subset, seg_subset)
  combined_df <- rbind(combined_df, combined_subset, row.names = NULL)
}


combined_df <- combined_df[, !duplicated(colnames(combined_df))]%>% 
  arrange(desc(Annual.Precipitation))


write.csv(combined_df, file = "data_output/correlation/correlation_last_date", row.names = FALSE)

drop_cols <- grepl("sd|p\\.value|sem|p\\_value", colnames(combined_df))
combined_df <- combined_df[, !drop_cols]

species_order <- combined_df %>% 
  arrange(desc(Annual.Precipitation)) %>% 
  pull(unique(species_geno))





# Create file middle  date -------------------------------------------------
biomass_wu_df <- read.csv("data_output/biomass/biomass_wu_table.csv")
biomass_wu_df$species_geno[biomass_wu_df$species_geno == "riparia_TXNM0821"] <- "hybrid_TXNM0821"

geo_df <- read.csv("data_output/Geo_Data/bio_table")
geo_df$Species[geo_df $Genotype == "TXNM0821"] <- "hybrid"
geo_df$species_geno <- paste0(geo_df$Species, "_", geo_df$Genotype)

biomass_df <- read.csv("data_output/biomass/biomass_table.csv")
biomass_df$species_geno[biomass_df$species_geno == "riparia_TXNM0821"] <- "hybrid_TXNM0821"

geo_biomass_df <- merge(geo_df, biomass_df, by = "species_geno", all = TRUE)

water_use_df <- read.csv("data_output/biomass/summary_data_cumWU.csv")
water_use_df$species_geno <- paste0(water_use_df$Species, "_", water_use_df$Genotype)

licor_df <- read.csv("data_output/licor/summary_merged_df_licor.csv") %>%
  mutate(Date = as.Date(Date, format = "%m-%d")) %>%
  filter(Date == as.Date("11-13", format = "%m-%d"))  %>%
  select(-Date)

wp_df <- read.csv("data_output/WP/WP_summary.csv") %>%
  mutate(Date = as.Date(Date_group, format = "%m/%d")) %>%
  filter(Date == as.Date("11/13", format = "%m/%d")) %>%
  rename(Genotype = "species_geno") %>%
  select(!c("Date", "Date_group"))


wu_licor_wp_df <- left_join(wp_df, water_use_df, by = c("Genotype", "Treatment"))
wu_licor_wp_df <- left_join(wu_licor_wp_df, licor_df, by = c("Genotype", "Treatment"))

seg_df<- read.csv("data_output/segmentation/grouped_data_seg.csv")

common_species_geno <- intersect(geo_df$species_geno, biomass_df$species_geno)
combined_df <- data.frame()

for (s in common_species_geno) {
  geo_biomass_subset <- subset(geo_biomass_df, species_geno == s)
  seg_subset <- subset(seg_df, species_geno == s)
  biomass_wu_subset <- subset(biomass_wu_df, species_geno == s)
  wu_licor_wp_df_subset <- subset(wu_licor_wp_df, species_geno == s)
  combined_subset <- cbind(geo_biomass_subset, wu_licor_wp_df_subset, biomass_wu_subset, seg_subset)
  combined_df <- rbind(combined_df, combined_subset, row.names = NULL)
}


combined_df <- combined_df[, !duplicated(colnames(combined_df))]%>% 
  arrange(desc(Annual.Precipitation))

write.csv(combined_df, file = "data_output/correlation/correlation_middle_date", row.names = FALSE)

drop_cols <- grepl("sd|p\\.value|sem|p\\_value", colnames(combined_df))
combined_df <- combined_df[, !drop_cols]

species_order <- combined_df %>% 
  arrange(desc(Annual.Precipitation)) %>% 
  pull(unique(species_geno))
