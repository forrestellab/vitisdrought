# Read in Data ------------------------------------------------------------
combined_df <- read.csv("data_output/correlation/correlation_last_date")

combined_df <- read.csv("data_output/correlation/correlation_middle_date")
library(dplyr)
library(lubridate)
library(tidyverse)
library(stringr)
library(readxl)
library(hrbrthemes)
library(viridis)
# Licor Data --------------------------------------------------------------

# Set Working Directory
setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

# Load & Modify Data ---------------------------------------------------------------
  # Licor Data --------------------------------------------------------------
gswclean_me <- read.csv("data/gsw_6800_cleaned.csv")
pre_gswclean <- read.csv("data/gsw_6800_pre-clean.csv")

gswclean_me <- subset(gswclean_me, 
                      select = c("Date", "Genotype", "rep", "A", "Ci", "gsw_6800", "Treatment")) %>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))
pre_gswclean <- subset(pre_gswclean, 
                       select = c("Date", "Genotype", "rep", "A", "Ci", "E","Tleaf", 
                                  "gsw_6800", "Treatment")) %>%
  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))


merged_df_licor <- merge(gswclean_me, pre_gswclean[, c("Date", "Genotype", "rep", "A", "Ci", "E", "gsw_6800", "Tleaf", "Treatment")], 
                         by = c("Date", "Genotype", "rep", "A", "Ci", "gsw_6800", "Treatment"), 
                         all.y = TRUE)

merged_df_licor <- merged_df_licor %>% 
  mutate(new_date = ifelse(Date %in% c("10.26", "10.27", "10.28", "10.29"), "10.28",
                           ifelse(Date %in% c("11.12", "11.13"), "11.13",
                                  ifelse(Date %in% c("12.06", "12.07", 
                                                     "12.08", "12.09"), "12.08", Date))))
merged_df_licor <- merged_df_licor%>% 
  select(-Date) %>% 
  rename(Date = new_date) %>%
  rename(gsw = gsw_6800)

merged_df_licor$Date <- as.character(merged_df_licor$Date)
merged_df_licor$Date <- gsub("\\.\\d{4}", "", merged_df_licor$Date)
merged_df_licor$Date <- sprintf("%02d-%02d", 
                                as.numeric(substring(merged_df_licor$Date, 1, 2)), 
                                as.numeric(substring(merged_df_licor$Date, 4, 5)))

# Calculate WUE -----------------------------------------------------------

merged_df_licor <- merged_df_licor %>%
  mutate(intrWUE_AE = A / gsw,
         instWUE_AGs= A / E)


# Biomass Data ---------------------------------------------------------------
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



# waterpotentials ---------------------------------------------------------


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
  filter(!is.na(LWP) | !is.na(PD) | !is.na(SWP))%>% 
filter(species_geno %in% c("9018", "T52", "b40-14", "b42-34", 
                           "T48", "NY1", "TXNM0821", "Vru42", "V60-96" ))

leaf <- read.csv("data/Subset/Harvest_Leaf.csv")

leaf<- leaf %>%
  select(Genotype, Species)


# here --------------------------------------------------------------------
# Create file final date -------------------------------------------------

licor_final <- merged_df_licor%>%
  mutate(Date = as.Date(Date, format = "%m-%d")) %>%
  filter(Date == as.Date("12-08", format = "%m-%d"))  %>%
  select(-Date)

WP_final <- WP_filtered %>%
  mutate(Date = as.Date(Date_group, format = "%m/%d")) %>%
  filter(Date == as.Date("12/18", format = "%m/%d")) %>%
  select(!c("Date", "Date_group", "X", "species_geno", "Month", "Day"))

merged_final <- merge(licor_final, WP_final,
                         by = c("Genotype", "Treatment"), 
                         all.y = TRUE)

merged_final <- merge(merged_final, leaf,
                      by = c("Genotype"), 
                      all.y = TRUE)
merged_final<- merged_final[!duplicated(merged_final), ]
 
write.csv(merged_final, file = "data_output/dates_merged/merged_final", row.names = FALSE)

# Create file middle date -------------------------------------------------

licor_middle <- merged_df_licor%>%
  mutate(Date = as.Date(Date, format = "%m-%d")) %>%
  filter(Date == as.Date("11-13", format = "%m-%d"))  %>%
  select(-Date)


WP_middle <- WP_filtered %>%
  mutate(Date = as.Date(Date_group, format = "%m/%d")) %>%
  filter(Date == as.Date("11/13", format = "%m/%d")) %>%
  select(!c("Date", "Date_group", "X", "species_geno", "Month", "Day"))


merged_middle <- merge(licor_middle, WP_middle,
                      by = c("Genotype", "Treatment"), 
                      all.y = TRUE)

merged_middle <- merge(merged_middle, leaf,
                      by = c("Genotype"))

merged_middle<- merged_middle[!duplicated(merged_middle), ]

write.csv(merged_middle, file = "data_output/dates_merged/merged_middle", row.names = FALSE)

# Create file beginning date -------------------------------------------------

licor_beginning <- merged_df_licor%>%
  mutate(Date = as.Date(Date, format = "%m-%d")) %>%
  filter(Date == as.Date("10-28", format = "%m-%d"))  %>%
  select(-Date)

WP_beginning <- WP_filtered %>%
  mutate(Date = as.Date(Date_group, format = "%m/%d")) %>%
  filter(Date == as.Date("11/1", format = "%m/%d")) %>%
  select(!c("Date", "Date_group", "X", "species_geno", "Month", "Day"))

merged_beginning <- merge(licor_beginning, WP_beginning,
                      by = c("Genotype", "Treatment"), 
                      all.y = TRUE)

merged_beginning <- merge(merged_beginning, leaf,
                      by = c("Genotype"), 
                      all.y = TRUE)
merged_beginning<- merged_beginning[!duplicated(merged_beginning), ]


write.csv(merged_beginning, file = "data_output/dates_merged/merged_beginning", row.names = FALSE)

