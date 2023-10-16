library(rgdal)
library(raster, quietly = T)
library(ggplot2)
library(maps)
library(ggpubr)
library(patchwork)
library(foreach)
library(knitr)
library(gtExtras)
library(gt)
library(webshot2)
library(kableExtra)


setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# read in & modify data ------------------------------------------------------------
coordinates <- read.csv("data/coordinates.csv")
coordinates <- (coordinates%>%
                  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))
coordinates$Lat <- as.numeric(coordinates$Lat)
coordinates$Long <- as.numeric(coordinates$Long)
coordinates <- coordinates[complete.cases(coordinates[c("Lat", "Long")]), ]
coordinates_full <- coordinates
coordinates <- as.data.frame(coordinates[, c("Long", "Lat")])

worldclim_data <- getData("worldclim", var = "bio", res = 10)
climate_data <- raster::extract(worldclim_data, coordinates)
#View(climate_data)

geno_table <- cbind(coordinates_full, climate_data)

colnames(geno_table)

print(geno_table)

unique(coordinates$Genotype)


colnames(geno_table)[6:24] <- c("Annual Mean Temperature", 
                                "Mean Diurnal Range (Mean of monthly (max temp – min temp))", 
                                "Isothermality (BIO2/BIO7) (* 100)", 
                                "Temperature Seasonality (standard deviation *100)", 
                                "Max Temperature of Warmest Month", 
                                "Min Temperature of Coldest Month", 
                                "Temperature Annual Range (BIO5-BIO6)", 
                                "Mean Temperature of Wettest Quarter", 
                                "Mean Temperature of Driest Quarter", 
                                "Mean Temperature of Warmest Quarter", 
                                "Mean Temperature of Coldest Quarter", 
                                "Annual Precipitation", 
                                "Precipitation of Wettest Month", 
                                "Precipitation of Driest Month", 
                                "Precipitation Seasonality (Coefficient of Variation)", 
                                "Precipitation of Wettest Quarter", 
                                "Precipitation of Driest Quarter", 
                                "Precipitation of Warmest Quarter", 
                                "Precipitation of Coldest Quarter")

cols_to_divide <- c("Annual Mean Temperature",
                    "Mean Diurnal Range (Mean of monthly (max temp – min temp))",
                    "Max Temperature of Warmest Month",
                    "Min Temperature of Coldest Month",
                    "Temperature Seasonality (standard deviation *100)",
                    "Temperature Annual Range (BIO5-BIO6)",
                    "Mean Temperature of Wettest Quarter",
                    "Mean Temperature of Driest Quarter",
                    "Mean Temperature of Warmest Quarter",
                    "Mean Temperature of Coldest Quarter")

geno_table[,cols_to_divide] <- geno_table[,cols_to_divide] / 10

geno_table <- geno_table[order(-geno_table$`Annual Precipitation`), ]

geno_table <- geno_table[order(-geno_table$`Annual Mean Temperature`), ]

print(geno_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Climate Data of North American Vitis Species") 

#ggsave("geno_table.pdf", geno_table)

# Wanted Data -------------------------------------------------------------

bio_table <- geno_table[c("Genotype",
                          "Species", 
                          "Sampling.Source", 
                          "Lat",
                          "Long",
                          "Annual Mean Temperature", 
                          "Annual Precipitation",
                          "Temperature Seasonality (standard deviation *100)",
                          "Mean Temperature of Coldest Quarter")]


#write.csv(bio_table, file = "data_output/Geo_Data/bio_table", row.names = FALSE)

print(bio_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Climate Data of North American Vitis Species") 



# create gt table
bio_table_gt <- bio_table %>%
  gt() %>%
  gt_theme_nytimes() %>%
  tab_header(title = "Climate Data of North American Vitis Species")

# save as pdf
gtsave(bio_table_gt, filename = "fig_output/maps/bio_table.pdf")

# save as image
gtsave(bio_table_gt, filename = "fig_output/maps/bio_table.png")

# save as html
gtsave(bio_table_gt, filename = "fig_output/maps/bio_table.html")
