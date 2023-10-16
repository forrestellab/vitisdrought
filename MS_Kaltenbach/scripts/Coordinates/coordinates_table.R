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

coordinates <- read.csv("data/VitisGenotypes_coordinates.csv")
coordinates <- coordinates %>%
  select(c("Genotype", "Species", "Sampling.Source", "Lat", "Long"))
coordinates <- (coordinates%>%
                  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))
coordinates$Lat <- as.numeric(coordinates$Lat)
coordinates$Long <- as.numeric(coordinates$Long)
coordinates <- coordinates[complete.cases(coordinates[c("Lat", "Long")]), ]
coordinates$Species[coordinates$Genotype == "TXNM0821"] <- "hybrid"
coordinates$Sampling.Source[coordinates$Sampling.Source == "Tyree"] <- "UC Davis Breeding Collections"
coordinates_full <- coordinates
coordinates <- as.data.frame(coordinates[, c("Long", "Lat")])

worldclim_data <- getData("worldclim", var = "bio", res = 10)
climate_data <- raster::extract(worldclim_data, coordinates)
#View(climate_data)

geno_table <- cbind(coordinates_full, climate_data)


print(geno_table)




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

bio_table <- geno_table[c("Genotype",
                          "Species", 
                          "Sampling.Source", 
                          "Lat",
                          "Long",
                          "Annual Mean Temperature", 
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
                          "Precipitation of Coldest Quarter")]



colnames(bio_table)




write.csv(bio_table, file = "data_output/Geo_Data/bio_table1", row.names = FALSE)

# Assuming 'bio_table' is the name of your data frame
sorted_by_temperature <- bio_table[order(bio_table$`Annual Mean Temperature`, decreasing = TRUE), ]



print(sorted_by_temperature) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Climate Data of North American Vitis Species") 

sorted_by_precipitation <- bio_table[order(bio_table$`Annual Precipitation`), ]




print(sorted_by_precipitation) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Climate Data of North American Vitis Species") 




print(bio_table) %>% 
  gt() %>% 
  gt_theme_nytimes()  %>% 
  tab_header(title = "Climate Data of North American Vitis Species") 


# PLOT RAIN ---------------------------------------------------------------

# Create the dot plot
scaleFactor <- max(bio_table$`Annual Precipitation`) / max(bio_table$`Annual Mean Temperature`)

ggplot(bio_table, aes(x = reorder(paste(Species, Genotype, sep = "  "), -`Annual Precipitation`))) +
  geom_col(aes(y = `Annual Precipitation`), fill = "#022851", width = 0.5) +
  geom_point(aes(y = `Annual Mean Temperature` * scaleFactor), col = "#c00000", size = 4) +
  scale_y_continuous(name = "Annual Precipitation (mm)", sec.axis = sec_axis(~./scaleFactor, name = "Mean Temperature (°C)")) +
  labs(title = "Annual Precipitation and Mean Temperature of Selected Genotypes") +
  theme(
    axis.title.y.left = element_text(color = "#022851", size = 12),
    axis.text.y.left = element_text(color = "#022851", size = 12),
    axis.title.y.right = element_text(color = "#c00000", size = 12),
    axis.text.y.right = element_text(color = "#c00000", size = 12),
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 30, vjust = 0.5, size = 12),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )


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
