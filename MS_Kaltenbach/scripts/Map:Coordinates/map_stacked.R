library(dplyr)
library(raster, quietly = T)
library(ggplot2)
library(maps)
library(ggpubr)
library(patchwork)
library(cowplot)
library(RColorBrewer)
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
library(stats)
library(FactoMineR)
library(factoextra)
library(ggrepel)


setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# read in & modify data ------------------------------------------------------------
coordinates <- read.csv("data/coordinates.csv")

coordinates <- read.csv("data/VitisGenotypes_coordinates.csv")
coordinates <- coordinates %>%
  select(c("Genotype", "Species", "Sampling.Source", "Lat", "Long"))
colnames(coordinates)

coordinates$Species[coordinates$Genotype == "TXNM0821"] <- "hybrid"


coordinates <- (coordinates%>%
    filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                             "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

coordinates$Lat <- as.numeric(coordinates$Lat)
coordinates$Long <- as.numeric(coordinates$Long)
coordinates <- coordinates[complete.cases(coordinates[c("Lat", "Long")]), ]

# call climate data & create df -------------------------------------------------------
worldclim <- getData("worldclim", var = "bio", res = 10)
worldclim[[1:4]]
worldclim[[c("bio1")]]

NAmap <- crop(worldclim, extent(-150, -50, 0, 70))

map_df = as.data.frame(NAmap, xy = TRUE)
#head(map_df)
map_data =  map(database = "world", fill = TRUE)


# Create a vector of genotypes to color code
color_genotypes <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")
species_names <- c("V. acerifolia 9018", "V. arizonica b40-14", "V. cinerea b42-34", "hybrid TXNM0821", "V. rupestris Vru42") 


n_colors <- length(color_genotypes)

colors <- colorRampPalette(c('#EE6677', '#4477AA', '#228833', "green", "cyan", "grey", "brown", "purple", "red"))(n_colors)


coordinates$Genotypes <- ifelse(coordinates$Genotype %in% color_genotypes, coordinates$Genotype, "Grey")
ColorCode <- setNames(colors[colors != "Grey"], color_genotypes[color_genotypes != "Grey"])


temp_map <- ggplot() +
  geom_raster(data = map_df, aes(x = x, y = y, fill = bio1/10)) +
  coord_quickmap() +
  geom_point(data = coordinates, aes(x = Long, y = Lat, color = Genotypes, size = 3)) +
  scale_color_manual(values = colors, guide = "none") +
  labs(
       x = "Longitude", y = "Latitude",  fill = "Temperature (°C)") +
  #theme(legend.position = "bottom") +
  theme_bw() +
  scale_fill_gradientn(colours = c('green', 'yellow', 'brown'), na.value = "grey", limits = c(-5, 30)) +
  coord_cartesian(xlim = c(-121, -73), ylim = c(21, 44 )) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  geom_text(data = subset(coordinates, Genotypes != "Grey" & !(Genotypes %in% c("T48", "T52", "NY1", "V60-96"))),
            aes(x = Long, y = Lat, label = species_names),
            color = "black", fontface = "bold", size = 4, nudge_y = +0.5, nudge_x = -0.5, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "T48"),
            aes(x = Long, y = Lat, label = "V. mustangensis T48"),
            color = "black", fontface = "bold",  size = 4, nudge_y = 0.6, nudge_x = -2, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "T52"),
            aes(x = Long, y = Lat, label = "V.aestivalis T52"),
            color = "black", fontface = "bold", size = 4, nudge_y = -0.5, nudge_x = 4, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "NY1"),
            aes(x = Long, y = Lat, label =   "V. riparia NY1"),
            color = "black", fontface = "bold",  size = 4, nudge_y = 0.5, nudge_x = -4, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "V60-96"),
            aes(x = Long, y = Lat, label = "V. vulpina V60-96"),
            color = "black", fontface = "bold", size = 4, nudge_y = 0.5, nudge_x = -4, show.legend = FALSE) +  
  guides(size = "none")+
  theme(
    text = element_text(face = "bold"),
    legend.position = c(1, 0),  # Set legend position to the right bottom corner
    legend.justification = c(1, 0),  # Adjust justification for right bottom corner
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 8),  # Adjust the font size of legend labels
    legend.background = element_rect(color = "black", fill = "white")  # Add a black box around the legend
  ) 

print(temp_map)

# Plot Coordinates with annual Precipitation ----------------------------------

rain_map <- ggplot() +
  geom_raster(data = map_df, 
              aes(x = x, y = y, fill = bio12)) +
  coord_quickmap() +
  geom_point(data = coordinates, aes(x = Long, y = Lat, color = Genotypes, size = 3)) +
  scale_color_manual(values = colors, guide = "none", labels = species_names) +
  labs(
    x = "Longitude", y = "", fill = "Precipitation (mm)") +
  theme(legend.position = "none") +  # Turn off the default legend
  theme_bw() +
  scale_fill_gradientn(
    colours = c('white', "blue"),
    na.value = "grey",
    limits = c(0, 2500),
    breaks = seq(0, 2500, by = 500),
    labels = scales::comma(seq(0, 2500, by = 500))
  ) +
  coord_cartesian(xlim = c(-121, -73), ylim = c(21, 44)) + 
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", 
               fill = NA) + 
  geom_text(data = subset(coordinates, Genotypes != "Grey" & !(Genotypes %in% c("T48", "T52", "NY1", "V60-96"))),
            aes(x = Long, y = Lat, label = species_names),
            color = "black", fontface = "bold", size = 4, nudge_y = +0.8, nudge_x = -0.5, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "T48"),
            aes(x = Long, y = Lat, label = "V. mustangensis T48"),
            color = "black", fontface = "bold", size = 4, nudge_y = 0.8, nudge_x = -2, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "T52"),
            aes(x = Long, y = Lat, label = "V.aestivalis T52"),
            color = "black", fontface = "bold", size = 4, nudge_y = -0.5, nudge_x = 4, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "NY1"),
            aes(x = Long, y = Lat, label =   "V. riparia NY1"),
            color = "black", fontface = "bold", size = 4, nudge_y = 0.5, nudge_x = -4, show.legend = FALSE) +
  geom_text(data = subset(coordinates, Genotypes == "V60-96"),
            aes(x = Long, y = Lat, label = "V. vulpina V60-96"),
            color = "black", fontface = "bold",size = 4, nudge_y = 0.5, nudge_x = -4, show.legend = FALSE) +  

  theme(
    text = element_text(face = "bold"),
    legend.position = c(1, 0),  # Set legend position to the right bottom corner
    legend.justification = c(1, 0),  # Adjust justification for right bottom corner
    legend.text = element_text(size = 8), 
    legend.title = element_text(size = 8),  # Adjust the font size of legend labels
    legend.background = element_rect(color = "black", fill = "white")  # Add a black box around the legend
  ) +
  guides(size = "none")  # Turn off the point size legend

print(rain_map)


# combine temp. & precipitation -------------------------------------------

ggarrange(
  temp_map, rain_map,  nrow = 1, ncol = 2, labels = c("A", "B"),
  common.legend = FALSE)

#combined <- (temp_map / rain_map) 


