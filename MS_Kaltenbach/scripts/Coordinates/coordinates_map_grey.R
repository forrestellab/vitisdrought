library(raster, quietly = T)
library(ggplot2)
library(maps)
library(ggpubr)
library(patchwork)
library(cowplot)
library(RColorBrewer)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")


# read in & modify data ------------------------------------------------------------
coordinates <- read.csv("data/coordinates.csv")

#coordinates <- (coordinates%>%
               #     filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
              #                             "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))

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

coordinates$Genotypes <- ifelse(coordinates$Genotype %in% color_genotypes, coordinates$Genotype, "Grey")
non_grey_colors <- setNames(colors[colors != "Grey"], color_genotypes[color_genotypes != "Grey"])

n_colors <- length(color_genotypes)
colors <- brewer.pal(n_colors, "Set1")

temp_map <- ggplot() +
  geom_raster(data = map_df, aes(x = x, y = y, fill = bio1/10)) +
  coord_quickmap() +
  geom_point(data = coordinates, aes(x = Long, y = Lat, color = Genotypes, size = 3)) +
  scale_color_manual(values = non_grey_colors, guide = "legend") +
  labs(title = "Map of North America with Annual Mean Temperature and Genotype Coordinates",
       x = "Longitude", y = "Latitude",  fill = "Temperature (°C)") +
  theme(legend.position = "bottom") +
  theme_bw() +
  scale_fill_gradientn(colours = c('green', 'yellow', 'brown'), na.value = "grey", limits = c(-5, 30)) +
  coord_cartesian(xlim = c(-121, -75), ylim = c(21, 39)) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype != "T52" & Genotype != "T48"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = +0.5, nudge_x = - 0.5,  show.legend = FALSE) +
  geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype == "T52"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = -0.5, nudge_x = 1.5,  show.legend = FALSE, check_overlap = TRUE) +
  geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype == "T48"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = +0.5, nudge_x = -1.5,  show.legend = FALSE, check_overlap = TRUE)+
            guides(size = "none") 

print(temp_map)

# Plot Coordinates with annual Precipitation ----------------------------------
rain_map <- ggplot() +
        geom_raster(data = map_df, 
                    aes(x = x, y = y, fill = bio12)) +
        coord_quickmap() +
        geom_point(data = coordinates, aes(x = Long, y = Lat, color = Genotypes, size = 3)) +
        scale_color_manual(values = non_grey_colors, guide = "legend") +
        labs(title = "Map of North America with Annual Precipitation and Genotype Coordinates", 
             x = "Longitude", y = "Latitude", fill = "Precipitation (mm)") +
        theme(legend.position = "bottom") + 
        theme_bw() +
        scale_fill_gradientn(colours=c('white', "blue"),
                             na.value = "grey", 
                             limits = c(0, 2500))+
        coord_cartesian(xlim = c(-121, -75), ylim = c(21, 39))+ 
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", 
                     fill = NA)+ 
        geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype != "T52" & Genotype != "T48"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = +0.5, nudge_x = - 0.5,  show.legend = FALSE) +
        geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype == "T52"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = -0.5, nudge_x = 1.5,  show.legend = FALSE, check_overlap = TRUE) +
        geom_text(data = subset(coordinates, ColorCode != "Grey" & Genotype == "T48"),
            aes(x = Long, y = Lat, label = paste(Genotype, Species)),
            color = "black", size = 3, nudge_y = +0.5, nudge_x = -1.5,  show.legend = FALSE, check_overlap = TRUE)+
        guides(size = "none") 

      print(rain_map)

# Plot Coordinates on Map -------------------------------------------------
coordinates_map <- ggplot() +
        geom_raster(data = map_df, 
                    aes(x = x, y = y, fill = bio1/10,)) +
        coord_quickmap() +
        scale_fill_gradient(low = "white", high = "white", guide = "none") +
        geom_point(data = coordinates, aes(x = Long, y = Lat, color = Genotype)) +
       scale_color_manual(values = rainbow(length(unique(coordinates$Genotype)))) +
        labs(title = "Map of North America with Coordinates of coordinates", 
             x = "Longitude", y = "Latitude", color = "Genotype") +
        theme(legend.position = "right") + 
        coord_cartesian(xlim = c(-121, -75), ylim = c(20, 49))+ 
        geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", 
                     fill = NA)
      
print(coordinates_map)  

#ggsave(paste0("fig_output/maps/coordinates_map", ".png"))
#ggsave(paste0("fig_output/maps/coordinates_map", ".pdf"))


# combine temp. & precipitation -------------------------------------------
combined <- temp_map + rain_map & theme(legend.position = "bottom")
combined_map <- combined + plot_layout(guides = "collect")

print(combined_map)

ggsave(paste0("fig_output/maps/combined_map_grey", ".png"))
ggsave(paste0("fig_output/maps/combined_map_grey", ".pdf"))


