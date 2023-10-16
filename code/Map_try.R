library(raster)
library(ggplot2)
library(maps)

setwd("~/Documents/GitHub/vitisdrought/MS_Kaltenbach")

genotypes <- read.csv("data/coordinates.csv")
genotypes$Lat <- as.numeric(genotypes$Lat)
genotypes$Long <- as.numeric(genotypes$Long)
genotypes <- genotypes[complete.cases(genotypes[c("Lat", "Long")]), ]

NAmap <- getData("worldclim", var = "tmean", res = 10)[[1]]
NAmap <- crop(NAmap, extent(-150, -50, 0, 70))

df <- as.data.frame(raster::values(NAmap))
df$Long <- rep(seq(-150, -50, length.out = ncol(df)), each = nrow(df))
df$Lat <- rep(seq(0, 70, length.out = nrow(df)), times = ncol(df))
df$Temp <- as.numeric(df$layer)

ggplot() +
  geom_raster(data = df, aes(x = Long, y = Lat, fill = Temp)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10))) +
  geom_point(data = genotypes, aes(x = Long, y = Lat, color = Genotype)) +
  scale_color_manual(values = rainbow(length(unique(genotypes$Genotype)))) +
  labs(title = "Map of North America with Annual Mean Temperature and Genotype Coordinates", 
       x = "Longitude", y = "Latitude", color = "Genotype") +
  theme(legend.position = "right")


NAmap <- getData("worldclim", var = "bio", res = 10)[[1]]
NAmap <- crop(NAmap, extent(-150, -50, 0, 70))
df <- data.frame(Long = raster::x(NAmap), 
                 Lat = raster::y(NAmap))


map_data <- map(database = "world", regions = c("USA", "Mexico"), fill = TRUE)

ggplot() + 
  geom_raster(data = df, aes(x = Long, y = Lat)) +
  coord_quickmap(xlim = c(-125, -65), ylim = c(15, 50)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), color = "black", fill = NA) +
  geom_point(data = genotypes
             
