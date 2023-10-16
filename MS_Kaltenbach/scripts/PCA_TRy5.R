
# load packages -----------------------------------------------------------


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
coordinates$Lat <- as.numeric(coordinates$Lat)
coordinates$Long <- as.numeric(coordinates$Long)
coordinates <- coordinates[complete.cases(coordinates[c("Lat", "Long")]), ]
coordinates$Species[coordinates$Genotype == "TXNM0821"] <- "hybrid"
coordinates_full <- coordinates
coordinates <- as.data.frame(coordinates[, c("Long", "Lat")])

worldclim_data <- getData("worldclim", var = "bio", res = 10)
climate_data <- raster::extract(worldclim_data, coordinates)
#View(climate_data)

geno_table <- cbind(coordinates_full, climate_data)

colnames(geno_table)

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

#geno_table[,cols_to_divide] <- geno_table[,cols_to_divide] / 10

# Run PCA --------------------------------------------------------------

climate_data <- geno_table[, 6:24]
climate_data <- climate_data[complete.cases(climate_data), ]


climate_data_std <- scale(climate_data)

pca <- prcomp(climate_data_std, center = TRUE, scale = FALSE)

# Scale the geno_table data
geno_table_scaled <- scale(geno_table[, -c(1:5)])

# Perform Principal Component Analysis
pc <- prcomp(x = geno_table_scaled, center = TRUE, scale. = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)


pc_summary <- summary(pc)
proportion_of_variance <- pc_summary$importance[2, ]
importance_percent <- proportion_of_variance * 100


# Set the genotypes to be colored
color_genotypes <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")

# Create a color palette for genotypes
genotype_colors <- rep("grey70", length(geno_table$Genotype))
genotype_colors[geno_table$Genotype %in% color_genotypes] <- "#F55C7A"

highlighted_arrows <- c(
  "Annual Mean Temperature",
  "Annual Precipitation",
  "Temperature Seasonality (standard deviation *100)",
  "Mean Temperature of Coldest Quarter",
  "Mean Temperature of Warmest Quarter"
)

# Plot PC1&2 --------------------------------------------------------------

# Plotting the biplot with colored genotypes
plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2 ), labels = TRUE)
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
      ylab = paste("PC 2 (", round(importance_percent[2], digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)

par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")

mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 2 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 3, las = 3)

box()
abline(v = 0, h = 0, lty = 2, col = "grey")
arrows(x0 = 0, x1 = pc$rotation[, 1], y0 = 0, y1 = pc$rotation[, 2],   col = ifelse(
  row.names(pc$rotation) %in% highlighted_arrows, "navy", "grey"), length = 0.08, lwd = 2, angle = 30)


text(
  x = pc$rotation[, 1],
  y = pc$rotation[, 2],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2, 
  col = ifelse(
    row.names(pc$rotation) %in% highlighted_arrows,"navy", "grey"),
  pos = c(3, 1, 1, 3, 1)
)

# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  # Determine the color based on genotype
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(species_points[i], genotype_points[i], sep = " - ")
  
  text(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    labels = label,
    col = genotype_color,
    pos = 3,
    offset = 0.4, 
    cex = 0.8
  )
}


# Plot PC2&3 --------------------------------------------------------------

# Plotting the biplot with colored genotypes
plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 2 (", round(importance_percent[2], digits = 1), "%)", sep = ""),
      ylab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)
# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
# Add labels for rotated axes
mtext(text = "PC 2 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 3 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 2], y0 = 0, y1 = pc$rotation[, 3], col = "navy", length = 0.08, lwd = 2, angle = 30)

# Plotting text
text(
  x = pc$rotation[, 2],
  y = pc$rotation[, 3],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2, 
  col = "navy",
  pos = c(3, 1, 1, 3, 1)
)

# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  # Determine the color based on genotype
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 2],
    y = species_scores[i, 3],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(species_points[i], genotype_points[i], sep = " - ")
  
  text(
    x = species_scores[i, 2],
    y = species_scores[i, 3],
    labels = label,
    col = genotype_color,
    pos = 3,
    offset = 0.4, 
    cex = 0.8
  )
}

ggsave(paste0("fig_output/PCA/PC2_3_arrows", ".png"))
ggsave(paste0("fig_output/PCA/PC2_3_arrows", ".pdf"))

# Plot PC1&3 --------------------------------------------------------------

# Plotting the biplot with colored genotypes
plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
      ylab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)
# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")

# Add labels for rotated axes
mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 3 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

# Add a box, reference lines, and arrows
box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 1], y0 = 0, y1 = pc$rotation[, 3], col = "navy", length = 0.08, lwd = 2, angle = 30)

# Plotting text
text(
  x = pc$rotation[, 1],
  y = pc$rotation[, 3],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2, 
  col = "navy",
  pos = c(3, 1, 1, 3, 1)
)
# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  # Determine the color based on genotype
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 3],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(species_points[i], genotype_points[i], sep = " - ")
  
  text(
    x = species_scores[i, 1],
    y = species_scores[i, 3],
    labels = label,
    col = genotype_color,
    pos = 3,
    offset = 0.4, 
    cex = 0.8
  )
}

ggsave(paste0("fig_output/PCA/PC1_3_arrows", ".png"))
ggsave(paste0("fig_output/PCA/PC1_3_arrows", ".pdf"))

# Plot PC1&4 --------------------------------------------------------------

# Plotting the biplot with colored genotypes
plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
      ylab = paste("PC 4 (", round(importance_percent[4], digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)
# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")

# Add labels for rotated axes
mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 4 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

# Add a box, reference lines, and arrows
box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 1], y0 = 0, y1 = pc$rotation[, 4], col = "navy", length = 0.08, lwd = 2, angle = 30)

# Plotting text
text(
  x = pc$rotation[, 1],
  y = pc$rotation[, 4],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2, 
  col = "navy",
  pos = c(3, 1, 1, 3, 1)
)
# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  # Determine the color based on genotype
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 4],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(species_points[i], genotype_points[i], sep = " - ")
  
  text(
    x = species_scores[i, 1],
    y = species_scores[i, 4],
    labels = label,
    col = genotype_color,
    pos = 3,
    offset = 0.4, 
    cex = 0.8
  )
}

ggsave(paste0("fig_output/PCA/PC1_4_arrows", ".png"))
ggsave(paste0("fig_output/PCA/PC1_4_arrows", ".pdf"))


# Plot PC3&4 --------------------------------------------------------------

# Plotting the biplot with colored genotypes
plot.new()
plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
      ylab = paste("PC 4 (", round(importance_percent[4], digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)
# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
# Add labels for rotated axes
mtext(text = "PC 3 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 4 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 3], y0 = 0, y1 = pc$rotation[, 4], col = "navy", length = 0.08, lwd = 2, angle = 30)

# Plotting text
text(
  x = pc$rotation[, 3],
  y = pc$rotation[, 4],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2, 
  col = "navy",
  pos = c(3, 1, 1, 3, 1)
)

# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  # Determine the color based on genotype
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 3],
    y = species_scores[i, 4],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(species_points[i], genotype_points[i], sep = " - ")
  
  text(
    x = species_scores[i, 3],
    y = species_scores[i, 4],
    labels = label,
    col = genotype_color,
    pos = 3,
    offset = 0.4, 
    cex = 0.8
  )
}

ggsave(paste0("fig_output/PCA/PC3_4_arrows", ".png"))
ggsave(paste0("fig_output/PCA/PC3_4_arrows", ".pdf"))

# in case I want a circle -------------------------------------------------
  # Creating a smaller unit circle
  # scaling_factor <- 0.646  # Adjust this factor to control the size of the circle
  # ucircle <- scaling_factor * cbind(
  #   cos((0:360) / 180 * pi),
  #   sin((0:360) / 180 * pi)
  # )
  # 
  # polygon(
  #   ucircle,
  #   lty = "solid",
  #   border = "gray10",
  #   lwd = 1
  # )


# Without arrows ----------------------------------------------------------

  # without arrows Plot PC1&2 --------------------------------------------------------------
  
  # Plotting the biplot with colored genotypes
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
  axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  axis(side = 2, at = c(-2, -1, 0, 1, 2 ), labels = TRUE)
  title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
  title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
        ylab = paste("PC 2 (", round(importance_percent[2], digits = 1), "%)", sep = ""),
        line = 2, adj = 0.5)
  
  par(new = TRUE, las = 1)
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  
  mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
  mtext(text = "PC 2 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)
  
  box()
  abline(v = 0, h = 0, lty = 2, col = "grey25")
  
  # Add species points to the biplot with colored genotypes
  species_points <- geno_table$Species
  genotype_points <- geno_table$Genotype
  species_scores <- predict(pc, geno_table_scaled)
  
  # Create a color palette for species points
  colors <- rainbow(length(species_points))
  
  for (i in seq_along(species_points)) {
    # Determine the color based on genotype
    genotype_color <- genotype_colors[i]
    
    points(
      x = species_scores[i, 1],
      y = species_scores[i, 2],
      col = genotype_color,
      pch = 16,
      cex = 0.8
    )
    
    label <- paste(species_points[i], genotype_points[i], sep = " - ")
    
    text(
      x = species_scores[i, 1],
      y = species_scores[i, 2],
      labels = label,
      col = genotype_color,
      pos = 3,
      offset = 0.4, 
      cex = 0.8
    )
  }
  
  ggsave(paste0("fig_output/PCA/PC1_2_no_arrows", ".png"))
  ggsave(paste0("fig_output/PCA/PC1_2_no_arrows", ".pdf"))
  
  # without arrows Plot PC2&3 --------------------------------------------------------------
  
  # Plotting the biplot with colored genotypes
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
  axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
  title(xlab = paste("PC 2 (", round(importance_percent[2], digits = 1), "%)", sep = ""),
        ylab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
        line = 2, adj = 0.5)
  # Add rotated axes
  par(new = TRUE, las = 1)
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  # Add labels for rotated axes
  mtext(text = "PC 2 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
  mtext(text = "PC 3 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)
  
  box()
  abline(v = 0, h = 0, lty = 2, col = "grey25")
 
  # Add species points to the biplot with colored genotypes
  species_points <- geno_table$Species
  genotype_points <- geno_table$Genotype
  species_scores <- predict(pc, geno_table_scaled)
  
  # Create a color palette for species points
  colors <- rainbow(length(species_points))
  
  for (i in seq_along(species_points)) {
    # Determine the color based on genotype
    genotype_color <- genotype_colors[i]
    
    points(
      x = species_scores[i, 2],
      y = species_scores[i, 3],
      col = genotype_color,
      pch = 16,
      cex = 0.8
    )
    
    label <- paste(species_points[i], genotype_points[i], sep = " - ")
    
    text(
      x = species_scores[i, 2],
      y = species_scores[i, 3],
      labels = label,
      col = genotype_color,
      pos = 3,
      offset = 0.4, 
      cex = 0.8
    )
  }
  
  ggsave(paste0("fig_output/PCA/PC2_3_no_arrows", ".png"))
  ggsave(paste0("fig_output/PCA/PC2_3_no_arrows", ".pdf"))
  
  # without arrows Plot PC1&3 --------------------------------------------------------------
  
  # Plotting the biplot with colored genotypes
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
  axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
  title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
        ylab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
        line = 2, adj = 0.5)
  # Add rotated axes
  par(new = TRUE, las = 1)
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  
  # Add labels for rotated axes
  mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
  mtext(text = "PC 3 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)
  
  # Add a box, reference lines, and arrows
  box()
  abline(v = 0, h = 0, lty = 2, col = "grey25")

  # Add species points to the biplot with colored genotypes
  species_points <- geno_table$Species
  genotype_points <- geno_table$Genotype
  species_scores <- predict(pc, geno_table_scaled)
  
  # Create a color palette for species points
  colors <- rainbow(length(species_points))
  
  for (i in seq_along(species_points)) {
    # Determine the color based on genotype
    genotype_color <- genotype_colors[i]
    
    points(
      x = species_scores[i, 1],
      y = species_scores[i, 3],
      col = genotype_color,
      pch = 16,
      cex = 0.8
    )
    
    label <- paste(species_points[i], genotype_points[i], sep = " - ")
    
    text(
      x = species_scores[i, 1],
      y = species_scores[i, 3],
      labels = label,
      col = genotype_color,
      pos = 3,
      offset = 0.4, 
      cex = 0.8
    )
  }

  ggsave(paste0("fig_output/PCA/PC1_3_no_arrows", ".png"))
  ggsave(paste0("fig_output/PCA/PC1_3_no_arrows", ".pdf"))
  
  # without arrows Plot PC1&4 --------------------------------------------------------------
  
  # Plotting the biplot with colored genotypes
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
  axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
  title(xlab = paste("PC 1 (", round(importance_percent[1], digits = 1), "%)", sep = ""),
        ylab = paste("PC 4 (", round(importance_percent[4], digits = 1), "%)", sep = ""),
        line = 2, adj = 0.5)
  # Add rotated axes
  par(new = TRUE, las = 1)
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  
  # Add labels for rotated axes
  mtext(text = "PC 1 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
  mtext(text = "PC 4 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)
  
  # Add a box, reference lines, and arrows
  box()
  abline(v = 0, h = 0, lty = 2, col = "grey25")
  
  # Add species points to the biplot with colored genotypes
  species_points <- geno_table$Species
  genotype_points <- geno_table$Genotype
  species_scores <- predict(pc, geno_table_scaled)
  
  # Create a color palette for species points
  colors <- rainbow(length(species_points))
  
  for (i in seq_along(species_points)) {
    # Determine the color based on genotype
    genotype_color <- genotype_colors[i]
    
    points(
      x = species_scores[i, 1],
      y = species_scores[i, 4],
      col = genotype_color,
      pch = 16,
      cex = 0.8
    )
    
    label <- paste(species_points[i], genotype_points[i], sep = " - ")
    
    text(
      x = species_scores[i, 1],
      y = species_scores[i, 4],
      labels = label,
      col = genotype_color,
      pos = 3,
      offset = 0.4, 
      cex = 0.8
    )
  }
  
  ggsave(paste0("fig_output/PCA/PC1_4_no_arrows", ".png"))
  ggsave(paste0("fig_output/PCA/PC1_4_no_arrows", ".pdf"))
  
  # without arrows Plot PC3&4 --------------------------------------------------------------
  
  # Plotting the biplot with colored genotypes
  plot.new()
  plot.window(xlim = c(-2, 2), ylim = c(-2, 2), asp = 1)
  axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)
  title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
  title(xlab = paste("PC 3 (", round(importance_percent[3], digits = 1), "%)", sep = ""),
        ylab = paste("PC 4 (", round(importance_percent[4], digits = 1), "%)", sep = ""),
        line = 2, adj = 0.5)
  # Add rotated axes
  par(new = TRUE, las = 1)
  plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
  axis(side = 3, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  axis(side = 4, at = c(-1, -0.75, -0.5, 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
  # Add labels for rotated axes
  mtext(text = "PC 3 rotations", side = 3, cex = 1, font =  2, family = "sans", col = "gray10", line = 2)
  mtext(text = "PC 4 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)
  
  box()
  abline(v = 0, h = 0, lty = 2, col = "grey25")
  
  # Add species points to the biplot with colored genotypes
  species_points <- geno_table$Species
  genotype_points <- geno_table$Genotype
  species_scores <- predict(pc, geno_table_scaled)
  
  # Create a color palette for species points
  colors <- rainbow(length(species_points))
  
  for (i in seq_along(species_points)) {
    # Determine the color based on genotype
    genotype_color <- genotype_colors[i]
    
    points(
      x = species_scores[i, 3],
      y = species_scores[i, 4],
      col = genotype_color,
      pch = 16,
      cex = 0.8
    )
    
    label <- paste(species_points[i], genotype_points[i], sep = " - ")
    
    text(
      x = species_scores[i, 3],
      y = species_scores[i, 4],
      labels = label,
      col = genotype_color,
      pos = 3,
      offset = 0.4, 
      cex = 0.8
    )
  }
  
  ggsave(paste0("fig_output/PCA/PC3_4_no_arrows", ".png"))
  ggsave(paste0("fig_output/PCA/PC3_4_no_arrows", ".pdf"))