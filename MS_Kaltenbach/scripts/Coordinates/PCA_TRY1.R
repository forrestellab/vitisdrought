

# Load Required Libraries -------------------------------------------------

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
require(stats)
require(graphics)


# Plot graph --------------------------------------------------------------

climate_data <- geno_table[, 6:24]
climate_data <- climate_data[complete.cases(climate_data), ]


climate_data_std <- scale(climate_data)

# Perform Principal Component Analysis
pc <- prcomp(x = climate_data_std[, -1], center = TRUE, scale. = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)

# Create the plot
plot.new()
plot.window(xlim = c(-5, 5), ylim = c(-10, 10), asp = 1)

# Add axes
axis(side = 1, at = c(-2, -1, 0, 1, 2), labels = TRUE)
axis(side = 2, at = c(-2, -1, 0, 1, 2), labels = TRUE)

# Add title
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 1 (", round(summary(pc)$importance[2] * 100, digits = 1), "%)", sep = ""),
      ylab = paste("PC 2 (", round(summary(pc)$importance[5] * 100, digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)

# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-5, 5), ylim = c(-5, 5), asp = 1)
axis(side = 3, at = c(-0.5, 0.25, 0, -0.25, 0.5), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-01, 0.25, 0, -0.25, 0.5), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")

# Add labels for rotated axes
mtext(text = "PC 1 rotations", side = 3, cex = 1, font = 2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 2 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

# Add a box, reference lines, and arrows
box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 1], y0 = 0, y1 = pc$rotation[, 2], col = "navy", length = 0.08, lwd = 2, angle = 30)


# Plotting text
text(
  x = pc$rotation[, 1],
  y = pc$rotation[, 2],
  labels = row.names(pc$rotation),
  cex = 0.5,
  font = 2,
  col = "navy",
  pos = c(3, 2, 1, 3, 1),
)


# Creating a smaller unit circle
scaling_factor <- 0.54  # Adjust this factor to control the size of the circle
ucircle <- scaling_factor * cbind(
  cos((0:360) / 180 * pi),
  sin((0:360) / 180 * pi)
)

polygon(
  ucircle,
  lty = "solid",
  border = "gray10",
  lwd = 1
)

# Add species points to the biplot
species_points <- geno_table$Species
species_scores <- predict(pc, climate_data_std[, -1])

# Create a color palette for species points
colors <- rainbow(length(species_points))

for (i in seq_along(species_points)) {
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    col = colors[i],
    pch = 16,
    cex = 0.8
  )
  
  text(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    labels = species_points[i],
    col = colors[i],
    pos = 3,
    offset = 0.5
  )
}
