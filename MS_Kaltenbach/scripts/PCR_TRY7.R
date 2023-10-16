# Combine climate_data_std and geno_table_scaled
combined_data <- cbind(climate_data_std, geno_table_scaled)

# Perform Principal Component Analysis on the combined data
pc <- prcomp(combined_data, center = TRUE, scale. = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)

# Plotting the biplot
biplot(pc, scale = 0, cex = 0.8)

# Add colored genotype points
species_scores <- predict(pc, geno_table_scaled)
genotype_colors <- rep("grey70", length(geno_table$Genotype))
genotype_colors[geno_table$Genotype %in% color_genotypes] <- "#F55C7A"

for (i in seq_along(species_points)) {
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


# Perform Principal Component Analysis
pc <- prcomp(x = geno_table[, 6:24], center = TRUE, scale. = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)

# Plotting the biplot
biplot(pc, scale = 0, cex = 0.8)

# Add colored genotype points
species_scores <- predict(pc, geno_table[, -c(1:5)])
genotype_colors <- rep("grey70", length(geno_table$Genotype))
genotype_colors[geno_table$Genotype %in% color_genotypes] <- "#F55C7A"

for (i in seq_along(geno_table$Species)) {
  genotype_color <- genotype_colors[i]
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  label <- paste(geno_table$Species[i], geno_table$Genotype[i], sep = " - ")
  
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


# Perform Principal Component Analysis on climate_data
climate_data_std <- scale(climate_data)

pca <- prcomp(climate_data_std, center = TRUE, scale = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)

# Extract PC summary
pc_summary <- summary(pca)
proportion_of_variance <- pc_summary$importance[2, ]
importance_percent <- proportion_of_variance * 100

# Set the genotypes to be colored
color_genotypes <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")

# Create a color palette for genotypes
genotype_colors <- rep("grey70", nrow(geno_table))
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
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
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
arrows(x0 = 0, x1 = pca$rotation[, 1], y0 = 0, y1 = pca$rotation[, 2], col = ifelse(row.names(pca$rotation) %in% highlighted_arrows, "navy", "grey"), length = 0.08, lwd = 2, angle = 30)

text(
  x = pca$rotation[, 1],
  y = pca$rotation[, 2],
  labels = row.names(pca$rotation),
  cex = 0.5,
  font = 2,
  col = ifelse(row.names(pca$rotation) %in% highlighted_arrows, "navy", "grey"),
  pos = c(3, 1, 1, 3, 1)
)

# Add species points to the biplot with colored genotypes
species_points <- geno_table$Species
genotype_points <- geno_table$Genotype
species_scores <- predict(pca, geno_table_scaled)

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

