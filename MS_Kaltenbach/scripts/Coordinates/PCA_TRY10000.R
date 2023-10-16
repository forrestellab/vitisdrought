drought_df <- drought_df %>%
  select(-c("Treatment", "mean_value_root_biomass.wu", "mean_value_TotalLeafArea",                           "mean_value_canopy_biomass", "mean_value_root_biomass","mean_value_LWP",  "mean_value_PD",                   
            "mean_value_cumWU",  "relative_water_saving", "mean_value_gsw",  "mean_value_A" ,                     "mean_value_E", "mean_value_intrWUE_AE" , "mean_value_instWUE_AGs", "mean_value_leafarea.wu"                        , "mean_value_canopy_biomass.wu"   ))

drought_df<- drought_df %>%
  select(-c("Lat", "Long", "Annual.Mean.Temperature","Annual.Precipitation",
            "Temperature.Seasonality..standard.deviation..100.", "Mean.Temperature.of.Coldest.Quarter" ))
colnames(drought_df)
# Scale the geno_table data
geno_table_scaled <- scale(drought_df[, -c(1:4)]) 

sum(is.na(geno_table_scaled))

# Remove rows with missing values
geno_table_scaled <- geno_table_scaled[complete.cases(geno_table_scaled), ]

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
genotype_colors <- rep("grey70", length(drought_df$Genotype))
genotype_colors[drought_df$Genotype %in% color_genotypes] <- "#F55C7A"

highlighted_arrows <- c(
  "Annual Mean Temperature",
  "Annual Precipitation",
  "Temp. Seasonality (SD *100)",
  "Mean Temp. of Coldest Quarter",
  "Mean Temp. of Warmest Quarter"
)


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
plot.window(xlim = c(-3, 3), ylim = c(-0.5, 0.5), asp = 1)
axis(side = 3, at = c(-1, -0.75, -0., 0.25, 0, -0.25, 0.5, 0.75, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
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

species_points <- drought_df$Species
genotype_points <- drought_df$Genotype
species_scores <- predict(pc, geno_table_scaled)

# Create a color palette for species points
colors <- rainbow(length(species_points))
for (i in seq_along(species_points)) {
  genotype_color <- genotype_colors[i]
  
  points(
    x = species_scores[i, 1],
    y = species_scores[i, 2],
    col = genotype_color,
    pch = 16,
    cex = 0.8
  )
  
  if (genotype_color == "#F55C7A") {
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
}
