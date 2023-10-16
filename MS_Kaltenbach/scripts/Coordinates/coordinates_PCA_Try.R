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

geno_table[,cols_to_divide] <- geno_table[,cols_to_divide] / 10



climate_data <- geno_table[, 6:24]
climate_data <- climate_data[complete.cases(climate_data), ]


climate_data_std <- scale(climate_data)

pca <- prcomp(climate_data_std, scale = FALSE)

# Plot biplot
biplot(pca, scale = 0)

biplot <- fviz_pca_biplot(pca,
                label="var",
                col.var = "black",
                habillage = geno_table$Species)

biplot + geom_text(size = 3)


biplot + geom_label_repel(aes(label = var.label), size = 3)


require(stats)
pc <- prcomp(x = climate_data_std[ , -1],
             center = TRUE, 
             scale. = FALSE)
print(pc)
summary(pc)

require(graphics)
par(pty = "s",
    cex.main = 1.2,
    cex.lab = 1,
    font.main = 2,
    font.lab = 2,
    family = "sans",
    col.main = "gray10",
    col.lab = "gray10",
    fg = "gray10",
    las = 1)

plot.new()
plot.window(xlim = c(-4, 4), 
            ylim = c(-4, 4), 
            asp = 1)

axis(side = 1, 
     at = c(-4, -2, 0, 2, 4),
     labels = TRUE)
axis(side = 2, 
     at = c(-4, -2, 0, 2, 4),
     labels = TRUE)
title(main = "Biplot for PCs of bank note data", 
      line = 3,
      adj = 0)
title(xlab = paste("PC 1 (", 
                   round(summary(pc)$importance[2]*100, 
                         digits = 1),
                   "%)", 
                   sep = ""), 
      ylab = paste("PC 2 (", 
                   round(summary(pc)$importance[5]*100, 
                         digits = 1),
                   "%)", 
                   sep = ""), 
      line = 2,
      adj = 0.5)

par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), 
            ylim = c(-1, 1), 
            asp = 1)
axis(side = 3, 
     at = c(-1, 0.5, 0, -0.5, 1), 
     labels = TRUE, 
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")
axis(side = 4, 
     at = c(-1, 0.5, 0, -0.5, 1), 
     labels = TRUE, 
     col = "navy",
     col.ticks = NULL,
     lwd = 2,
     col.axis = "navy")


mtext((text = "PC 1 rotations"), 
      side = 3,
      cex = 1,
      font = 2,
      family = "sans",
      col = "gray10", 
      line = 2) 
mtext((text = "PC 2 rotations"), 
      side = 4,
      cex = 1,
      font = 2,
      family = "sans",
      col = "gray10", 
      line = 2,
      las = 3)

box()
abline(v = 0, h = 0, lty = 2, col = "grey25")

arrows(x0 = 0, x1 = pc$rotation[,1], 
       y0 = 0, y1 = pc$rotation[,2], 
       col = "navy", 
       length = 0.08, 
       lwd = 2,
       angle = 30)


colnames(geno_table)

colored_variables <- (coordinates%>%
                  filter(Genotype %in% c("9018", "T52", "b40-14", "b42-34", 
                                         "T48", "NY1", "TXNM0821", "Vru42", "V60-96" )))
# Perform PCA on climate data
pca_result <- prcomp(geno_table[, 6:24], scale. = TRUE)

# Extract the scores for each genotype
genotype_scores <- data.frame(Genotype = geno_table$Genotype, pca_result$x)

# Identify the genotypes to highlight and grey out
highlighted_genotypes <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")

# Assign colors to genotypes
genotype_colors <- ifelse(genotype_scores$Genotype %in% highlighted_genotypes, "red", "grey")

# Plot the PCA biplot
biplot(pca_result, main = "Climate PCA Biplot")

# Add genotype scores as points on the biplot
points(pca_result$x[, 1], pca_result$x[, 2], col = genotype_colors)

# Label the highlighted genotypes
highlighted_indices <- which(genotype_scores$Genotype %in% highlighted_genotypes)
text(pca_result$x[highlighted_indices, 1], pca_result$x[highlighted_indices, 2],
     labels = genotype_scores$Genotype[highlighted_indices], col = "red")


# Perform PCA on climate data
pca_result <- prcomp(geno_table[, 6:24], scale. = TRUE)

# Extract the scores for each genotype
genotype_scores <- data.frame(Genotype = geno_table$Genotype, pca_result$x)

# Identify the genotypes to highlight and grey out
highlighted_genotypes <- c("9018", "T52", "b40-14", "b42-34", "T48", "NY1", "TXNM0821", "Vru42", "V60-96")

# Assign colors and text options to genotypes
genotype_colors <- ifelse(genotype_scores$Genotype %in% highlighted_genotypes, "red", "grey")
genotype_text_colors <- ifelse(genotype_scores$Genotype %in% highlighted_genotypes, "red", "black")
genotype_text_size <- ifelse(genotype_scores$Genotype %in% highlighted_genotypes, 1, 0.5) # Adjust the text size as desired

# Plot the PCA biplot
biplot(pca_result, main = "Climate PCA Biplot")

# Add genotype scores as points on the biplot
points(pca_result$x[, 1], pca_result$x[, 2], col = genotype_colors)

# Label the highlighted genotypes
highlighted_indices <- which(genotype_scores$Genotype %in% highlighted_genotypes)
text(pca_result$x[highlighted_indices, 0,5], pca_result$x[highlighted_indices, 2],
     labels = genotype_scores$Genotype[highlighted_indices], col = genotype_text_colors, cex = genotype_text_size)


env_data <- geno_table[, -(1:5)]
pca_result <- PCA(env_data, graph = FALSE)
biplot(pca_result$ind$coord, pca_result$var$coord, cex = 0.8, arrow.col = "blue")



env_data <- geno_table[, -(1:5)]

pca_result <- PCA(env_data, graph = FALSE)  # Perform PCA without displaying the graph initially

biplot(pca_result, cex = 0.8, arrow.col = "blue", choix = "var")

fviz_pca_biplot(pca_result, habillage = geno_table$Species, label = "none")

corr_circle <- factoextra::fviz_pca_var(pca_result, axes = c(1, 2), col.var = "black", alpha.var = 0.5)
add_conf_ellipse(pca_result, ax = c(1, 2), col = "red")


env_data <- geno_table[, -(1:5)]
pca_result <- prcomp(env_data, scale. = TRUE)
biplot(pca_result, cex = 0.8, arrow.col = "black", choices = c(1,2))
fviz_pca_biplot(pca_result, habillage = geno_table$Species)


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



