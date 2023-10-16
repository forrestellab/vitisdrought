require(stats)
require(graphics)

# Perform Principal Component Analysis
pc <- prcomp(x = climate_data_std[, -1], center = TRUE, scale. = FALSE)

# Set graphical parameters
par(pty = "s", cex.main = 1.2, cex.lab = 1, font.main = 2, font.lab = 2, family = "sans",
    col.main = "gray10", col.lab = "gray10", fg = "gray10", las = 1)

# Create the plot
plot.new()
plot.window(xlim = c(-4, 4), ylim = c(-4, 4), asp = 1)

# Add axes
axis(side = 1, at = c(-4, -2, 0, 2, 4), labels = TRUE)
axis(side = 2, at = c(-4, -2, 0, 2, 4), labels = TRUE)

# Add title
title(main = "Biplot for Principal Components of Climate Data", line = 3, adj = 0)
title(xlab = paste("PC 1 (", round(summary(pc)$importance[2] * 100, digits = 1), "%)", sep = ""),
      ylab = paste("PC 2 (", round(summary(pc)$importance[5] * 100, digits = 1), "%)", sep = ""),
      line = 2, adj = 0.5)

# Add rotated axes
par(new = TRUE, las = 1)
plot.window(xlim = c(-1, 1), ylim = c(-1, 1), asp = 1)
axis(side = 3, at = c(-1, 0.5, 0, -0.5, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")
axis(side = 4, at = c(-1, 0.5, 0, -0.5, 1), labels = TRUE, col = "navy", col.ticks = NULL, lwd = 2, col.axis = "navy")

# Add labels for rotated axes
mtext(text = "PC 1 rotations", side = 3, cex = 1, font = 2, family = "sans", col = "gray10", line = 2)
mtext(text = "PC 2 rotations", side = 4, cex = 1, font = 2, family = "sans", col = "gray10", line = 2, las = 3)

# Add a box, reference lines, and arrows
box()
abline(v = 0, h = 0, lty = 2, col = "grey25")
arrows(x0 = 0, x1 = pc$rotation[, 1], y0 = 0, y1 = pc$rotation[, 2], col = "navy", length = 0.08, lwd = 2, angle = 30)


text(x = pc$rotation[,1], y = pc$rotation[,2], 
     labels = row.names(pc$rotation), 
     cex = 1.2,
     font = 2,
     col = "gray10")


# Plotting text
text(
  x = pc$rotation[, 1],
  y = pc$rotation[, 2],
  labels = row.names(pc$rotation),
  cex = 1.2,
  font = 2,
  col = "navy",
  pos = c(4, 3, 2, 1, 3, 1)
)

# Creating a unit circle
ucircle <- cbind(
  cos((0:360) / 180 * pi),
  sin((0:360) / 180 * pi)
)
polygon(
  ucircle,
  lty = "solid",
  border = "gray10",
  lwd = 1
)

