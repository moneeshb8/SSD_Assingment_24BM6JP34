# --------------------------------------------------------------------------------
# Script for Univariate and Multivariate Analysis of the AirQuality Dataset
# Assignment: Numerical Analysis using R
# --------------------------------------------------------------------------------

# Load Necessary Libraries ---------------------------------------------------
required_packages <- c("caTools", "plotly", "corrplot", "lattice", "ggplot2", 
                       "dplyr", "MASS", "skimr", "reshape2", "gridExtra", "broom")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Load the Dataset -----------------------------------------------------------
data("airquality")  # Load the AirQuality dataset
air <- na.omit(airquality)  # Remove rows with NA values

# --------------------------------------------------------------------------------
# 1. Data Overview
# --------------------------------------------------------------------------------
cat("\n### 1. Data Overview ###\n")
cat("Structure of the AirQuality Dataset:\n")
str(air)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(air))

# --------------------------------------------------------------------------------
# 2. Summary Statistics
# --------------------------------------------------------------------------------
cat("\n### 2. Summary Statistics ###\n")
numeric_columns <- names(air)[sapply(air, is.numeric)]
for (column_name in numeric_columns) {
  column_data <- air[[column_name]]
  cat("\nStatistics for column:", column_name, "\n")
  cat("  Min:     ", min(column_data, na.rm = TRUE), "\n")
  cat("  Max:     ", max(column_data, na.rm = TRUE), "\n")
  cat("  Mean:    ", mean(column_data, na.rm = TRUE), "\n")
  cat("  Median:  ", median(column_data, na.rm = TRUE), "\n")
  cat("  Std Dev: ", sd(column_data, na.rm = TRUE), "\n")
}
print(summary(air))
cat("\nDetailed Summary (skimr):\n")
skim(air)

# --------------------------------------------------------------------------------
# 3. Distribution Visualization
# --------------------------------------------------------------------------------
cat("\n### 3. Distribution Visualization ###\n")
par(mfrow = c(2, 2))  # Set layout for multiple plots in 2x2 grid
for (column in numeric_columns) {
  # Boxplot
  boxplot(air[[column]], 
          main = paste("Box Plot of", column), 
          col = 'skyblue', 
          horizontal = TRUE)
  # Histogram
  hist(air[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'skyblue', 
       border = 'black', 
       breaks = 30)
}
par(mfrow = c(1, 1))  # Reset layout after plotting

# --------------------------------------------------------------------------------
# 4. Scatter Plot Matrix
# --------------------------------------------------------------------------------
cat("\n### 4. Scatter Plot Matrix ###\n")
splom(air[, numeric_columns], col = 'skyblue', main = "Scatter Plot Matrix for Numeric Features")

# --------------------------------------------------------------------------------
# 5. Correlation Analysis
# --------------------------------------------------------------------------------
cat("\n### 5. Correlation Analysis ###\n")
cor_matrix <- cor(air[, numeric_columns], use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, cl.cex = 0.8, 
         title = "Correlation Matrix of AirQuality Dataset")

# --------------------------------------------------------------------------------
# 6. Scatter Plot with Trend Line
# --------------------------------------------------------------------------------
cat("\n### 6. Scatter Plot with Trend Line ###\n")
ggplot(air, aes(x = Temp, y = Ozone)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between Temperature and Ozone",
       x = "Temperature (F)", y = "Ozone (ppb)") +
  theme_minimal()

# --------------------------------------------------------------------------------
# 7. Multiple Regression
# --------------------------------------------------------------------------------
cat("\n### 7. Multiple Regression ###\n")
model <- lm(Ozone ~ Temp + Solar.R + Wind, data = air)
print(summary(model))

# --------------------------------------------------------------------------------
# 8. Model Diagnostics
# --------------------------------------------------------------------------------
cat("\n### 8. Model Diagnostics ###\n")
model_data <- augment(model)

p1 <- ggplot(model_data, aes(.fitted, .resid)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
  theme_minimal()

p2 <- ggplot(model_data, aes(sample = .std.resid)) +
  stat_qq(color = "darkgreen") +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()

grid.arrange(p1, p2, nrow = 1, top = "Diagnostic Plots for Regression Model")

# --------------------------------------------------------------------------------
# 9. Principal Component Analysis (PCA)
# --------------------------------------------------------------------------------
cat("\n### 9. Principal Component Analysis (PCA) ###\n")
pca <- prcomp(air[, numeric_columns], scale. = TRUE)
explained_variance <- summary(pca)$importance[2, ]
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(explained_variance)
screeplot(pca, type = "lines", main = "Scree Plot")

# --------------------------------------------------------------------------------
# 10. PCA Interpretation
# --------------------------------------------------------------------------------
cat("\n### 10. PCA Interpretation ###\n")
plot(pca$x[, 1], pca$x[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Biplot of the AirQuality Dataset", 
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "green")
arrows(0, 0, pca$rotation[, 1] * max(pca$x[, 1]), pca$rotation[, 2] * max(pca$x[, 2]), 
       col = "red", length = 0.1)
text(pca$rotation[, 1] * max(pca$x[, 1]) * 1.1, pca$rotation[, 2] * max(pca$x[, 2]) * 1.1, 
     labels = colnames(air[, numeric_columns]), col = "red", cex = 1.2)
