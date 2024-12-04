# --------------------------------------------------------------------------------
# Script for Univariate and Multivariate Analysis of the Swiss Dataset
# Assignment: Numerical Analysis using R
# --------------------------------------------------------------------------------

# Load Necessary Libraries ---------------------------------------------------
required_packages <- c("ggplot2", "corrplot", "dplyr", "skimr", "gridExtra", "GGally", "broom")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Load the Dataset -----------------------------------------------------------
data("swiss")  # Load the Swiss dataset
swiss_data <- swiss

# --------------------------------------------------------------------------------
# 1. Data Overview
# --------------------------------------------------------------------------------
cat("\n### 1. Data Overview ###\n")
cat("Structure of the Swiss Dataset:\n")
str(swiss_data)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(swiss_data))

# --------------------------------------------------------------------------------
# 2. Summary Statistics
# --------------------------------------------------------------------------------
cat("\n### 2. Summary Statistics ###\n")
numeric_columns <- names(swiss_data)
for (column_name in numeric_columns) {
  column_data <- swiss_data[[column_name]]
  cat("\nStatistics for column:", column_name, "\n")
  cat("  Min:     ", min(column_data, na.rm = TRUE), "\n")
  cat("  Max:     ", max(column_data, na.rm = TRUE), "\n")
  cat("  Mean:    ", mean(column_data, na.rm = TRUE), "\n")
  cat("  Median:  ", median(column_data, na.rm = TRUE), "\n")
  cat("  Std Dev: ", sd(column_data, na.rm = TRUE), "\n")
}

print(summary(swiss_data))
cat("\nDetailed Summary (skimr):\n")
skim(swiss_data)

# --------------------------------------------------------------------------------
# 3. Distribution Visualization
# --------------------------------------------------------------------------------
cat("\n### 3. Distribution Visualization ###\n")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust layout and margins
for (column in numeric_columns) {
  # Boxplot
  boxplot(swiss_data[[column]], 
          main = paste("Box Plot of", column), 
          col = 'skyblue', 
          horizontal = TRUE)
  # Histogram
  hist(swiss_data[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'skyblue', 
       border = 'black', 
       breaks = 30)
}
par(mfrow = c(1, 1))  # Reset layout after plotting

# Create a bar plot for the 'Education' variable
ggplot(swiss, aes(x = factor(Education))) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Education Levels", 
       x = "Education Level", 
       y = "Count") +
  theme_minimal()

# --------------------------------------------------------------------------------
# 4. Scatter Plot Matrix
# --------------------------------------------------------------------------------
cat("\n### 4. Scatter Plot Matrix ###\n")
ggpairs(swiss_data)

# --------------------------------------------------------------------------------
# 5. Correlation Analysis
# --------------------------------------------------------------------------------
cat("\n### 5. Correlation Analysis ###\n")
cor_matrix <- cor(swiss_data, use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, cl.cex = 0.8, 
         title = "Correlation Matrix of Swiss Dataset")

# --------------------------------------------------------------------------------
# 6. Scatter Plot with Trend Line
# --------------------------------------------------------------------------------
cat("\n### 6. Scatter Plot with Trend Line ###\n")
ggplot(swiss_data, aes(x = Education, y = Fertility)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between Education and Fertility",
       x = "Education (%)", y = "Fertility (Births per 1000 Women)") +
  theme_minimal()

# --------------------------------------------------------------------------------
# 7. Multiple Regression
# --------------------------------------------------------------------------------
cat("\n### 7. Multiple Regression ###\n")
model <- lm(Fertility ~ Education + Agriculture + Catholic + Infant.Mortality, data = swiss_data)
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
pca <- prcomp(swiss_data, scale. = TRUE)
explained_variance <- summary(pca)$importance[2, ]
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(explained_variance)

screeplot(pca, type = "lines", main = "Scree Plot")

# Perform PCA
pca <- prcomp(swiss_data, scale. = TRUE)

# Extract eigenvalues (variances) and cumulative variance
eigenvalues <- pca$sdev^2
explained_variance <- eigenvalues / sum(eigenvalues) * 100
cumulative_variance <- cumsum(explained_variance)

# Adjust margins to fit the legend at the top
par(mar = c(5, 4, 4, 4) + 0.1)

# Plot cumulative variance as bars
bar_positions <- barplot(cumulative_variance, col = "skyblue", border = NA, ylim = c(0, 100), 
                         names.arg = 1:length(cumulative_variance), xlab = "Principal Component", 
                         ylab = "Cumulative Variance (%)", main = "Scree Plot and Cumulative Variance")

# Overlay the scree plot (eigenvalues)
par(new = TRUE)
plot(1:length(eigenvalues), eigenvalues, type = "b", pch = 16, col = "orange", lwd = 2, 
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(eigenvalues)))
axis(4, at = seq(0, round(max(eigenvalues), 1), by = round(max(eigenvalues) / 5, 1)), 
     col = "orange", col.axis = "orange", las = 2)
mtext("Eigenvalues", side = 4, line = 3, col = "orange")

# Add legend outside the plot at the top
legend("top", inset = c(0, -0.15), legend = c("Eigenvalues (Line)", "Cumulative Variance (Bar)"), 
       col = c("orange", "skyblue"), pch = c(16, NA), lty = c(1, NA), fill = c(NA, "skyblue"), 
       bty = "n", horiz = TRUE)

# --------------------------------------------------------------------------------
# 10. PCA Interpretation
# --------------------------------------------------------------------------------
cat("\n### 10. PCA Interpretation ###\n")
plot(pca$x[, 1], pca$x[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Biplot of the Swiss Dataset", 
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "green",
     xlim = c(-4, max(pca$x[, 1])))  # Set PC1 axis to start from -4)
arrows(0, 0, pca$rotation[, 1] * max(pca$x[, 1]), pca$rotation[, 2] * max(pca$x[, 2]), 
       col = "red", length = 0.1)
text(pca$rotation[, 1] * max(pca$x[, 1]) * 1.1, pca$rotation[, 2] * max(pca$x[, 2]) * 1.1, 
     labels = colnames(swiss_data), col = "red", cex = 1.2)

cat("\n### PCA Loadings ###\n")
loadings <- pca$rotation  # Extract the PCA loadings
loadings_table <- as.data.frame(loadings)  # Convert to a data frame for better readability
print(loadings_table[0:2])  # Print the table
