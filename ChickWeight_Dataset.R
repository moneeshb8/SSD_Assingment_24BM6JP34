# --------------------------------------------------------------------------------
# Script for Univariate and Multivariate Analysis of the ChickWeight Dataset
# Assignment: Numerical Analysis using R
# --------------------------------------------------------------------------------

# Load Necessary Libraries ---------------------------------------------------
required_packages <- c("caTools", "plotly", "corrplot", "lattice", "ggplot2", 
                       "dplyr", "MASS", "skimr", "reshape2", "gridExtra", "broom")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Load the Dataset -----------------------------------------------------------
data("ChickWeight")  # ChickWeight dataset from base R
chick <- ChickWeight

# --------------------------------------------------------------------------------
# Univariate Analysis
# --------------------------------------------------------------------------------

# 1. Data Overview
cat("\n### 1. Data Overview ###\n")
cat("Structure of the ChickWeight Dataset:\n")
str(chick)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(chick))

# Separate Numeric and Categorical Columns
numeric_columns <- names(chick)[sapply(chick, is.numeric)]
categorical_columns <- names(chick)[sapply(chick, function(x) is.factor(x) | is.character(x))]

# 2. Summary Statistics
cat("\n### 2. Summary Statistics ###\n")

# Process Numeric Columns
cat("### Summary Statistics for Numeric Columns ###\n")
for (column_name in numeric_columns) {
  column_data <- chick[[column_name]]
  
  # Calculate metrics
  min_value <- min(column_data, na.rm = TRUE)
  max_value <- max(column_data, na.rm = TRUE)
  mean_value <- mean(column_data, na.rm = TRUE)
  median_value <- median(column_data, na.rm = TRUE)
  std_dev <- sd(column_data, na.rm = TRUE)
  
  # Print the metrics for the column
  cat("Statistics for column:", column_name, "\n")
  cat("  Min:     ", min_value, "\n")
  cat("  Max:     ", max_value, "\n")
  cat("  Mean:    ", mean_value, "\n")
  cat("  Median:  ", median_value, "\n")
  cat("  Std Dev: ", std_dev, "\n\n")
}

print(summary(chick))
cat("\nDetailed Summary (skimr):\n")
skim(chick)

# 3. Distribution Visualization
cat("\n### 3. Distribution Visualization ###\n")
numeric_columns <- names(chick)[sapply(chick, is.numeric)]

# Combined Box Plot and Histogram for Each Feature
par(mfrow = c(1, 1))
for (column in numeric_columns) {
  par(mfrow = c(1, 2))
  boxplot(chick[[column]], 
          main = paste("Box Plot of", column), 
          col = 'skyblue', 
          horizontal = TRUE)
  hist(chick[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'skyblue', 
       border = 'black', 
       breaks = 30)
}

# Categorical Variable Analysis
cat("\n### Categorical Variable Analysis ###\n")
categorical_vars <- c("Chick", "Diet")
par(mfrow = c(1, 1))
for (cat_var in categorical_vars) {
  barplot(table(chick[[cat_var]]),
          main = paste("Distribution of", cat_var),
          xlab = cat_var, 
          ylab = "Count", 
          col = "lightgreen")
}

# --------------------------------------------------------------------------------
# Multivariate Analysis
# --------------------------------------------------------------------------------

# 4. Correlation Analysis
cat("\n### 4. Correlation Analysis ###\n")
numeric_data <- chick[, sapply(chick, is.numeric)]
cor_matrix <- cor(numeric_data)
cat("Correlation Matrix:\n")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, cl.cex = 0.8, 
         title = "Correlation Matrix of ChickWeight Dataset")

# 5. Scatter Plot with Trend Line
cat("\n### 5. Scatter Plot with Trend Line ###\n")
ggplot(chick, aes(x = Time, y = weight, color = Diet)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Growth Trend Over Time by Diet",
       x = "Time (Days)", y = "Weight (g)") +
  theme_minimal()

# 6. Multiple Regression
cat("\n### 6. Multiple Regression ###\n")
model <- lm(weight ~ Time + Diet, data = chick)
summary(model)

# 7. Model Diagnostics
cat("\n### 7. Model Diagnostics ###\n")
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
# Advanced Analysis
# --------------------------------------------------------------------------------

# 8. Principal Component Analysis (PCA)
cat("\n### 8. Principal Component Analysis (PCA) ###\n")
pca <- prcomp(numeric_data, scale. = TRUE)
explained_variance <- summary(pca)$importance[2, ]
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(explained_variance)
screeplot(pca, type = "lines", main = "Scree Plot")

# 9. PCA Interpretation
cat("\n### 9. PCA Interpretation ###\n")
plot(pca$x[, 1], pca$x[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Biplot of the ChickWeight Dataset", 
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "green")
arrows(0, 0, pca$rotation[, 1] * max(pca$x[, 1]), pca$rotation[, 2] * max(pca$x[, 2]), 
       col = "red", length = 0.1)
text(pca$rotation[, 1] * max(pca$x[, 1]) * 1.1, pca$rotation[, 2] * max(pca$x[, 2]) * 1.1, 
     labels = rownames(pca$rotation), col = "red", cex = 1.2)

# --------------------------------------------------------------------------------
# END OF SCRIPT
# --------------------------------------------------------------------------------
