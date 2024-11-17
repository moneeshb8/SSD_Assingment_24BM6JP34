# --------------------------------------------------------------------------------
# Script for Univariate and Multivariate Analysis of the Boston Dataset
# Assignment: Numerical Analysis using R
# --------------------------------------------------------------------------------

# Load Necessary Libraries ---------------------------------------------------
required_packages <- c("caTools", "plotly", "corrplot", "lattice", "ggplot2", 
                       "dplyr", "MASS", "skimr", "reshape2", "gridExtra", "broom")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# Load the Dataset -----------------------------------------------------------
housing <- Boston  # Boston dataset from MASS package

# --------------------------------------------------------------------------------
# Univariate Analysis
# --------------------------------------------------------------------------------

# 1. Data Overview
cat("\n### 1. Data Overview ###\n")
cat("Structure of the Boston Dataset:\n")
str(housing)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(housing))

# 2. Summary Statistics
cat("\n### 2. Summary Statistics ###\n")
mean_medv <- mean(housing$medv)
median_medv <- median(housing$medv)
sd_medv <- sd(housing$medv)
min_medv <- min(housing$medv)
max_medv <- max(housing$medv)
cat("Mean:", mean_medv, "\nMedian:", median_medv, "\nStandard Deviation:", sd_medv, 
    "\nMinimum:", min_medv, "\nMaximum:", max_medv, "\n")
print(summary(housing))
cat("\nDetailed Summary (skimr):\n")
skim(housing)

# 3. Distribution Visualization
cat("\n### 3. Distribution Visualization ###\n")
numeric_columns <- names(housing)[sapply(housing, is.numeric)]

# Combined Box Plot and Histogram for Each Feature
par(mfrow = c(1, 1))
for (column in numeric_columns) {
  par(mfrow = c(1, 2))
  boxplot(housing[[column]], 
          main = paste("Box Plot of", column), 
          col = 'skyblue', 
          horizontal = TRUE)
  hist(housing[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'skyblue', 
       border = 'black', 
       breaks = 30)
}

# Scatter Plot Matrix
cat("\n### Scatter Plot Matrix ###\n")
dropList <- c('chas', 'rad', 'black')
housingplot <- housing[, !colnames(housing) %in% dropList]
splom(housingplot, col = 'skyblue', main = "Scatter Plot Matrix for Numeric Features")

# Categorical Variable Analysis
cat("\n### Categorical Variable Analysis ###\n")
categorical_vars <- "chas"
par(mfrow = c(1, 1))
for (cat_var in categorical_vars) {
  barplot(table(housing[[cat_var]]),
          main = paste("Distribution of", cat_var),
          xlab = cat_var, 
          ylab = "Count", 
          col = "lightgreen")
}

# --------------------------------------------------------------------------------
# Multivariate Analysis
# --------------------------------------------------------------------------------

# 5. Correlation Analysis
cat("\n### 5. Correlation Analysis ###\n")
cor_rm_medv <- cor(housing$rm, housing$medv)
cat("Pearson Correlation Coefficient between RM and MEDV:", cor_rm_medv, "\n")

Boston_num <- housing[, sapply(housing, is.numeric)]
cor_matrix <- cor(Boston_num)
cat("Correlation Matrix:\n")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, cl.cex = 0.8, 
         title = "Correlation Matrix of Boston Dataset")

# 6. Scatter Plot with Trend Line
cat("\n### 6. Scatter Plot with Trend Line ###\n")
ggplot(housing, aes(x = rm, y = medv)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relationship between RM and MEDV",
       x = "Average Number of Rooms (RM)", y = "Median Home Value (MEDV)")

# 7. Multiple Regression
cat("\n### 7. Multiple Regression ###\n")
model <- lm(medv ~ rm + lstat, data = housing)
summary(model)

# 8. Model Diagnostics
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
p3 <- ggplot(model_data, aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point(color = "purple") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Scale-Location", x = "Fitted values", y = expression(sqrt("|Standardized residuals|"))) +
  theme_minimal()
p4 <- ggplot(model_data, aes(.hat, .std.resid)) +
  geom_point(color = "red") +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  labs(title = "Residuals vs Leverage", x = "Leverage", y = "Standardized Residuals") +
  theme_minimal()
grid.arrange(p1, p2, p3, p4, nrow = 2, top = "Enhanced Diagnostic Plots for Regression Model")

# --------------------------------------------------------------------------------
# Advanced Analysis
# --------------------------------------------------------------------------------

# 9. Principal Component Analysis (PCA)
cat("\n### 9. Principal Component Analysis (PCA) ###\n")
pca <- prcomp(Boston_num, scale. = TRUE)
explained_variance <- summary(pca)$importance[2, ]
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(explained_variance)
screeplot(pca, type = "lines", main = "Scree Plot")

# 10. PCA Interpretation
cat("\n### 10. PCA Interpretation ###\n")
plot(pca$x[, 1], pca$x[, 2], 
     xlab = "PC1", ylab = "PC2", 
     main = "PCA Biplot of the Boston Dataset", 
     cex.lab = 1.5, cex.axis = 1.2, pch = 16, col = "green")
arrows(0, 0, pca$rotation[, 1] * max(pca$x[, 1]), pca$rotation[, 2] * max(pca$x[, 2]), 
       col = "red", length = 0.1)
text(pca$rotation[, 1] * max(pca$x[, 1]) * 1.1, pca$rotation[, 2] * max(pca$x[, 2]) * 1.1, 
     labels = rownames(pca$rotation), col = "red", cex = 1.2)

# end
