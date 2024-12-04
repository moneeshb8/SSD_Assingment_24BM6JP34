# ======================= #
# INSTALL AND LOAD LIBRARIES
# ======================= #

# Install and load required packages
if (!requireNamespace("AER", quietly = TRUE)) install.packages("AER")
if (!requireNamespace("mice", quietly = TRUE)) install.packages("mice")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("missMethods", quietly = TRUE)) install.packages("missMethods")

library(AER)
library(mice)
library(ggplot2)
library(missMethods)

# ======================= #
# PART 1: LOAD AND INSPECT DATA
# ======================= #

# Load the CreditCard dataset
data("CreditCard", package = "AER")

#Round age to integer
CreditCard$age <- round(CreditCard$age)

# Drop rows where age is less than 18
CreditCard <- subset(CreditCard, age >= 18)

# Preview the first few rows
head(CreditCard)

# --------------------------------------------------------------------------------
# 1. Data Overview
# --------------------------------------------------------------------------------
cat("\n### 1. Data Overview ###\n")
cat("Structure of the CreditCard Dataset:\n")
str(CreditCard)
cat("\nDimensions of the Dataset (Rows x Columns):\n")
print(dim(CreditCard))

# --------------------------------------------------------------------------------
# 2. Summary Statistics
# --------------------------------------------------------------------------------
cat("\n### 2. Summary Statistics ###\n")
numeric_columns <- names(CreditCard)[sapply(CreditCard, is.numeric)]
for (column_name in numeric_columns) {
  column_data <- CreditCard[[column_name]]
  cat("\nStatistics for column:", column_name, "\n")
  cat("  Min:     ", min(column_data, na.rm = TRUE), "\n")
  cat("  Max:     ", max(column_data, na.rm = TRUE), "\n")
  cat("  Mean:    ", mean(column_data, na.rm = TRUE), "\n")
  cat("  Median:  ", median(column_data, na.rm = TRUE), "\n")
  cat("  Std Dev: ", sd(column_data, na.rm = TRUE), "\n")
}
print(summary(CreditCard))
cat("\nDetailed Summary (skimr):\n")
skim(CreditCard)

# --------------------------------------------------------------------------------
# 3. Distribution Visualization
# --------------------------------------------------------------------------------
cat("\n### 3. Distribution Visualization ###\n")
par(mfrow = c(2, 2))  # Set layout for multiple plots in 2x2 grid
for (column in numeric_columns) {
  # Boxplot
  boxplot(CreditCard[[column]], 
          main = paste("Box Plot of", column), 
          col = 'skyblue', 
          horizontal = TRUE)
  # Histogram
  hist(CreditCard[[column]], 
       main = paste("Histogram of", column), 
       xlab = column, 
       col = 'skyblue', 
       border = 'black', 
       breaks = 30)
}
par(mfrow = c(1, 1))  # Reset layout after plotting

# Create the bar plot
ggplot(CreditCard, aes(x = card)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Plot of Observations by card", x = "Month", y = "Frequency") +
  theme_minimal()

# --------------------------------------------------------------------------------
# 4. Scatter Plot Matrix
# --------------------------------------------------------------------------------
cat("\n### 4. Scatter Plot Matrix ###\n")
splom(CreditCard[, numeric_columns], col = 'skyblue', main = "Scatter Plot Matrix for Numeric Features")

# --------------------------------------------------------------------------------
# 5. Correlation Analysis
# --------------------------------------------------------------------------------
cat("\n### 5. Correlation Analysis ###\n")
cor_matrix <- cor(CreditCard[, numeric_columns], use = "complete.obs")
print(cor_matrix)
corrplot(cor_matrix, method = "circle", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         tl.cex = 0.8, cl.cex = 0.8, 
         title = "Correlation Matrix of CreditCard Dataset")

# --------------------------------------------------------------------------------
# 6. Scatter Plot with Trend Line
# --------------------------------------------------------------------------------
cat("\n### 6. Scatter Plot with Trend Line ###\n")
ggplot(CreditCard, aes(x = expenditure, y = share)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between expenditure and share",
       x = "expenditure", y = "share") +
  theme_minimal()

# Scatter Plot: Share vs. Income
cat("\n### Scatter Plot with Trend Line: Share vs. Income ###\n")
ggplot(CreditCard, aes(x = income, y = share)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between Income and Share",
       x = "Income", y = "Share") +
  theme_minimal()

# Scatter Plot: Income vs. Expenditure
cat("\n### Scatter Plot with Trend Line: Income vs. Expenditure ###\n")
ggplot(CreditCard, aes(x = income, y = expenditure)) +
  geom_point(color = "blue", alpha = 0.7) +  # Add transparency to handle overlap
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Relationship between Income and Expenditure",
       x = "Income", y = "Expenditure") +
  theme_minimal()

# ======================= #
# PART 7: Multiple Regression
# ======================= #
cat("\n### 7. Multiple Regression ###\n")

# Fit a multiple regression model using CreditCard dataset
# Example model: Predicting expenditure using income, age, and share as predictors
model <- lm(expenditure ~ income + share, data = CreditCard)

# Display the summary of the regression model
print(summary(model))

# ======================= #
# PART 8: Model Diagnostics
# ======================= #
cat("\n### 8. Model Diagnostics ###\n")

# Create diagnostic plots for the fitted model
library(broom)
library(gridExtra)

# Augment the model to get diagnostics
model_data <- augment(model)

# Residuals vs Fitted Plot
p1 <- ggplot(model_data, aes(.fitted, .resid)) +
  geom_point(color = "blue") +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

# Q-Q Plot for Residuals
p2 <- ggplot(model_data, aes(sample = .std.resid)) +
  stat_qq(color = "darkgreen") +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
  theme_minimal()

# Combine the plots in a grid
grid.arrange(p1, p2, nrow = 1, top = "Diagnostic Plots for Regression Model")

# ======================= #
# PART 9: Principal Component Analysis (PCA)
# ======================= #
cat("\n### 9. Principal Component Analysis (PCA) ###\n")

# Select numeric columns for PCA
numeric_columns <- names(CreditCard)[sapply(CreditCard, is.numeric)]

# Perform PCA on numeric columns, scaling the data
pca <- prcomp(CreditCard[, numeric_columns], scale. = TRUE)

# Proportion of variance explained by each component
explained_variance <- summary(pca)$importance[2, ]
cat("Proportion of Variance Explained by Each Principal Component:\n")
print(explained_variance)

# Extract eigenvalues and explained variance
eigenvalues <- pca$sdev^2
explained_variance <- eigenvalues / sum(eigenvalues) * 100
cumulative_variance <- cumsum(explained_variance)

# Scree Plot with Cumulative Variance
par(mfrow = c(1, 1))  # Reset layout
bar_positions <- barplot(cumulative_variance, col = "skyblue", border = NA, ylim = c(0, 100),
                         names.arg = 1:length(cumulative_variance), xlab = "Principal Component",
                         ylab = "Cumulative Variance (%)", main = "Scree Plot with Cumulative Variance")
par(new = TRUE)
plot(1:length(eigenvalues), eigenvalues, type = "b", pch = 16, col = "orange", lwd = 2,
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(eigenvalues)))
axis(4, at = seq(0, round(max(eigenvalues), 1), by = round(max(eigenvalues) / 5, 1)),
     col = "orange", col.axis = "orange", las = 2)
mtext("Eigenvalues", side = 4, line = 3, col = "orange")
legend("top", inset = c(0, -0.15), legend = c("Eigenvalues (Line)", "Cumulative Variance (Bar)"),
       col = c("orange", "skyblue"), pch = c(16, NA), lty = c(1, NA), fill = c(NA, "skyblue"),
       bty = "n", horiz = TRUE)

# ======================= #
# PART 10: PCA Interpretation (Spread Arrows and Labels)
# ======================= #
library(ggplot2)

cat("\n### 10. PCA Interpretation (Spread Arrows and Labels) ###\n")

# Create a PCA biplot using ggplot2
pca_data <- as.data.frame(pca$x[, 1:2])  # Extract the first two PCs
colnames(pca_data) <- c("PC1", "PC2")
pca_data$Category <- CreditCard$card  # Add the categorical variable for coloring

# Normalize the variable loadings to spread arrows and labels
loadings <- as.data.frame(pca$rotation[, 1:2])  # Extract loadings for PC1 and PC2
loadings$Variable <- rownames(loadings)  # Add variable names
scale_factor <- 4  # Scale factor for arrows and labels
loadings$PC1 <- loadings$PC1 * scale_factor
loadings$PC2 <- loadings$PC2 * scale_factor

# Plot the PCA biplot with normalized arrows
ggplot() +
  # Add data points
  geom_point(data = pca_data, aes(x = PC1, y = PC2, color = Category), alpha = 0.7, size = 2) +
  # Add arrows for variable loadings
  geom_segment(data = loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.3, "cm")), color = "red") +
  # Add variable labels spread out
  geom_text(data = loadings, aes(x = PC1 * 1.1, y = PC2 * 1.1, label = Variable), 
            color = "red", size = 4, hjust = 0.5, vjust = 0.5) +
  # Cap the axes
  xlim(-5, 5) +
  ylim(-4, 4) +
  # Add labels and theme
  labs(title = "PCA Biplot of the CreditCard Dataset (Spread Arrows and Labels)",
       x = "Principal Component 1", 
       y = "Principal Component 2", 
       color = "Card Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14), 
        axis.title = element_text(size = 12),
        legend.position = "bottom")

# Print the loadings of PC1 and PC2
cat("\n### Loadings of the First Two Principal Components ###\n")
loadings <- as.data.frame(pca$rotation[, 1:2])  # Extract the first two components
colnames(loadings) <- c("PC1", "PC2")  # Rename columns for clarity
print(loadings)
