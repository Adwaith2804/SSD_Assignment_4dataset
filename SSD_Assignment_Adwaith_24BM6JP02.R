#|___________________SSD Assignment_________________________________|
#All questions are converted into functions.
#For each dataset ,these functions are called to answer respective questions
#Program structure 
#First all functions are built
#second, all dataset loaded
#Third ,seperate Analysis of each dataset .
#___________________________________________________________________|



# Function for Q1: Data Overview
data_overview <- function(data) {
  cat("Dataset Overview:\n")
  str(data)
  cat("Number of observations:", nrow(data), "\n")
  cat("Number of variables:", ncol(data), "\n")
}

# Function for Q2: Summary Statistics
summary_statistics <- function(data, column) {
  if (column %in% colnames(data)) {
    sts <- summary(data[[column]])
    sdev <- sd(data[[column]])
    cat("Summary Statistics for", column, ":\n")
    cat("Mean:", sts["Mean"], "\nMedian:", sts["Median"], "\nSD:", sdev, "\n")
    cat("Min:", sts["Min."], "\nMax:", sts["Max."], "\n")
  } else {
    cat("Column", column, "not found in dataset.\n")
  }
}

# Function for Q3: Distribution Visualization
distribution_visualization <- function(data, column, group_column = NULL) {
  if (column %in% colnames(data)) {
    hist(data[[column]], 
         main=paste("Histogram of", column), 
         xlab=column, col="skyblue", breaks=10)
    boxplot(data[[column]], range=1.5,
            main=paste("Boxplot of", column), 
            ylab=column, col="orange")
  } else {
    cat("Column", column, "not found in dataset.\n")
  }
  
  if (!is.null(group_column) && group_column %in% colnames(data)) {
    barplot(table(data[[group_column]]), 
            main=paste("Distribution of", group_column), 
            xlab=group_column, ylab="Frequency", col="lightgreen")
  } else if (!is.null(group_column)) {
    cat("Column", group_column, "not found in dataset.\n")
  }
}
# Function for Q5: Correlation Analysis
correlation_analysis <- function(data, column1, column2) {
  if (column1 %in% colnames(data) && column2 %in% colnames(data)) {
    cor_val <- cor(data[[column1]], data[[column2]])
    cat("Pearson Correlation Coefficient (", column1, "vs", column2, "):", cor_val, "\n")
  } else {
    cat("One or both columns not found in dataset.\n")
  }
}

# Function for Q6: Scatter Plot Visualization
scatter_plot <- function(data, x_col, y_col) {
  if (x_col %in% colnames(data) && y_col %in% colnames(data)) {
    plot(data[[x_col]], data[[y_col]], 
         main=paste("Scatter Plot of", y_col, "vs", x_col), 
         xlab=x_col, ylab=y_col, col="blue", pch=19)
    abline(lm(data[[y_col]] ~ data[[x_col]]), col="red", lwd=2)
  } else {
    cat("One or both columns not found in dataset.\n")
  }
}

# Function for Q7: Multiple Regression
multiple_regression <- function(data, formula) {
  if (all(all.vars(formula) %in% colnames(data))) {
    model <- lm(formula, data=data)
    cat("Linear Regression Summary:\n")
    print(summary(model))
    return(model)
  } else {
    cat("Some variables in the formula are not present in the dataset.\n")
  }
}

# Function for Q8: Model Diagnostics
model_diagnostics <- function(model) {
  if (!is.null(model)) {
    par(mfrow=c(2, 2))
    plot(model)
  } else {
    cat("Model is null. Cannot perform diagnostics.\n")
  }
}

# Function for Q9: Principal Component Analysis (PCA)
perform_pca <- function(data) {
  numeric_cols <- data[, sapply(data, is.numeric)]
  if (ncol(numeric_cols) > 1) {
    pca <- prcomp(numeric_cols, scale=TRUE)
    plot(pca, type="lines", main="Scree Plot")
    return(pca)
  } else {
    cat("Not enough numeric columns for PCA.\n")
  }
}

# Function for Q10: PCA Interpretation
pca_interpretation <- function(pca) {
  if (!is.null(pca)) {
    par(mfrow=c(1, 1), mar=c(1, 1, 1, 1))
    biplot(pca, main="Biplot of PCA")
  } else {
    cat("PCA object is null. Cannot create biplot.\n")
  }
}
#load all data
# Load datasets
data("LifeCycleSavings")
data("iris")
data("airquality")
data("mtcars")

# Analyze mtcar dataset
#functions called in the same order of questions
par(mar = c(4, 4, 4, 4))
cat("\n=== Analysis for mtcar Dataset ===\n")
data_overview(mtcars)
summary_statistics(mtcars, "mpg")
distribution_visualization(mtcars, "mpg", "cyl")
correlation_analysis(mtcars, "mpg", "hp")
scatter_plot(mtcars, "hp", "mpg")
model <- multiple_regression(mtcars, mpg ~ hp + wt)
model_diagnostics(model)
pca <- perform_pca(mtcars)
pca_interpretation(pca)

par(mar = c(4, 4, 4, 4))

# Analyze iris dataset
cat("\n=== Analysis for Iris Dataset ===\n")
data_overview(iris)
summary_statistics(iris, "Sepal.Length")
distribution_visualization(iris, "Sepal.Length", "Species")
correlation_analysis(iris, "Sepal.Length", "Petal.Length")
scatter_plot(iris, "Petal.Length", "Sepal.Length")
model <- multiple_regression(iris,Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width)
model_diagnostics(model)
pca_iris <- perform_pca(iris[, -5]) # Exclude non-numeric column "Species"
pca_interpretation(pca_iris)



# Analyze airquality dataset
par(mar = c(4, 4, 4, 4))
cat("\n=== Analysis for AirQuality Dataset ===\n")
data_overview(airquality)

#mean imputation to remove "na" in those variable under study.(manually observed from data overview)
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE)
airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)

data_overview(airquality)
summary_statistics(airquality, "Ozone")
distribution_visualization(airquality, "Ozone", "Month")
correlation_analysis(airquality, "Ozone", "Temp")
scatter_plot(airquality, "Temp", "Ozone")
model_airquality <- multiple_regression(airquality, Ozone ~ Temp + Wind + Solar.R)
model_diagnostics(model_airquality)
pca_airquality <- perform_pca(na.omit(airquality)) # Remove rows with NA for PCA
pca_interpretation(pca_airquality)

#Analyze LifeSavingCycle dataset
# Add a new column to classify nations based on savings ratio
#Categorical data derived
LifeCycleSavings$SavingCategory <- cut(
  LifeCycleSavings$sr,
  breaks = c(-Inf, 5, 10, 15, 20, Inf),  # Define the ranges explicitly
  labels = c("Very Low", "Low", "Medium", "High", "Very High"),
  include.lowest = TRUE
)

# Display the first few rows to verify
head(LifeCycleSavings)

par(mar = c(4, 4, 4, 4))
cat("\n=== Analysis for LifeCycleSavings Dataset ===\n")
data_overview(LifeCycleSavings)
summary_statistics(LifeCycleSavings, "sr")
distribution_visualization(LifeCycleSavings, "sr", "SavingCategory")
correlation_analysis(LifeCycleSavings, "sr", "dpi")
scatter_plot(LifeCycleSavings, "dpi", "sr")
model <- multiple_regression(LifeCycleSavings, sr ~ dpi + pop15+ pop75)

model_diagnostics(model)
pca <- perform_pca(LifeCycleSavings)
pca_interpretation(pca)
