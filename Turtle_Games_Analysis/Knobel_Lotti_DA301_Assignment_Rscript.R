# download necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caret)
library(Metrics)

# Load the CSV file
getwd()
turtle_data <- read.csv("C:/Users/lotti/Downloads/turtle_reviews.csv")

# View the data
str(turtle_data)

summary(turtle_data)

head(turtle_data)

# Rename columns for easier handling & remove unnecessary columns
colnames(turtle_data)
turtle_data <- turtle_data %>%
  select(-language, -platform) %>%
  rename(income = `remuneration..k..`, 
	spending_score = `spending_score..1.100.`)
colnames(turtle_data)

# Check for empty cells
colSums(is.na(turtle_data))

# Check for duplicates
turtle_data[duplicated(turtle_data), ]

# Create function to check for outliers
check_outliers <- function(column_data) {
  q1 <- quantile(column_data, 0.25, na.rm = TRUE)
  q3 <- quantile(column_data, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  return(column_data < lower | column_data > upper)}

# Check for outliers in age
outliers_age <- check_outliers(turtle_data$age)
turtle_data[outliers_age, ]

# Check for outliers in income
outliers_income <- check_outliers(turtle_data$income)
turtle_data[outliers_income, ]

# Check for outliers in spending_score
outliers_spending_score <- check_outliers(turtle_data$spending_score)
turtle_data[outliers_spending_score, ]

# Check for outliers in loyalty_points
outliers_loyalty_points <- check_outliers(turtle_data$loyalty_points)
turtle_data[outliers_loyalty_points, ]

# Basic stats - grouping by gender
turtle_data %>%
  group_by(gender) %>%
  summarise(
    avg_loyalty = mean(loyalty_points, na.rm = TRUE),
    avg_income = mean(income, na.rm = TRUE),
    avg_age = mean(age, na.rm = TRUE))

# Basic stats - grouping by education
turtle_data %>%
  group_by(education) %>%
  summarise(
    avg_loyalty = mean(loyalty_points, na.rm = TRUE),
    count = n())

# Histogram displaying Loyalty Points vs. Customer Count
ggplot(turtle_data, aes(x = loyalty_points)) +
  geom_histogram(binwidth = 50, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Loyalty Points", 
	x = "Loyalty Points", y = "Customer Count")

# Boxplot displaying Gender vs. Loyalty Points
ggplot(turtle_data, aes(x = gender, y = loyalty_points, fill = gender)) +
  geom_boxplot() + labs(title = "Loyalty Points by Gender")

# Scatterplot displaying Income vs. Loyalty Points
ggplot(turtle_data, aes(x = income, y = loyalty_points)) +
  geom_point(alpha = 0.6, color = "darkgreen") +
  labs(title = "Income vs. Loyalty Points",
    x = "Income", y = "Loyalty Points")

# Scatterplot displaying Spending Score vs. Loyalty Points
ggplot(turtle_data, aes(x = spending_score, y = loyalty_points)) +
  geom_point(alpha = 0.6, color = "darkred") +
  labs(title = "Spending Score vs. Loyalty Points",
    x = "Spending Score", y = "Loyalty Points")

# Create Multi linear regression model
turtle_data %>%
  group_by(product) %>%
  summarise(avg_loyalty = mean(loyalty_points, na.rm = TRUE))

model <- lm(loyalty_points ~ age + income + spending_score, data = turtle_data)
summary(model)

plot(model)

# Create Random Forest Model
turtle_data <- turtle_data %>%
  select(-product, -review, -summary)

# Turn gender and education into factors
turtle_data$gender <- as.factor(turtle_data$gender)
turtle_data$education <- as.factor(turtle_data$education)
str(turtle_data)

set.seed(42)

# Split the data into train and test set
trainIndex <- createDataPartition(turtle_data$loyalty_points, 
	p = 0.8, list = FALSE)
train_data <- turtle_data[trainIndex, ]
test_data <- turtle_data[-trainIndex, ]

# Train the model
rf_model <- randomForest(loyalty_points ~ age + income + 
	spending_score + gender + education, data = train_data, 
  ntree = 500, mtry = 3, importance = TRUE)

# Print the model
print(rf_model)

# Predict on the test data
predictions <- predict(rf_model, newdata = test_data)

# Calculate performance metrics
rmse_score <- rmse(test_data$loyalty_points, predictions)
cat("RMSE: ", rmse_score, "\n")

# Compare predicted vs actual loyalty points
comparison <- data.frame(Actual = test_data$loyalty_points, 
	Predicted = predictions)
head(comparison)

# Visualize feature importance
importance(rf_model)
varImpPlot(rf_model)

