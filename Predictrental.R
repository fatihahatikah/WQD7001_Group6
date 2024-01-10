# Load necessary libraries
library(caret)
library(randomForest)
library(e1071)
library(Metrics)

# Read the dataset
df <- read.csv("/Users/fatihahatikah/Documents/Principles of Data Science/Assignments/rentkl_clean.csv")

# Set seed for reproducibility
set.seed(123)

# Split the data into training and test sets
split <- createDataPartition(df$RentalPerMth, p = 0.80, list = FALSE)
trainSet <- df[split, ]
testSet <- df[-split, ]

# Adjust factor levels for the test set to match the training set
# First, combine the levels from both training and test sets
all_levels <- unique(c(levels(trainSet$PropertyName), levels(testSet$PropertyName)))

# Then, set the levels of the factor in both training and test sets to include all possible levels
trainSet$PropertyName <- factor(trainSet$PropertyName, levels = all_levels)
testSet$PropertyName <- factor(testSet$PropertyName, levels = all_levels)

# Identify the factor with only one level
for (var in names(trainSet)) {
  if (is.factor(trainSet[[var]])) {
    if (length(levels(trainSet[[var]])) < 2) {
      cat("Variable", var, "has only", length(levels(trainSet[[var]])), "level(s) in the training set.\n")
    }
  }
}

# Based on the output, you can decide how to handle the variable.
# Let's say the problematic variable is 'PropertyName'

# Option 1: Remove the variable if it's not important
trainSet$PropertyName <- NULL
testSet$PropertyName <- NULL


# Train the models with the updated training set
# Linear Regression
lm_model <- lm(RentalPerMth ~ ., data = trainSet)

# Random Forest Regression
rf_model <- randomForest(RentalPerMth ~ ., data = trainSet)

# SVM Regression
svm_model <- svm(RentalPerMth ~ ., data = trainSet)

# Make predictions and evaluate the models
# Linear Regression Model Evaluation
lm_predictions <- predict(lm_model, newdata = testSet)
lm_mse <- mse(testSet$RentalPerMth, lm_predictions)
lm_rmse <- sqrt(lm_mse)

# Random Forest Regression Model Evaluation
rf_predictions <- predict(rf_model, newdata = testSet)
rf_mse <- mse(testSet$RentalPerMth, rf_predictions)
rf_rmse <- sqrt(rf_mse)

# SVM Regression Model Evaluation
svm_predictions <- predict(svm_model, newdata = testSet)
svm_mse <- mse(testSet$RentalPerMth, svm_predictions)
svm_rmse <- sqrt(svm_mse)

# Print the evaluation metrics for each model
cat("Linear Regression MSE:", lm_mse, "RMSE:", lm_rmse, "\n")
cat("Random Forest MSE:", rf_mse, "RMSE:", rf_rmse, "\n")
cat("SVM MSE:", svm_mse, "RMSE:", svm_rmse, "\n")



#Fill up the form in the excel Sample Data.csv <3

#sample= read.csv("C:/Users/nkuk/Documents/sampleData.csv")
#svm_predictions <- predict(svm_model, newdata = sample)
#lm_predictions<- predict(lm_model, newdata=sample)
#rf_predictions <- predict(rf_model, newdata = sample)


#svm_mse <- mse(testSet$RentalPerMth, svm_predictions)
#svm_rmse <- sqrt(svm_mse)
#lm_mse <- mse(testSet$RentalPerMth, lm_predictions)
#lm_rmse <- sqrt(lm_mse)
#rf_mse <- mse(testSet$RentalPerMth, rf_predictions)
#rf_rmse <- sqrt(rf_mse)
#cat("Linear Regression MSE:", lm_mse, "RMSE:", lm_rmse, "\n")
#cat("Random Forest MSE:", rf_mse, "RMSE:", rf_rmse, "\n")
#cat("SVM MSE:", svm_mse, "RMSE:", svm_rmse, "\n")
