# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)

# Load the dataset (assuming your dataset is stored in a variable called 'accidents_data')
# Replace 'your_dataset.csv' with the actual file path or dataset name
accidents_data <- read.csv('US_accidents_EDA.csv')

# Create a binary variable indicating the presence of an amenity
accidents_data$Amenity_binary <- ifelse(accidents_data$Amenity == 'False', 0, 1)
accidents_data$Amenity_binary <- as.factor(accidents_data$Amenity_binary)

# Split the dataset into training and testing sets
set.seed(123)
split_index <- sample(nrow(accidents_data), size = 4000)
train_data <- accidents_data[split_index, ]
test_data <- accidents_data[-split_index, ]

# Train a predictive model (using logistic regression in this case)
selected_features <- c('Severity', 'Pressure.in.', 'Visibility.mi.', 'Wind_Direction','Temperature.F.')
model <- glm(Amenity_binary ~ ., data = train_data[, c('Amenity_binary', selected_features)], family = "binomial",maxit = 1000)

# Make predictions on the test set
predictions <- predict(model, newdata = test_data[, selected_features], type = "response")

# Convert probabilities to binary predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)
binary_predictions <- as.factor(binary_predictions)

# Evaluate the model
binary_predictions <- as.factor(binary_predictions)
confusion_matrix <- confusionMatrix(binary_predictions, test_data$Amenity_binary)
print(confusion_matrix)

print(typeof(binary_predictions))
print(typeof(test_data$Amenity_binary))
print(binary_predictions)
print(test_data$Amenity_binary)
