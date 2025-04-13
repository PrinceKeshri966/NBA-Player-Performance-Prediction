# Load necessary libraries
library(e1071)  # For SVM
library(ggplot2)  # For plotting
library(cluster)  # For clustering and plotting clusters

# Load the dataset
nba_data <- read.csv(file.choose())

# Select relevant columns for analysis
nba_data <- nba_data[, c("PTS", "AST", "TRB", "FG.", "X3P.")]

# Convert percentages to numeric format if needed
nba_data$FG. <- as.numeric(as.character(nba_data$FG.))
nba_data$X3P. <- as.numeric(as.character(nba_data$X3P.))

# Remove rows with NA values
nba_data <- na.omit(nba_data)

# 1. Simple Linear Regression
# Predict points (PTS) based on assists (AST)
lm_model <- lm(PTS ~ AST, data = nba_data)
print(summary(lm_model))

# Plotting the linear regression
ggplot(nba_data, aes(x = AST, y = PTS)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Simple Linear Regression", x = "Assists", y = "Points")

# 2. Support Vector Machine (SVM)
# Create a binary label for high vs low scorer based on median points
median_points <- median(nba_data$PTS)
nba_data$scorer_label <- ifelse(nba_data$PTS > median_points, "High", "Low")

# Train the SVM model
# Convert scorer_label to a factor for classification
nba_data$scorer_label <- as.factor(nba_data$scorer_label)

# Train the SVM model for classification
svm_model <- svm(scorer_label ~ AST + TRB + FG. + X3P., 
                 data = nba_data, 
                 kernel = "linear", 
                 scale = TRUE)

# Make predictions using the trained SVM model
svm_predictions <- predict(svm_model, newdata = nba_data)

# Display confusion matrix for predictions
print(table(Predicted = svm_predictions, Actual = nba_data$scorer_label))


# Display SVM results
print(table(Predicted = svm_predictions, Actual = nba_data$scorer_label))

# 3. K-Means Clustering
# Clustering based on AST, TRB, and FG%
kmeans_result <- kmeans(nba_data[, c("AST", "TRB", "FG.")], centers = 3, nstart = 20)

# Add cluster results to the dataset
nba_data$cluster <- as.factor(kmeans_result$cluster)

# Plotting the clusters using AST and TRB
ggplot(nba_data, aes(x = AST, y = TRB, color = cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering", x = "Assists", y = "Rebounds") +
  scale_color_discrete(name = "Cluster")

