# Installing the tidyverse packages required

install.packages(tidyverse)
library(tidyverse)


library(dplyr)


# Read the CSV file with the correct name from the excel worksheet
df <- read.csv('diabetes_data (2).csv')

View(df)
str(df)

# Remove unnecessary columns
df <- df[, !names(df) %in% c("PatientID", "DoctorInCharge", "SocioeconomicStatus", 
                             "Ethnicity", "EducationLevel", "OccupationalExposureChemicals")]

View(df)  


#finding missing values and handling it

missing_values_in_dataset <- df %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "Variable", value = "MissingCount") %>%
  arrange(desc(MissingCount))

# Display the result of missing values
print(missing_values_in_dataset)

View(df)
str(df)

# Remove unnecessary columns
df <- df[, !names(df) %in% c("PatientID", "DoctorInCharge", "SocioeconomicStatus", 
                             "Ethnicity", "EducationLevel", "OccupationalExposureChemicals")]

View(df)  


#finding missing values and handling it

missing_values_in_dataset <- df %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "Variable", value = "MissingCount") %>%
  arrange(desc(MissingCount))

# Display the result of missing values
print(missing_values_in_dataset)

################## Finding the Outliers #################

# Load necessary libraries
library(ggplot2)
library(ggplot2)
library(grid)
library(gridExtra)

# List of specified numeric columns
numeric_columns <- c("Age", "BMI", "AlcoholConsumption", "PhysicalActivity", "DietQuality", 
                     "SleepQuality", "SystolicBP", "DiastolicBP", "FastingBloodSugar", 
                     "HbA1c", "SerumCreatinine", "BUNLevels", "CholesterolTotal", 
                     "CholesterolLDL" , "CholesterolHDL", "CholesterolTriglycerides", 
                     "FatigueLevels", "QualityOfLifeScore", "MedicalCheckupsFrequency", 
                     "MedicationAdherence", "HealthLiteracy")

# Create boxplot for specified numeric columns in the dataset
create_boxplots <- function(df, columns) {
  num_plots <- length(columns)
  plots_per_page <- 9
  pages <- ceiling(num_plots / plots_per_page)
  
  plot_list <- list()
  
  for (i in seq_along(columns)) {
    column <- columns[i]
    plot_list[[i]] <- ggplot(df, aes(x = "", y = .data[[column]])) + 
      geom_boxplot() + 
      labs(title = paste("Boxplot of", column), y = column) + 
      theme(axis.title.x = element_blank(), 
            axis.text.x = element_blank())
  }
  
  # Arrange the plots in a grid and display them page by page
  for (p in seq_len(pages)) {
    start_idx <- (p - 1) * plots_per_page + 1
    end_idx <- min(p * plots_per_page, num_plots)
    grid_plots <- plot_list[start_idx:end_idx]
    
    grid.newpage()
    do.call(grid.arrange, c(grid_plots, ncol = 3))
  }
}


# Call the function with your dataframe and specified numeric columns
create_boxplots(df, numeric_columns)

# Identifying binary columns in the dataset by distribution bar plots

binary_columns <- sapply(df, function(col) all(unique(col) %in% c(0, 1)))
binary_columns <- names(df)[binary_columns]

# Creating bar plots for binary columns 
create_bar_plots <- function(df, columns) {
  num_plots <- length(columns)
  plot_list <- vector("list", num_plots)
  
  for (i in seq_along(columns)) {
    column <- columns[i]
    plot_list[[i]] <- ggplot(df, aes(x = factor(df[[column]]))) + 
      geom_bar(aes(y = ..count..)) + 
      labs(title = paste("Bar Plot of", column), x = column, y = "Count") + 
      theme(axis.title.x = element_blank())
  }
  
  return(plot_list)
}

# Display all plots
display_all_plots <- function(plot_list) {
  num_plots <- length(plot_list)
  plots_per_page <- 9
  pages <- ceiling(num_plots / plots_per_page)
  
  for (p in seq_len(pages)) {
    start_idx <- (p - 1) * plots_per_page + 1
    end_idx <- min(p * plots_per_page, num_plots)
    grid_plots <- plot_list[start_idx:end_idx]
    
    grid.newpage()
    grid.arrange(grobs = grid_plots, ncol = 3)
  }
}

# Create and display bar plots for binary columns
binary_plots <- create_bar_plots(df, binary_columns)
display_all_plots(binary_plots)

View(df)
#########################Finished Until Outliers####################


# Assuming the target variable is named 'Diagnosis'
# and that diabetes is labeled as 'Diabetes' or 1

# Calculate the number of diabetes and non-diabetes cases in dataset

num_diabetes_cases <- sum(new_df$Diagnosis == "Diabetes" | new_df$Diagnosis == 1)
num_non_diabetes_cases <- sum(new_df$Diagnosis != "Diabetes" & new_df$Diagnosis != 1)
total_cases <- nrow(new_df)
percentage_diabetes <- (num_diabetes_cases / total_cases) * 100
print(paste("Percentage of diabetes cases:", round(percentage_diabetes, 2), "%"))

# Create a dataframe for the counts
df_counts <- data.frame(
  Diagnosis = c("Diabetes", "Non-Diabetes"),
  Count = c(num_diabetes_cases, num_non_diabetes_cases)
)

# Pie Chart
ggplot(df_counts, aes(x = "", y = Count, fill = Diagnosis)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Percentage of Diabetes Cases") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Bar Plot
ggplot(df_counts, aes(x = Diagnosis, y = Count, fill = Diagnosis)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Diabetes and Non-Diabetes Cases", x = "Diagnosis", y = "Count") +
  theme_minimal()

View(new_df)
##########################Calculating the diabetes and non diabetes in patients###############

# Create a copy of the dataset
df_copy_2 <- new_df_col
View(new_df_col)
View(df_copy_2)
# Calculate Mean Arterial Pressure (MAP)
df_copy_2 <- df_copy_2 %>%
  mutate(MAPSystolicDiastolic = (1/3) * SystolicBP + (2/3) * DiastolicBP)
View(df_copy_2)

# Remove the original DiastolicBP and SystolicBP columns if they are no longer needed
df_copy_2 <- df_copy_2 %>%
  select(-SystolicBP, -DiastolicBP)

df_copy_2 <- df_copy_2[, !names(df_copy_2) %in% c("Smoking", "AlcoholConsumption", "SleepQuality", 
                                                "FatigueLevels", "HeavyMetalsExposure", "MedicalCheckupsFrequency")]##########
df_copy_2<- df_copy_2[!names(df_copy_2) %in% c("PolycysticOvarySyndrome", "GestationalDiabetes", "PreviousPreDiabetes", 
                                                  "BUNLevels", "AntihypertensiveMedications", "AntidiabeticMedications")]
df_copy_2 <- df_copy_2[, !names(df_copy_2) %in% c("MedicationAdherence", "QualityOfLifeScore", "SlowHealingSores", 
                                                  "Statins", "FamilyHistoryDiabetes", "HealthLiteracy")]

df_copy_2 <- df_copy_2[, !names(df_copy_2) %in% c("SerumCreatinine", "BlurredVision", "CholesterolHDL", 
                                                  "CholesterolTotal", "MedicalCheckupsFrequency","UnexplainedWeightLoss")]
df_copy_1 <- df_copy_1[, !names(df_copy_1) %in% c("MedicalCheckupsFrequency")]





# Load necessary libraries
library(randomForest)
library(caret)
df_copy_2$Diagnosis <- as.factor(df_copy_2$Diagnosis)
set.seed(123)
# Train a Random Forest model to extract feature importance based on Mean Decrease in Gini
rf_model <- randomForest(Diagnosis ~ ., data = df_copy_2, importance = TRUE)
importance <- importance(rf_model)
importance_df <- data.frame(Feature = rownames(importance), 
                            Importance = importance[, "MeanDecreaseGini"])

importance_df <- importance_df[order(-importance_df$Importance), ]

top_features <- head(importance_df, 12)
print("Top 12 Features:")
print(top_features$Feature)

# Subset the dataset to include only the top 12 features plus the Diagnosis column
df_subset_final <- df_copy_2[, c(top_features$Feature, "Diagnosis")]

# View the structure of the reduced dataset
str(df_subset_final)
View(df_subset_final)


# Load necessary library
library(ggplot2)

# Create a bar plot of the top 12 important features
ggplot(top_features, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + 
  labs(title = "Top 12 Important Features from Random Forest Model",
       x = "Feature",
       y = "Mean Decrease in Gini") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), 
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 14, face = "bold"))
###################Correlation Analysis##################

# Calculate the correlation matrix

corr_matrix <- round(cor(df_subset_final[ , sapply(df_subset_final, is.numeric)]), 2)

melted_corr_matrix <- reshape2::melt(corr_matrix)


# Plot the heatmap with enhanced color contrast and correlation values
library(ggplot2)

ggplot(data = melted_corr_matrix, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "black", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") + 
  geom_text(aes(label = value), color = "grey", size = 4) + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) + 
  coord_fixed() +
  labs(title = "Correlation Heatmap", x = "Features", y = "Features")



#################Train Data###########
# Load necessary libraries
library(caret)
library(e1071)
library(ggplot2)
library(pROC)

# Split the data into training and testing sets
set.seed(123)
index <- createDataPartition(df_subset_final$Diagnosis, p = 0.8, list = FALSE)
train_data <- df_subset_final[index, ]
test_data <- df_subset_final[-index, ]

# Number of rows and columns in the train_data and test_data

ncol(train_data)
nrow(train_data)

ncol(test_data)
nrow(test_data)
############################LG Regression################

# Load necessary libraries
library(caret)
library(ggplot2)
library(pROC)
library(rpart)
library(rpart.plot)

# Train the logistic model
logistic_model <- train(Diagnosis ~ ., data = train_data, method = "glm", family = "binomial")

# Predict probabilities on the test data
pred_prob <- predict(logistic_model, test_data, type = "prob")[,2]

# Convert probabilities to class labels with a threshold of 0.5
pred_class <- ifelse(pred_prob > 0.5, "1", "0")

# Check if predictions were made
print(table(pred_class))

# Check the distribution of the actual classes in the test set
print(table(test_data$Diagnosis))

# Create the confusion matrix
conf_matrix_logistic <- confusionMatrix(as.factor(pred_class), as.factor(test_data$Diagnosis))

# Print the confusion matrix to verify results
print(conf_matrix_logistic$table)

# Extracting Accuracy, Precision, Recall, and F1-Score for Logistic Regression
accuracy_logistic <- conf_matrix_logistic$overall['Accuracy']
precision_logistic <- conf_matrix_logistic$byClass['Pos Pred Value']
recall_logistic <- conf_matrix_logistic$byClass['Sensitivity']
f1_score_logistic <- 2 * (precision_logistic * recall_logistic) / (precision_logistic + recall_logistic)

# Print the results
cat("Accuracy:", accuracy_logistic, "\n")
cat("Precision:", precision_logistic, "\n")
cat("Recall:", recall_logistic, "\n")
cat("F1-Score:", f1_score_logistic, "\n")

# Additional step: Check if the predicted probabilities are valid
summary(pred_prob)


# ROC Curve and AUC for Logistic Regression
roc_curve_logistic <- roc(test_data$Diagnosis, pred_prob)
auc_logistic <- auc(roc_curve_logistic)

# Plot the ROC curve for Logistic Regression
roc_data_logistic <- data.frame(
  specificity = rev(roc_curve_logistic$specificities),
  sensitivity = rev(roc_curve_logistic$sensitivities)
)

ggplot(roc_data_logistic, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") +
  theme_minimal() +
  labs(title = paste("ROC Curve for Logistic Regression (AUC =", round(auc_logistic, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))
##
cat("Accuracy:", accuracy_logistic, "| Precision:", precision_logistic, "| Recall:", recall_logistic, "| F1-Score:", f1_score_logistic, "\n")
#####################end of logistic regression#########

################ Decision Tree#################
# Train a Decision Tree model
tree_model <- rpart(Diagnosis ~ ., data = train_data, method = "class")

# Visualize the decision tree
rpart.plot(tree_model)

# Predict on the test set
tree_predictions <- predict(tree_model, test_data, type = "class")

# Confusion Matrix for Decision Tree
conf_matrix_tree <- confusionMatrix(tree_predictions, test_data$Diagnosis)

print(conf_matrix_tree$table)

# Extracting Accuracy, Precision, Recall, and F1-Score for Decision Tree
accuracy_tree <- conf_matrix_tree$overall['Accuracy']
precision_tree <- conf_matrix_tree$byClass['Pos Pred Value']
recall_tree <- conf_matrix_tree$byClass['Sensitivity']
f1_score_tree <- 2 * (precision_tree * recall_tree) / (precision_tree + recall_tree)

# Calculate probabilities for the positive class for ROC curve

tree_prob <- predict(tree_model, test_data, type = "prob")[,2]

cat("Accuracy:", accuracy_tree, "| Precision:", precision_tree, "| Recall:", recall_tree, "| F1-Score:", f1_score_tree, "\n")

# ROC Curve and AUC for Decision Tree
roc_curve_tree <- roc(test_data$Diagnosis, tree_prob)
auc_tree <- auc(roc_curve_tree)

# Plot the ROC curve for Decision Tree
roc_data_tree <- data.frame(
  specificity = rev(roc_curve_tree$specificities),
  sensitivity = rev(roc_curve_tree$sensitivities)
)

ggplot(roc_data_tree, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") +
  theme_minimal() +
  labs(title = paste("ROC Curve for Decision Tree (AUC =", round(auc_tree, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

######################RandomForest#########################

# Load necessary libraries
library(caret)
library(randomForest)  # For random forest
# Train a Random Forest model
rf_model <- randomForest(Diagnosis ~ ., data = train_data, importance = TRUE, ntree = 500)

# Print the model summary
print(rf_model)
# Plot the importance of variables
varImpPlot(rf_model)
# Predict on the test set
rf_predictions <- predict(rf_model, test_data)
# Confusion Matrix
conf_matrix_rf <- confusionMatrix(rf_predictions, test_data$Diagnosis)
print(conf_matrix_rf)

# Extract Accuracy, Precision, Recall, and F1-Score
accuracy_rf <- conf_matrix_rf$overall['Accuracy']
precision_rf <- conf_matrix_rf$byClass['Pos Pred Value']
recall_rf <- conf_matrix_rf$byClass['Sensitivity']
f1_score_rf <- 2 * (precision_rf * recall_rf) / (precision_rf + recall_rf)


cat("Accuracy:", accuracy_rf, "| Precision:", precision_rf, "| Recall:", recall_rf, "| F1-Score:", f1_score_rf, "\n")


# Calculate probabilities for the positive class
rf_prob <- predict(rf_model, test_data, type = "prob")[,2]

# ROC Curve and AUC
roc_curve_rf <- roc(test_data$Diagnosis, rf_prob)
auc_rf <- auc(roc_curve_rf)

# Plot the ROC curve using ggplot2
roc_data_rf <- data.frame(
  specificity = rev(roc_curve_rf$specificities),
  sensitivity = rev(roc_curve_rf$sensitivities)
)

ggplot(roc_data_rf, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") + # ROC curve in orange
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") + # Diagonal line
  theme_minimal() +
  labs(title = paste("ROC Curve for Random Forest (AUC =", round(auc_rf, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

############################Gradient Boosting Machine################

# Load necessary libraries
library(caret)
install.packages("gbm")

library(gbm)  # For gradient boosting
# Train a GBM model
set.seed(123)
gbm_model <- train(Diagnosis ~ ., data = train_data, method = "gbm", 
                   trControl = trainControl(method = "cv", number = 5),
                   verbose = FALSE)
# Print the model summary
print(gbm_model)
# Predict on the test set
gbm_predictions <- predict(gbm_model, test_data)
# Confusion Matrix
conf_matrix_gbm <- confusionMatrix(gbm_predictions, test_data$Diagnosis)
print(conf_matrix_gbm)

# Extract Accuracy, Precision, Recall, and F1-Score
accuracy_gbm <- conf_matrix_gbm$overall['Accuracy']
precision_gbm <- conf_matrix_gbm$byClass['Pos Pred Value']
recall_gbm <- conf_matrix_gbm$byClass['Sensitivity']
f1_score_gbm <- 2 * (precision_gbm * recall_gbm) / (precision_gbm + recall_gbm)

cat("Accuracy:", accuracy_gbm, "| Precision:", precision_gbm, "| Recall:", recall_gbm, "| F1-Score:", f1_score_gbm, "\n")


# Calculate probabilities for the positive class
gbm_prob <- predict(gbm_model, test_data, type = "prob")[,2]

# ROC Curve and AUC
roc_curve_gbm <- roc(test_data$Diagnosis, gbm_prob)
auc_gbm <- auc(roc_curve_gbm)

# Plot the ROC curve using ggplot2
roc_data_gbm <- data.frame(
  specificity = rev(roc_curve_gbm$specificities),
  sensitivity = rev(roc_curve_gbm$sensitivities)
)

ggplot(roc_data_gbm, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") + # ROC curve in purple
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") + # Diagonal line
  theme_minimal() +
  labs(title = paste("ROC Curve for GBM (AUC =", round(auc_gbm, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

###################Support Vector Machine#############
# Load necessary libraries
# Load necessary libraries
library(caret)
library(e1071)# For SVM
install.packages("kernlab")

library(kernlab)  # Required for SVM with caret

# Convert the Diagnosis variable to a factor with valid R variable names
train_data$Diagnosis <- factor(train_data$Diagnosis, 
                               levels = c("0", "1"), 
                               labels = c("Negative", "Positive"))

test_data$Diagnosis <- factor(test_data$Diagnosis, 
                              levels = c("0", "1"), 
                              labels = c("Negative", "Positive"))


# Train an SVM model with probability output
svm_model <- train(Diagnosis ~ ., data = train_data, method = "svmRadial", 
                   trControl = trainControl(method = "cv", number = 5, classProbs = TRUE),
                   tuneLength = 10)

# Print the model summary
print(svm_model)
# Predict on the test set
svm_predictions <- predict(svm_model, test_data)
# Confusion Matrix
conf_matrix_svm <- confusionMatrix(svm_predictions, test_data$Diagnosis)
print(conf_matrix_svm)

# Extract Accuracy, Precision, Recall, and F1-Score
accuracy_svm <- conf_matrix_svm$overall['Accuracy']
precision_svm <- conf_matrix_svm$byClass['Pos Pred Value']
recall_svm <- conf_matrix_svm$byClass['Sensitivity']
f1_score_svm <- 2 * (precision_svm * recall_svm) / (precision_svm + recall_svm)

cat("Accuracy:", accuracy_svm, "| Precision:", precision_svm, "| Recall:", recall_svm, "| F1-Score:", f1_score_svm, "\n")


# Calculate probabilities for the positive class
svm_prob <- predict(svm_model, test_data, type = "prob")[,2]

# ROC Curve and AUC
roc_curve_svm <- roc(test_data$Diagnosis, svm_prob)
auc_svm <- auc(roc_curve_svm)

# Plot the ROC curve using ggplot2
roc_data_svm <- data.frame(
  specificity = rev(roc_curve_svm$specificities),
  sensitivity = rev(roc_curve_svm$sensitivities)
)

ggplot(roc_data_svm, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") + # ROC curve in blue
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") + # Diagonal line
  theme_minimal() +
  labs(title = paste("ROC Curve for SVM (AUC =", round(auc_svm, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

########################## k-NN ######################################
# Normalize the features
preProcValues <- preProcess(train_data[, -ncol(train_data)], method = c("center", "scale"))
train_data_normalized <- predict(preProcValues, train_data)
test_data_normalized <- predict(preProcValues, test_data)

# Train a k-NN model
knn_model <- train(Diagnosis ~ ., data = train_data_normalized, method = "knn",
                   trControl = trainControl(method = "cv", number = 5),
                   tuneLength = 10)
# Print the model summary
print(knn_model)

# Predict on the test set
knn_predictions <- predict(knn_model, test_data_normalized)

# Confusion Matrix
conf_matrix_knn <- confusionMatrix(knn_predictions, test_data_normalized$Diagnosis)
print(conf_matrix_knn)

# Extract Accuracy, Precision, Recall, and F1-Score
accuracy_knn <- conf_matrix_knn$overall['Accuracy']
precision_knn <- conf_matrix_knn$byClass['Pos Pred Value']
recall_knn <- conf_matrix_knn$byClass['Sensitivity']
f1_score_knn <- 2 * (precision_knn * recall_knn) / (precision_knn + recall_knn)

cat("Accuracy:", accuracy_knn, "| Precision:", precision_knn, "| Recall:", recall_knn, "| F1-Score:", f1_score_knn, "\n")

# Calculate probabilities for the positive class (if needed)
knn_prob <- predict(knn_model, test_data_normalized, type = "prob")[,2]

# ROC Curve and AUC
roc_curve_knn <- roc(test_data_normalized$Diagnosis, knn_prob)
auc_knn <- auc(roc_curve_knn)

# Plot the ROC curve using ggplot2
roc_data_knn <- data.frame(
  specificity = rev(roc_curve_knn$specificities),
  sensitivity = rev(roc_curve_knn$sensitivities)
)

ggplot(roc_data_knn, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(size = 1.5, color = "skyblue") + # ROC curve in green
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") + # Diagonal line
  theme_minimal() +
  labs(title = paste("ROC Curve for k-NN (AUC =", round(auc_knn, 2), ")"),
       x = "False Positive Rate",
       y = "True Positive Rate") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 12))

############## Performance Metrics################


# Create a data frame to store the metrics
model_metrics <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting", "SVM", "k-NN"),
  Accuracy = c(accuracy_logistic, accuracy_tree, accuracy_rf, accuracy_gbm, accuracy_svm, accuracy_knn),
  Precision = c(precision_logistic, precision_tree, precision_rf, precision_gbm, precision_svm, precision_knn),
  Recall = c(recall_logistic, recall_tree, recall_rf, recall_gbm, recall_svm, recall_knn),
  F1_Score = c(f1_score_logistic, f1_score_tree, f1_score_rf, f1_score_gbm, f1_score_svm, f1_score_knn)
)

# Print the metrics table
print(model_metrics)


# Print the metrics table
print(model_metrics)
library(ggplot2)

# Reshape the data frame for easier plotting
library(reshape2)
metrics_melted <- melt(model_metrics, id.vars = "Model")
View(model_metrics)

# Plot the metrics for comparison

# Define a custom palette with more shades of blue

blue_palette <- c("#08306b", "#6baed6", "#2171b5", "#c6dbef")
# Plot the metrics for comparison
ggplot(metrics_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Comparison of Model Performance Metrics",
       x = "Model",
       y = "Value") +
  scale_fill_manual(values = blue_palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################### Xplanable AI SHAP and LIME#########

############################SHAPFORCEPLOT##################

# Assuming you have defined instance_id somewhere before this code
# If not, define it as the row number of the instance you want to explain
instance_id <- 1  # For example, the first instance in the test data

# Extract SHAP values for the instance
instance_shap_values <- shap_values$S[instance_id, ]
# Extract the corresponding feature values
instance_features <- shap_values$X[instance_id, ]
# Create a data frame for visualization
shap_df <- data.frame(
  feature = colnames(shap_values$X),
  shap_value = as.numeric(instance_shap_values),
  feature_value = as.numeric(instance_features)
)

# Sort the data by SHAP value for better visualization
shap_df <- shap_df[order(abs(shap_df$shap_value), decreasing = TRUE), ]

# Plotting the SHAP values as a bar chart (this is an approximation of a force plot)
library(ggplot2)

ggplot(shap_df, aes(x = reorder(feature, shap_value), y = shap_value, fill = shap_value > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "SHAP Force Plot for Instance",
       x = "Feature",
       y = "SHAP Value") +
  theme_minimal()


 ###############ConfusionMatrix######################

# Assuming your model and test data are loaded
# model <- your_loaded_model
# test_data <- your_loaded_test_data

# Make predictions
pred_prob <- predict(model, test_data, type = "prob")[, 2] # If the model outputs probabilities
pred_class <- ifelse(pred_prob > 0.5, 1, 0) # Adjust threshold if needed

# Generate confusion matrix
library(caret)
conf_matrix <- confusionMatrix(as.factor(pred_class), as.factor(test_data$Diagnosis))

# Print or plot the confusion matrix
print(conf_matrix)
# Extract the confusion matrix table
conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Actual", "Predicted", "Freq")

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_table, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix",
       x = "Predicted Label",
       y = "Actual Label") +
  theme_minimal()


########################newcofusion###############

# Make predictions
pred_prob <- predict(model, test_data, type = "prob")[, 2]  # Probabilities for the positive class
pred_class <- ifelse(pred_prob > 0.5, 1, 0)  # Adjust threshold as necessary

# Ensure the levels of predicted and actual classes are consistent
pred_class <- factor(pred_class, levels = c(0, 1))
test_data$Diagnosis <- factor(test_data$Diagnosis, levels = c(0, 1))

# Generate confusion matrix
conf_matrix <- confusionMatrix(pred_class, test_data$Diagnosis)

# Print the confusion matrix
print(conf_matrix)

# Extract the confusion matrix table for plotting
conf_matrix_table <- as.data.frame(conf_matrix$table)
colnames(conf_matrix_table) <- c("Actual", "Predicted", "Freq")

# Plot the confusion matrix using ggplot2
ggplot(conf_matrix_table, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix",
       x = "Predicted Label",
       y = "Actual Label") +
  theme_minimal()

