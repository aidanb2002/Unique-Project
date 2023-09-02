library(xgboost)
library(caret)
library(dplyr)

ml <- joined_table %>% 
  select(full_name, plus, wOBA, Hard.)

set.seed(123)
splitIndex <- createDataPartition(ml$wOBA, p = 0.8, list = FALSE)
train_data <- ml[splitIndex, ]
test_data <- ml[-splitIndex, ]

# Separate the target variable and input features for both training and test datasets
train_label <- train_data$wOBA
train_input <- as.matrix(train_data[, "plus", drop=FALSE])

test_label <- test_data$wOBA
test_input <- as.matrix(test_data[, "plus", drop=FALSE])

# Train the XGBoost model using cross-validation
dtrain <- xgb.DMatrix(data=train_input, label=train_label)
params <- list(
  objective = "reg:squarederror",
  eta = 0.01,
  max_depth = 6,
  eval_metric = "rmse"
)

# Perform cross-validation
cv_model <- xgb.cv(params=params, data=dtrain, nrounds=500, nfold=5, 
                   early_stopping_rounds=10, verbose=0)

# Train the model using the optimal number of rounds
optimal_nrounds <- cv_model$best_iteration
model <- xgb.train(params=params, data=dtrain, nrounds=optimal_nrounds)

# Create a DMatrix for the entire dataset
full_data_input <- as.matrix(ml[, "plus", drop=FALSE])
dfull <- xgb.DMatrix(data=full_data_input)

# Predict on the full dataset
full_predictions <- predict(model, newdata=dfull)

# Add predictions to the full dataset
joined_table$predicted_wOBA <- full_predictions

joined_table <- joined_table %>% 
  mutate(predicted_wOBA = round(predicted_wOBA, 3),
         new_plus = round((mean(predicted_wOBA) / predicted_wOBA) * 100))



setwd("/Users/aidanbeilke/Desktop/R Documents/Shiny App")

write.xlsx(joined_table, file = "joined_table.xlsx")
  
  
  
  
