library(xgboost)
library(caret)
library(dplyr)

ml <- joined_table %>% 
  select(full_name, plus, wOBA, Hard.)

set.seed(123)
# Features (excluding 'full_name' and target 'wOBA')
features <- ml %>% select(-full_name, -Hard.)

# Convert features to matrix format as required by xgboost
data_input <- as.matrix(features)

# Target variable
data_label <- ml$Hard.


params <- list(
  objective = "reg:squarederror",
  eta = 0.01,
  max_depth = 6,
  eval_metric = "rmse",
  # Include regularization parameters if desired
  alpha = 0.01,      # L1 regularization term
  lambda = 1         # L2 regularization term
)


# Convert data to DMatrix format
dmatrix_data <- xgb.DMatrix(data=data_input, label=data_label)

# Perform k-fold CV and get predictions for each fold
cv_results <- xgb.cv(
  params=params, 
  data=dmatrix_data, 
  nrounds=100,  # number of boosting rounds. Might want to increase this.
  nfold=5,      # 5-fold CV
  prediction=TRUE  # to get predictions for each fold
)

# Extract predictions
predictions <- cv_results$pred

# Add predictions to the full dataset
joined_table$predicted_hhr <- predictions

joined_table <- joined_table %>% 
  mutate(predicted_hhr = round(predicted_hhr, 3),
         new_plus = round((mean(predicted_hhr) / predicted_hhr)*100))

setwd("/Users/aidanbeilke/Desktop/R Documents/Shiny App")

write.xlsx(joined_table, file = "joined_table.xlsx")
  

  
