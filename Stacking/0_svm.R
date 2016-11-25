library(caret)

#######
### Parameters
data_train <- train_num_rs
data_test <- test_num

# Notes:
# - Assumes folds already calculated

#######


### Loop through the folds
mdl_pred <- rep(NA, n)
mdl_pred_test <- matrix(NA, nrow=dim(test_num)[1], length(folds))

train_control<- trainControl(method="cv", number=5, savePredictions = TRUE)

mae_log <- function(pred,label){
  # Computes the MAE
  # Assumes pred and label in log form 
  sum(abs(exp(label) - exp(pred)))/length(labels)
}

for (i in 1:length(folds)){
  holdout <- folds[[i]]
  rest <- unlist(folds[-i])
  
  # Train
  input <- data_train %>% 
    select(71:80,loss) %>%
    slice(rest)
  
  input_pred <- data_train %>% 
    select(71:80) %>%
    slice(holdout)
  
  mdl_fit <- train(loss ~ .,
                  data = input,
                  method = "svmRadial",
                  tuneLength = 5,
                  #trControl = trainControl(method = "repeatedcv",repeats = 5),
                  trControl = train_control)
  
  # Predict on fold AND on test data
  mdl_pred[holdout] <- predict(mdl_fit, newdata=input_pred)
  mdl_pred_test[ ,i] <- predict(mdl_fit, newdata=test_num) 
  
  # Errors
  error_holdout <- mae_log(mdl_pred[holdout], data_train$loss[holdout])
  cat("MAE:",error_holdout,"\n")
}

###