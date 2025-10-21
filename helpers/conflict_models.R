# Logistic regression helper
fit_and_eval_logit <- function(fit_data,
                               eval_data,
                               model_name = "Logit",
                               formula,
                               prob_cutoff = 0.5,
                               upweight_wars_by = 1) {
  
  # Fit model on train fold
  message("Fitting ", model_name)
  mdl <- glm(
    formula = formula,
    family = "binomial",
    data = fit_data %>%
      mutate(wts = ifelse(civilwar == "War", upweight_wars_by, 1)),
    weights = wts
  )
  
  # Predict on validation fold
  probs <- predict(mdl, newdata = eval_data, type = "response")
  preds <- ifelse(probs > prob_cutoff, "War", "Peace")
  
  # Evaluate
  metrics <- evaluate_classification(
    ytrue = as.numeric(eval_data$civilwar == "War"),
    ypred = as.numeric(preds == "War")
  )
  
  # Attach model name
  metrics <- metrics %>%
    mutate(Model = model_name)
  
  return(metrics)
}


# Random forest helper
fit_and_eval_forest <- function(fit_data,
                                eval_data,
                                model_name = "Forest",
                                formula,
                                prob_cutoff = 0.5,
                                num_trees = 100,
                                upweight_wars_by = 1) {
  library(randomForest)
  
  # Fit forest on train fold
  message("Fitting ", model_name)
  rf <- randomForest(
    formula = formula,
    data = fit_data,
    ntree = num_trees,
    importance = TRUE,
    classwt = if (!is.null(upweight_wars_by)) c("Peace" = 1, "War" = upweight_wars_by) else NULL
  )
  
  # Predict on validation fold
  probs <- predict(rf, newdata = eval_data, type = "prob")[, "War"]
  preds <- ifelse(probs > prob_cutoff, "War", "Peace")
  
  # Evaluate
  metrics <- evaluate_classification(
    ytrue = as.numeric(eval_data$civilwar == "War"),
    ypred = as.numeric(preds == "War")
  )
  
  # Attach model name
  metrics <- metrics %>%
    mutate(Model = model_name)
  
  return(metrics)
}