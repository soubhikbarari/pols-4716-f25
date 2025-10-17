evaluate_classification <- function(ytrue, ypred) {
  # Evaluate a classifier's predictions
  tp <- sum(ytrue == 1 & ypred == 1, na.rm = T)
  fp <- sum(ytrue == 0 & ypred == 1, na.rm = T)
  fn <- sum(ytrue == 1 & ypred == 0, na.rm = T)
  tn <- sum(ytrue == 0 & ypred == 0, na.rm = T)
  
  accuracy  <- mean(ytrue == ypred, na.rm = T)
  fnr       <- fn / (fn + tp)
  fpr       <- fp / (fp + tn)
  precision <- tp / (tp + fp)
  recall    <- tp / (tp + fn)
  
  tibble(accuracy, fpr, fnr, precision, recall)
}

compute_roc_auc <- function(y_true, y_prob, plot = TRUE) {
  # Create an ROC curve and also output the AUC
  valid_obs <- !is.na(y_true) & !is.na(y_prob)
  y_true <- as.numeric(y_true[valid_obs])
  y_prob <- as.numeric(y_prob[valid_obs])
  
  if (length(y_true) != length(y_prob)) stop("y_true and y_prob must be the same length")
  if (!all(y_true %in% c(0, 1))) stop("y_true must be binary (0/1)")
  
  thresholds <- seq(0, 1, by = 0.005)
  tpr <- numeric(length(thresholds))
  fpr <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    thr <- thresholds[i]
    pred <- ifelse(y_prob >= thr, 1, 0)
    TP <- sum(pred == 1 & y_true == 1)
    FP <- sum(pred == 1 & y_true == 0)
    FN <- sum(pred == 0 & y_true == 1)
    TN <- sum(pred == 0 & y_true == 0)
    
    tpr[i] <- TP / (TP + FN)  # sensitivity
    fpr[i] <- FP / (FP + TN)  # 1 - specificity
  }
  
  # calculate area under curve (AUC)
  ord <- order(fpr)
  x <- fpr[ord]
  y <- tpr[ord]
  auc <- sum(diff(x) * (head(y, -1) + tail(y, -1)) / 2)
  
  roc_df <- data.frame(threshold = thresholds, fpr = fpr, tpr = tpr)
  
  if (plot) {
    gg <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
      geom_line(color = "#2E86C1", size = 1.2) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray50") +
      labs(
        x = "False Positive Rate",
        y = "True Positive Rate",
        title = paste0("ROC Curve (AUC = ", round(auc, 3), ")"),
        subtitle ="Cooperative Election Study | Logistic Model"
      ) +
      theme_minimal(base_size = 13)
    print(gg)
  }
  
  return(list(
    auc = auc,
    roc = data.frame(threshold = thresholds, fpr = fpr, tpr = tpr)
  ))
}
