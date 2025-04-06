
# Computec pairwise Cramér’s V
compute_cramer_v_matrix <- function(data) {
  vars <- names(data)
  n <- length(vars)
  mtx <- matrix(NA, n, n)
  rownames(mtx) <- colnames(mtx) <- vars
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      tbl <- table(data[[i]], data[[j]])
      mtx[i, j] <- suppressWarnings(DescTools::CramerV(tbl))
    }
  }
  mtx
}

# PLots confusion matrix
plot_confusion_matrix <- function(actual, predicted, eval_measure, title, positive_label = "1") {
  
  cm <- confusionMatrix(predicted, actual, positive = positive_label, )
  
  sensi <- round(cm$byClass[1], 2) # sensitivity
  
  speci <- round(cm$byClass[2], 2) # specificity
  
  cm_df <- as.data.frame(cm$table)
  
  colnames(cm_df) <- c("Predicted", "Ground_truth", "Freq")
  
  
  cm_df$Label <- with(cm_df, ifelse(Predicted == positive_label & Ground_truth == positive_label, "TP",
                                    ifelse(Predicted == positive_label & Ground_truth != positive_label, "FP",
                                           ifelse(Predicted != positive_label & Ground_truth == positive_label, "FN", "TN"))))
  
  
  p <- ggplot(data = cm_df, aes(x = Ground_truth, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = paste0(Label, "\n", Freq)), size = 7, color = "black") +
    scale_fill_gradient(low = "#E8F0F9", high = "#4682B4") +
    theme_minimal(base_size = 17) +
    labs(title = title, x = "Ground_truth", y = "Predicted") +
    theme(plot.title = element_text(hjust = 0.5), legend.position = "none")+
    xlab("Ground truth") +
    labs(title = paste(title, "\n\n MCC:", eval_measure, ", Sensi:", sensi, ", Speci:", speci))+
    theme(plot.title = element_text(hjust = 0.5, size = 17))
  
  
  p
  
}



plot_missingness_distribution <- function(dat, var) {
    tmp <- sym(var)
    gg_miss_fct(dat, fct = !!tmp) +
      ylab(NULL) +  
      theme(
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle=0, size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        legend.text = element_text(size=14),
        legend.title=element_text(size=14)
      )
    

}


# Evaluates model performance
evaluate_model_fit <- function(ground_truth, probs) {
  
  t <- best_threshold(ground_truth, probs)
  
  pred_class <- factor(ifelse(probs > t$threshold, 1, 0), levels=c(1, 0)) 
  
  sensi <- round(sens_vec(truth = ground_truth, estimate = pred_class), 2)
  
  speci <- round(spec_vec(truth = ground_truth, estimate = pred_class), 2)
  
  f1 <- round(f_meas_vec(truth = ground_truth, estimate = pred_class), 2)
  
  aucroc <- round(roc_auc_vec(truth = ground_truth, probs), 2)
  
  matcc <- mcc_vec(ground_truth, pred_class)
  
  list("sensi" = sensi, "speci" = speci, "f1" = f1, "aucroc" = aucroc, "matcc" = matcc, "pred_class"=pred_class)
  
  }




xgb_tune <- function(params_list, dtrain, nrounds=1000) {
  
      cv_results <- list()
      
      for(i in 1:nrow(params_list)){
        
        params <- list(
          objective = "binary:logistic",  
          eval_metric = "auc",            
          eta = params_list$eta[i],
          max_depth = params_list$max_depth[i],
          subsample = params_list$subsample[i],
          colsample_bytree = params_list$colsample_bytree[i]
        )
        
        cv <- xgb.cv(
          params = params,
          data = dtrain,
          nrounds = nrounds,
          nfold = 5,
          early_stopping_rounds = 20,
          maximize = TRUE,
          verbose = 0,
          prediction = TRUE
        )
        
        best_iter <- cv$best_iteration
        best_score <- cv$evaluation_log[best_iter]$test_auc_mean
        
        cv_results[[i]] <- data.table(
          eta = params$eta,
          max_depth = params$max_depth,
          subsample = params$subsample,
          colsample_bytree = params$colsample_bytree,
          best_iteration = best_iter,
          auc = best_score
        )
      
        
        }
      
        cv_results

}




best_threshold <- function(ground_truth, probs) {
  
    roc_obj <- roc(response = ground_truth, predictor = probs)
    
    coords(roc_obj, "best", best.method = "youden")
  
    }




plot_roc <- function(df, title) {
  
  roc_curve(df, ground_truth, probs) %>%
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, size = 18), 
      panel.spacing = unit(1.2, "lines"),
      axis.text.x = element_text(size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size=16)
      ) +
    labs(title = title)

}


plot_wrong_predictions <- function(df, var, what_var) {
  
   tmp <- sym(var)
  
   ggplot(df, aes(x = !!tmp, fill = factor(stroke))) +
    geom_density(alpha = 0.4) +
    
    # Rug plot for incorrect predictions
    geom_rug(data = subset(df, outcome %in% c("False Positive", "False Negative")),
             aes(color = outcome),
             sides = "b", alpha = 0.7) +
    
    # Manual fill colors for stroke = 0 and 1
    scale_fill_manual(values = c("0" = "#A6CEE3", "1" = "#33A02C"),
                      labels = c("No Stroke", "Stroke"),
                      name = "Stroke Status") +
    
    # Manual colors for rug (incorrect predictions)
    scale_color_manual(values = c("False Positive" = "blue", "False Negative" = "red"),
                       name = "Incorrect Prediction") +
    
    labs(x = what_var, y = "Density") +
    theme_minimal(base_size = 14)
  
}

