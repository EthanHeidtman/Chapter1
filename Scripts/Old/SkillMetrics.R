# Function to calculate skill-based metrics
calculate_skill_metrics <- function(observed, predicted, reference_pred = NULL) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   if (sum(valid_idx) <= 1) {
      return(list(skill_score = 0, nash_sutcliffe = 0))
   }
   
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   # If no reference provided, use climatological mean
   if (is.null(reference_pred)) {
      reference_pred <- rep(mean(obs_clean), length(obs_clean))
   } else {
      reference_pred <- reference_pred[valid_idx]
   }
   
   # Model skill score relative to reference
   mse_model <- mean((obs_clean - pred_clean)^2)
   mse_reference <- mean((obs_clean - reference_pred)^2)
   
   skill_score <- ifelse(mse_reference == 0, 0, 1 - (mse_model / mse_reference))
   
   # Nash-Sutcliffe Efficiency
   obs_var <- sum((obs_clean - mean(obs_clean))^2)
   nse <- ifelse(obs_var == 0, 0, 1 - (sum((obs_clean - pred_clean)^2) / obs_var))
   
   return(list(
      skill_score = skill_score,
      nash_sutcliffe = nse
   ))
}


