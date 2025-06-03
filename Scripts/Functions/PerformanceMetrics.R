# Mean Absolute Percentage Error (MAPE) - sensitive to extreme values
mape <- function(observed, predicted) {
   valid_idx <- !is.na(observed) & !is.na(predicted) & observed != 0
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   return(mean(abs((obs_clean - pred_clean) / obs_clean)) * 100)
}

# Peak Flow Criteria (PFC) - specifically for extreme events
peak_flow_criteria <- function(observed, predicted, threshold_percentile = 95) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Define threshold as percentile of observed data
   threshold <- quantile(obs_clean, threshold_percentile/100, na.rm = TRUE)
   
   # Find peak events
   peak_idx <- obs_clean >= threshold
   
   if (sum(peak_idx) == 0) return(NA)
   
   obs_peaks <- obs_clean[peak_idx]
   pred_peaks <- pred_clean[peak_idx]
   
   # Calculate relative error for peaks
   relative_errors <- abs(obs_peaks - pred_peaks) / obs_peaks
   return(mean(relative_errors) * 100)
}

# Threshold Exceedance Probability (TEP) metrics
threshold_exceedance_metrics <- function(observed, predicted, threshold) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA))
   }
   
   # Binary classification: does it exceed threshold?
   obs_exceed <- obs_clean > threshold
   pred_exceed <- pred_clean > threshold
   
   # Confusion matrix elements
   true_positives <- sum(obs_exceed & pred_exceed)
   false_positives <- sum(!obs_exceed & pred_exceed)
   true_negatives <- sum(!obs_exceed & !pred_exceed)
   false_negatives <- sum(obs_exceed & !pred_exceed)
   
   # Calculate metrics
   hit_rate <- if(sum(obs_exceed) > 0) true_positives / sum(obs_exceed) else NA
   false_alarm_rate <- if(sum(!obs_exceed) > 0) false_positives / sum(!obs_exceed) else NA
   critical_success_index <- true_positives / (true_positives + false_positives + false_negatives)
   
   return(list(
      hit_rate = hit_rate,
      false_alarm_rate = false_alarm_rate,
      critical_success_index = critical_success_index
   ))
}

# Volume Error for extreme events
volume_error <- function(observed, predicted, threshold) {
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Focus on values above threshold
   exceed_idx <- obs_clean > threshold
   
   if (sum(exceed_idx) == 0) return(NA)
   
   obs_volume <- sum(obs_clean[exceed_idx])
   pred_volume <- sum(pred_clean[exceed_idx])
   
   volume_error <- (pred_volume - obs_volume) / obs_volume * 100
   return(volume_error)
}


# Function to calculate weighted RMSE, giving emphasis to higher values
weighted_rmse <- function(observed, predicted) {
   # Remove NA values
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) return(NA)
   
   # Weights proportional to observed values
   weights <- obs_clean / mean(obs_clean, na.rm = TRUE)
   
   weighted_squared_errors <- weights * (obs_clean - pred_clean)^2
   return(sqrt(mean(weighted_squared_errors, na.rm = TRUE)))
}

# Function to calculate metrics for high salinity events only
extreme_event_metrics <- function(observed, predicted, threshold) {
   # Remove NA values first
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, peak_flow_criteria = NA, 
                  hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA,
                  volume_error = NA))
   }
   
   # Subset to high salinity events
   high_idx <- obs_clean > threshold
   
   if (sum(high_idx) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0,
                  mape = NA, peak_flow_criteria = NA,
                  hit_rate = NA, false_alarm_rate = NA, critical_success_index = NA,
                  volume_error = NA))
   }
   
   obs_high <- obs_clean[high_idx]
   pred_high <- pred_clean[high_idx]
   
   # Original metrics
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   # New metrics
   mape_val <- mape(obs_high, pred_high)
   pfc_val <- peak_flow_criteria(obs_clean, pred_clean)
   tep_metrics <- threshold_exceedance_metrics(obs_clean, pred_clean, threshold)
   vol_error <- volume_error(obs_clean, pred_clean, threshold)
   
   return(list(rmse = rmse, mae = mae, bias = bias, count = length(obs_high),
               mape = mape_val, peak_flow_criteria = pfc_val,
               hit_rate = tep_metrics$hit_rate, 
               false_alarm_rate = tep_metrics$false_alarm_rate,
               critical_success_index = tep_metrics$critical_success_index,
               volume_error = vol_error))
}


# Function to perform model evaluation
evaluate_model <- function(model, data, threshold, model_type = "linear") {
   
   # Get predictions based on model type
   predicted <- get_model_predictions(model, data, model_type)
   observed <- data$Salinity
   
   # Remove rows where either observed or predicted is NA
   valid_idx <- !is.na(observed) & !is.na(predicted)
   obs_clean <- observed[valid_idx]
   pred_clean <- predicted[valid_idx]
   
   if (length(obs_clean) == 0) {
      return(list(
         overall_rmse = NA,
         weighted_rmse = NA,
         overall_r2 = NA,
         high_salinity_rmse = NA,
         high_salinity_mae = NA,
         high_salinity_bias = NA,
         high_salinity_r2 = NA,
         high_salinity_count = 0,
         high_salinity_mape = NA,
         peak_flow_criteria = NA,
         hit_rate = NA,
         false_alarm_rate = NA,
         critical_success_index = NA,
         volume_error = NA,
         total_observations = 0
      ))
   }
   
   # Overall metrics
   overall_rmse <- sqrt(mean((obs_clean - pred_clean)^2))
   weighted_rmse_val <- weighted_rmse(obs_clean, pred_clean)
   
   # Overall R-squared
   tss <- sum((obs_clean - mean(obs_clean))^2)
   rss <- sum((obs_clean - pred_clean)^2)
   overall_r2 <- 1 - rss/tss
   
   # High salinity metrics
   high_metrics <- extreme_event_metrics(obs_clean, pred_clean, threshold)
   
   # R-squared for high salinity events
   high_idx <- obs_clean > threshold
   if (sum(high_idx) > 1) {
      obs_high <- obs_clean[high_idx]
      pred_high <- pred_clean[high_idx]
      tss_high <- sum((obs_high - mean(obs_high))^2)
      rss_high <- sum((obs_high - pred_high)^2)
      high_r2 <- 1 - rss_high/tss_high
   } else {
      high_r2 <- NA
   }
   
   return(list(
      overall_rmse = overall_rmse,
      weighted_rmse = weighted_rmse_val,
      overall_r2 = overall_r2,
      high_salinity_rmse = high_metrics$rmse,
      high_salinity_mae = high_metrics$mae,
      high_salinity_bias = high_metrics$bias,
      high_salinity_r2 = high_r2,
      high_salinity_count = high_metrics$count,
      high_salinity_mape = high_metrics$mape,
      peak_flow_criteria = high_metrics$peak_flow_criteria,
      hit_rate = high_metrics$hit_rate,
      false_alarm_rate = high_metrics$false_alarm_rate,
      critical_success_index = high_metrics$critical_success_index,
      volume_error = high_metrics$volume_error,
      total_observations = length(obs_clean)
   ))
}

get_model_predictions <- function(model, data, model_type = "linear") {
   
   if (model_type == "linear" || model_type == "gam") {
      # Standard predict method works for both lm and gam objects
      predictions <- predict(model, newdata = data)
      
   } else if (model_type == "threshold") {
      # Handle threshold models (which are lists containing multiple models)
      predictions <- numeric(nrow(data))
      
      if ("LowDischargeRegime" %in% names(data)) {
         # Discharge-based threshold model
         low_idx <- data$LowDischargeRegime
         high_idx <- !data$LowDischargeRegime
         
         if (sum(low_idx, na.rm = TRUE) > 0) {
            predictions[low_idx] <- predict(model$low_regime, newdata = data[low_idx, ])
         }
         if (sum(high_idx, na.rm = TRUE) > 0) {
            predictions[high_idx] <- predict(model$high_regime, newdata = data[high_idx, ])
         }
         
      } else if ("IsHighStress" %in% names(data)) {
         # Stress-based threshold model
         normal_idx <- !data$IsHighStress
         stress_idx <- data$IsHighStress
         
         if (sum(normal_idx, na.rm = TRUE) > 0) {
            predictions[normal_idx] <- predict(model$normal_regime, newdata = data[normal_idx, ])
         }
         if (sum(stress_idx, na.rm = TRUE) > 0) {
            predictions[stress_idx] <- predict(model$stress_regime, newdata = data[stress_idx, ])
         }
      }
   }
}

# Function to gather the model predictions with confidence intervals
get_predictions <- function(model, data, model_type = "linear") {
   
   if (model_type == "linear") {
      # Get predictions with standard errors for linear models
      preds <- predict(model, newdata = data, se.fit = TRUE)
      predicted <- preds$fit
      se <- preds$se.fit
      
   } else if (model_type == "gam") {
      # GAM predictions with standard errors
      preds <- predict(model, newdata = data, se.fit = TRUE)
      predicted <- preds$fit
      se <- preds$se.fit
      
   } else if (model_type == "threshold") {
      # Threshold models - get predictions without SE for now
      predicted <- get_model_predictions(model, data, model_type)
      se <- rep(NA, length(predicted))  # Standard errors not easily available for threshold models
   }
   
   # Create prediction dataframe
   pred_df <- data.frame(
      date_time = data$DateTime,
      observed = data$Salinity,
      predicted = predicted,
      lower_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted - 1.96 * se,
      upper_ci = if(all(is.na(se))) rep(NA, length(predicted)) else predicted + 1.96 * se,
      is_high = data$Salinity > threshold,
      stringsAsFactors = FALSE
   )
   
   return(pred_df)
}


# Function to compare multiple models
compare_models <- function(models, model_types, data, threshold, model_names = NULL, Group) {
   
   # Create Model names if none are given
   if (is.null(model_names)) {
      model_names <- paste0("Model_", 1:length(models))
   }
   
   # Initialize results data frame
   results <- data.frame()
   
   for (i in 1:length(models)) {
      eval_results <- evaluate_model(models[[i]], data, threshold, model_types[i])
      
      # Add the results to the results dataframe
      results <- rbind(results, data.frame(
         Group = as.factor(Group),
         Model = model_names[i],
         Type = model_types[i],
         Overall_RMSE = eval_results$overall_rmse,
         Weighted_RMSE = eval_results$weighted_rmse,
         Overall_R2 = eval_results$overall_r2,
         High_Sal_RMSE = eval_results$high_salinity_rmse,
         High_Sal_MAE = eval_results$high_salinity_mae,
         High_Sal_Bias = eval_results$high_salinity_bias,
         High_Sal_R2 = eval_results$high_salinity_r2,
         High_Sal_Mape = eval_results$high_salinity_mape,
         High_Sal_PeakFlow = eval_results$peak_flow_criteria,
         High_Sal_HitRate = eval_results$hit_rate,
         High_Sal_FalseAlarm = eval_results$false_alarm_rate,
         High_Sal_CriticalSuccess = eval_results$critical_success_index,
         High_Sal_VolumeError = eval_results$volume_error,
         High_Sal_Count = eval_results$high_salinity_count,
         Total_Obs = eval_results$total_observations,
         stringsAsFactors = FALSE
      ))
   }
   return(results)
}
