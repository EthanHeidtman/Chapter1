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
      return(list(rmse = NA, mae = NA, bias = NA, count = 0))
   }
   
   # Subset to high salinity events
   high_idx <- obs_clean > threshold
   
   if (sum(high_idx) == 0) {
      return(list(rmse = NA, mae = NA, bias = NA, count = 0))
   }
   
   obs_high <- obs_clean[high_idx]
   pred_high <- pred_clean[high_idx]
   
   rmse <- sqrt(mean((obs_high - pred_high)^2))
   mae <- mean(abs(obs_high - pred_high))
   bias <- mean(pred_high - obs_high)
   
   return(list(rmse = rmse, mae = mae, bias = bias, count = length(obs_high)))
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
compare_models <- function(models, model_types, data, threshold, model_names = NULL) {
   
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
         Model = model_names[i],
         Type = model_types[i],
         Overall_RMSE = eval_results$overall_rmse,
         Weighted_RMSE = eval_results$weighted_rmse,
         Overall_R2 = eval_results$overall_r2,
         High_Sal_RMSE = eval_results$high_salinity_rmse,
         High_Sal_MAE = eval_results$high_salinity_mae,
         High_Sal_Bias = eval_results$high_salinity_bias,
         High_Sal_R2 = eval_results$high_salinity_r2,
         High_Sal_Count = eval_results$high_salinity_count,
         Total_Obs = eval_results$total_observations,
         stringsAsFactors = FALSE
      ))
   }
   return(results)
}