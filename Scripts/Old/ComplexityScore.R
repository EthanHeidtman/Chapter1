# Function to calculate complexity score
calculate_complexity <- function(model, model_type = "gam") {
   if (model_type == "gam") {
      if (inherits(model, "gam")) {
         # GAM complexity metrics
         edf <- sum(model$edf)  # Total effective degrees of freedom
         n_smooths <- length(model$smooth)  # Number of smooth terms
         
         # Simple complexity penalty
         complexity_penalty <- edf / 100  # Normalize by reasonable max EDF
         parsimony_score <- max(0, 1 - complexity_penalty)
         
      } else {
         edf <- 20
         n_smooths <- 5
         parsimony_score <- 0.8
      }
   } else {
      # Linear models
      n_params <- length(coef(model))
      edf <- n_params
      n_smooths <- 0
      
      complexity_penalty <- n_params / 50  # Normalize by reasonable max parameters
      parsimony_score <- max(0, 1 - complexity_penalty)
   }
   
   return(list(
      score = parsimony_score,
      edf = edf,
      n_smooths = n_smooths,
      model_type = model_type
   ))
}