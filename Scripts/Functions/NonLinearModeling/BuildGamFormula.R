# Formula to systematically build formula for each particular strategy
build_gam_formula <- function(linear_formula, linear_predictors, strategy) {
   
   response_var <- all.vars(linear_formula)[1]
   
   # Default k configuration
   default_k_config <- list(
      main_effects = 10,        # For main effect smooths
      flow_vars = 15,           # Flow variables may need more flexibility
      stress_vars = 12,         # Stress variables moderate complexity
      tidal_vars = 8,           # Tidal effects typically simpler
      interactions = 6,         # By-terms should be conservative
      tensor_flow = c(8, 6),    # Tensor products: [main_var, by_var]
      tensor_stress = c(10, 6), # Stress tensors slightly more complex
      categorical_by = 4,       # When continuous varies by categorical
      max_k = 20,               # Hard limit to prevent excessive computation
      min_k = 3                 # Minimum for meaningful smooths
   )
   
   # Parse the original linear formula (returns various groups of predictors)
   var_types <- parse_linear_formula(linear_formula)
   
   # Build formula based on strategy
   if(strategy == "linear" || strategy == "baseline") {
      # Reproduce linear model exactly
      formula_rhs <- paste(deparse(linear_formula[[3]]), collapse = "")
      by_vars_used <- character(0)
      
   } else if(strategy == "smooth_all") {
      # Convert interactions first to identify by variables
      interaction_result <- convert_interactions_to_gam(var_types$interactions, var_types, "smart")
      by_vars_used <- interaction_result$by_vars
      
      # Smooth all continuous main effects, excluding those used in by terms
      continuous_for_smooth <- setdiff(var_types$continuous_vars, by_vars_used)
      smooth_main <- if(length(continuous_for_smooth) > 0) paste0("s(", continuous_for_smooth, ")") else NULL
      
      # Keep categorical variables as parametric main effects only if not used in by terms
      categorical_for_main <- setdiff(var_types$categorical_vars, by_vars_used)
      parametric_main <- categorical_for_main
      
      formula_rhs <- paste(c(smooth_main, parametric_main, interaction_result$terms), collapse = " + ")
      
   } else if(strategy == "smooth_flow") {
      # Convert interactions first
      interaction_result <- convert_interactions_to_gam(var_types$interactions, var_types, "by_terms")
      by_vars_used <- interaction_result$by_vars
      
      # Smooth only flow variables not used in by terms
      flow_for_smooth <- setdiff(var_types$flow_vars, by_vars_used)
      smooth_terms <- if(length(flow_for_smooth) > 0) paste0("s(", flow_for_smooth, ")") else NULL
      
      # Linear terms excluding those used in by terms
      linear_terms <- setdiff(var_types$main_effects, c(var_types$flow_vars, by_vars_used))
      
      formula_rhs <- paste(c(smooth_terms, linear_terms, interaction_result$terms), collapse = " + ")
      
   } else if(strategy == "smooth_stress") {
      # Convert interactions first
      interaction_result <- convert_interactions_to_gam(var_types$interactions, var_types, "by_terms")
      by_vars_used <- interaction_result$by_vars
      
      # Smooth only stress variables not used in by terms
      stress_for_smooth <- setdiff(var_types$stress_vars, by_vars_used)
      smooth_terms <- if(length(stress_for_smooth) > 0) paste0("s(", stress_for_smooth, ")") else NULL
      
      # Linear terms excluding those used in by terms
      linear_terms <- setdiff(var_types$main_effects, c(var_types$stress_vars, by_vars_used))
      
      formula_rhs <- paste(c(smooth_terms, linear_terms, interaction_result$terms), collapse = " + ")
      
   } else if(strategy == "tensor" || strategy == "tensor_flow_stress") {
      # Create strategic tensor products first
      tensor_result <- create_strategic_tensors(var_types, "salinity")
      
      # Convert remaining interactions
      interaction_result <- convert_interactions_to_gam(var_types$interactions, var_types, "parametric")
      by_vars_used <- c(tensor_result$by_vars, interaction_result$by_vars)
      
      # Get variables already handled by tensor terms
      tensor_vars <- unique(unlist(strsplit(gsub("te\\(|s\\(|\\)|,", " ", tensor_result$terms), "\\s+")))
      tensor_vars <- tensor_vars[tensor_vars != "" & !grepl("by", tensor_vars)]
      
      # Remaining main effects as smooths (continuous) or linear (categorical)
      remaining_continuous <- setdiff(var_types$continuous_vars, c(tensor_vars, by_vars_used))
      remaining_categorical <- setdiff(var_types$categorical_vars, by_vars_used)
      
      remaining_smooth <- if(length(remaining_continuous) > 0) paste0("s(", remaining_continuous, ")") else NULL
      
      formula_rhs <- paste(c(tensor_result$terms, remaining_smooth, remaining_categorical, interaction_result$terms), 
                           collapse = " + ")
      
   } else if(strategy == "mixed_interactions") {
      # Convert interactions first with smart method
      interaction_result <- convert_interactions_to_gam(var_types$interactions, var_types, "smart")
      by_vars_used <- interaction_result$by_vars
      
      # Smooth continuous main effects not used in by terms
      continuous_for_smooth <- setdiff(var_types$continuous_vars, by_vars_used)
      smooth_main <- if(length(continuous_for_smooth) > 0) paste0("s(", continuous_for_smooth, ")") else NULL
      
      # Parametric categorical main effects not used in by terms
      categorical_for_main <- setdiff(var_types$categorical_vars, by_vars_used)
      parametric_main <- categorical_for_main
      
      formula_rhs <- paste(c(smooth_main, parametric_main, interaction_result$terms), collapse = " + ")
      
   } else {
      # Default: keep original formula
      formula_rhs <- paste(deparse(linear_formula[[3]]), collapse = "")
      by_vars_used <- character(0)
   }
   
   # Clean up formula string
   formula_rhs <- gsub("\\s+", " ", formula_rhs)        # Clean whitespace
   formula_rhs <- gsub("^\\s+|\\s+$", "", formula_rhs)  # Trim
   formula_rhs <- gsub("\\+\\s*\\+", "+", formula_rhs)  # Remove double +
   
   # Remove empty terms
   terms <- strsplit(formula_rhs, "\\s*\\+\\s*")[[1]]
   terms <- terms[terms != "" & !is.na(terms)]
   formula_rhs <- paste(terms, collapse = " + ")
   
   # Handle edge case where formula_rhs is empty
   if(formula_rhs == "" || is.na(formula_rhs)) {
      formula_rhs <- "1"  # Intercept only
   }
   
   # Construct final formula
   gam_formula <- as.formula(paste(response_var, "~", formula_rhs))
   
   return(list(
      formula = gam_formula,
      var_types = var_types,
      strategy = strategy,
      formula_string = formula_rhs,
      by_vars_excluded = by_vars_used
   ))
}
