# Function to build the GAM formula depending on the identified strategy
build_gam_formula <- function(linear_formula, linear_predictors, strategy) {
   
   response_var <- all.vars(linear_formula)[1]
   
   # Parse the original linear formula to extract interactions
   vars <- parse_linear_formula(linear_formula)
   
   # Build formula based on strategy
   if(strategy == "linear" || strategy == "baseline") {
      # Reproduce linear model exactly
      formula_rhs <- paste(deparse(linear_formula[[3]]), collapse = "")
      
   } else if(strategy == "smooth_all") {
      # Smooth all continuous main effects, smart conversion of interactions
      smooth_main <- paste0("s(", vars$continuous_vars, ")")
      parametric_main <- vars$categorical_vars
      smart_interactions <- convert_interactions_to_gam(vars$interactions, vars, "smart")
      
      formula_rhs <- paste(c(smooth_main, parametric_main, smart_interactions), collapse = " + ")
      
   } else if(strategy == "smooth_flow") {
      # Smooth only flow variables, keep others linear
      smooth_terms <- if(length(vars$flow_vars) > 0) paste0("s(", vars$flow_vars, ")") else NULL
      linear_terms <- setdiff(vars$main_effects, vars$flow_vars)
      flow_interactions <- convert_interactions_to_gam(vars$interactions, vars, "by_terms")
      
      formula_rhs <- paste(c(smooth_terms, linear_terms, flow_interactions), collapse = " + ")
      
   } else if(strategy == "smooth_stress") {
      # Smooth only stress variables
      smooth_terms <- if(length(vars$stress_vars) > 0) paste0("s(", vars$stress_vars, ")") else NULL
      linear_terms <- setdiff(vars$main_effects, vars$stress_vars)
      stress_interactions <- convert_interactions_to_gam(vars$interactions, vars, "by_terms")
      
      formula_rhs <- paste(c(smooth_terms, linear_terms, stress_interactions), collapse = " + ")
      
   } else if (strategy == 'smooth_tide') {
      # Smooth only stress variables
      smooth_terms <- if(length(vars$tide_vars) > 0) paste0("s(", vars$tide_vars, ")") else NULL
      linear_terms <- setdiff(vars$main_effects, vars$tide_vars)
      tide_interactions <- convert_interactions_to_gam(vars$interactions, vars, "by_terms")
      
      formula_rhs <- paste(c(smooth_terms, linear_terms, tide_interactions), collapse = " + ")
      
   } else if(strategy == "tensor" || strategy == "tensor_flow_stress" || strategy == 'tensor_flow_tide') {
      # Create strategic tensor products
      tensor_terms <- create_strategic_tensors(vars, "Salinity")
      
      # Get variables already handled by tensor terms
      tensor_vars <- unique(unlist(strsplit(gsub("te\\(|s\\(|\\)|,", " ", tensor_terms), "\\s+")))
      tensor_vars <- tensor_vars[tensor_vars != ""]
      
      # Remaining main effects as smooths (continuous) or linear (categorical)
      remaining_continuous <- setdiff(vars$continuous_vars, tensor_vars)
      remaining_categorical <- setdiff(vars$categorical_vars, tensor_vars)
      
      remaining_smooth <- if(length(remaining_continuous) > 0) paste0("s(", remaining_continuous, ")") else NULL
      
      # Handle remaining interactions
      remaining_interactions <- convert_interactions_to_gam(vars$interactions, vars, "parametric")
      
      formula_rhs <- paste(c(tensor_terms, remaining_smooth, remaining_categorical, remaining_interactions), 
                           collapse = " + ")
      
   } else if(strategy == "mixed_interactions") {
      # Strategic mix: smooth main effects, smart interaction conversion
      smooth_main <- paste0("s(", vars$continuous_vars, ")")
      parametric_main <- vars$categorical_vars
      mixed_interactions <- convert_interactions_to_gam(vars$interactions, vars, "smart")
      
      formula_rhs <- paste(c(smooth_main, parametric_main, mixed_interactions), collapse = " + ")
      
   } else {
      # Default: keep original formula
      formula_rhs <- paste(deparse(linear_formula[[3]]), collapse = "")
   }
   
   # Clean up formula string
   formula_rhs <- gsub("\\s+", " ", formula_rhs)  # Clean whitespace
   formula_rhs <- gsub("^\\s+|\\s+$", "", formula_rhs)  # Trim
   formula_rhs <- gsub("\\+\\s*\\+", "+", formula_rhs)  # Remove double +
   
   # Remove empty terms
   terms <- strsplit(formula_rhs, "\\s*\\+\\s*")[[1]]
   terms <- terms[terms != "" & !is.na(terms)]
   formula_rhs <- paste(terms, collapse = " + ")
   
   # Construct final formula
   gam_formula <- as.formula(paste(response_var, "~", formula_rhs))
   
   return(list(
      formula = gam_formula,
      vars = vars,
      strategy = strategy,
      formula_string = formula_rhs
   ))
}