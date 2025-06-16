# Function to build the GAM formula depending on the identified strategy
build_gam_formula <- function(linear_formula, linear_predictors, strategy) {
   
   response_var <- all.vars(linear_formula)[1]
   
   # Parse the original linear formula to extract interactions
   linear_terms <- parse_linear_formula(linear_formula)
   main_effects <- linear_terms$main_effects
   interactions <- linear_terms$interactions
   
   # Identify different types of predictors
   flow_vars <- linear_terms$flow_vars
   stress_vars <- linear_terms$stress_vars
   time_vars <- linear_terms$time_vars
   tide_vars <- linear_terms$tide_vars
   other_vars <- linear_terms$other_vars
   categorical_vars <- linear_terms$categorical_vars
   
   # Build formula based on strategy
   if(strategy == "linear" || strategy == "baseline") {
      # Reproduce linear model exactly (including interactions)
      formula_rhs <- paste(deparse(linear_formula[[3]]), collapse = "")
      
   } else if(strategy == "smooth_all") {
      # Smooth all main effects, convert interactions to smooth interactions
      smooth_main <- paste0("s(", main_effects, ")")
      smooth_interactions <- convert_interactions_to_gam(interactions, "smooth")
      formula_rhs <- paste(c(smooth_main, smooth_interactions), collapse = " + ")
      
   } else if(strategy == "smooth_flow") {
      # Smooth flow variables, keep others linear
      smooth_flow <- if(length(flow_vars) > 0) paste0("s(", flow_vars, ")") else NULL
      linear_others <- c(stress_vars, time_vars, other_vars)
      
      # Handle interactions involving flow variables
      flow_interactions <- convert_interactions_to_gam(interactions, "mixed", flow_vars)
      other_interactions <- interactions[!interactions %in% flow_interactions]
      
      formula_rhs <- paste(c(smooth_flow, linear_others, flow_interactions, other_interactions), 
                           collapse = " + ")
      
   } else if(strategy == "smooth_stress") {
      # Smooth stress variables, keep others linear
      smooth_stress <- if(length(stress_vars) > 0) paste0("s(", stress_vars, ")") else NULL
      linear_others <- c(flow_vars, time_vars, other_vars)
      
      # Handle interactions involving tidal variables
      stress_interactions <- convert_interactions_to_gam(interactions, "mixed", stress_vars)
      other_interactions <- interactions[!interactions %in% stress_interactions]
      
      formula_rhs <- paste(c(smooth_stress, linear_others, stress_interactions, other_interactions), 
                           collapse = " + ")
      
   } else if(strategy == "tensor") {
      # Create tensor products for key variable combinations
      tensor_terms <- create_tensor_terms(flow_vars, stress_vars, time_vars)
      
      # Add remaining main effects
      remaining_vars <- setdiff(main_effects, unlist(lapply(tensor_terms, extract_tensor_vars)))
      remaining_smooth <- if(length(remaining_vars) > 0) paste0("s(", remaining_vars, ")") else NULL
      
      # Handle remaining interactions
      remaining_interactions <- filter_remaining_interactions(interactions, tensor_terms)
      
      formula_rhs <- paste(c(tensor_terms, remaining_smooth, remaining_interactions, other_vars), 
                           collapse = " + ")
      
   } else if(strategy == "mixed_interactions") {
      # Strategic mix of smooth terms and tensor products for interactions
      # Main effects as smooths
      smooth_main <- paste0("s(", main_effects, ")")
      
      # Key interactions as tensor products, others as parametric
      key_interactions <- identify_key_interactions(interactions, flow_vars, stress_vars)
      tensor_interactions <- paste0("te(", key_interactions$tensor, ")")
      parametric_interactions <- key_interactions$parametric
      
      formula_rhs <- paste(c(smooth_main, tensor_interactions, parametric_interactions), 
                           collapse = " + ")
   }
   
   # Clean up formula string and construct final formula
   formula_rhs <- gsub("\\s+", " ", formula_rhs)  # Clean up whitespace
   formula_rhs <- gsub("^\\s+|\\s+$", "", formula_rhs)  # Trim
   
   gam_formula <- as.formula(paste(response_var, "~", formula_rhs))
   
   return(gam_formula)
}