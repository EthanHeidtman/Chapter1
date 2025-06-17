# Helper functions to make sure interactions and other GAM terms are handled properly

# Function to group the different predictor type together
parse_linear_formula <- function(formula) {
   # Extract terms from the formula - this automatically expands * interactions
   terms_obj <- terms(formula)
   term_labels <- attr(terms_obj, "term.labels")
   
   # Get all variable names (including those in interactions)
   all_vars <- all.vars(formula)[-1]  # Remove response variable
   
   # Separate main effects from interactions
   main_effects <- term_labels[!grepl(":", term_labels)]
   interactions <- term_labels[grepl(":", term_labels)]
   
   # Additional parsing for your specific variables
   # Identify variable types based on naming patterns
   flow_vars <- main_effects[grepl("Discharge|Inflows", main_effects, ignore.case = TRUE)]
   stress_vars <- main_effects[grepl("Stress", main_effects, ignore.case = TRUE)]
   time_vars <- main_effects[grepl("Year|Season", main_effects, ignore.case = TRUE)]
   tide_vars <- main_effects[grepl("Tide", main_effects, ignore.case = TRUE)]
   categorical_vars <- main_effects[grepl('SalinitySeason|ConowingoStressed|IsHighStress|IsModerateStress|
                                          IsStressed|IsFlush|StressLevel|BasicRegime|TidePhase', main_effects, ignore.case = TRUE)]
   other_vars <- setdiff(main_effects, c(flow_vars, stress_vars, time_vars))
   continuous_vars <- setdiff(all_vars, categorical_vars)
   
   return(list(
      main_effects = main_effects,
      interactions = interactions,
      all_terms = term_labels,
      all_variables = all_vars,
      flow_vars = flow_vars,
      stress_vars = stress_vars,
      time_vars = time_vars,
      tide_vars = tide_vars,
      other_vars = other_vars,
      categorical_vars = categorical_vars,
      continuous_vars = continuous_vars
   ))
}

# Convert linear interactions to GAM terms based on variable types
convert_interactions_to_gam <- function(interactions, vars, method = "smart") {
   if(length(interactions) == 0) return(character(0))
   
   converted <- character(0)
   
   for(interaction in interactions) {
      variables <- strsplit(interaction, ":")[[1]]
      
      # Classify variables in this interaction
      continuous_in_int <- variables[variables %in% vars$continuous_vars]
      categorical_in_int <- variables[variables %in% vars$categorical_vars]
      flow_in_int <- variables[variables %in% vars$flow_vars]
      stress_in_int <- variables[variables %in% vars$stress_vars]
      time_in_int <- variables[variables %in% vars$time_vars]
      tide_in_int <- variables[variables %in% vars$tide_vars]
      
      if(method == "smart") {
         converted_term <- convert_interaction_smart(variables, continuous_in_int, categorical_in_int,
                                                     flow_in_int, stress_in_int, time_in_int, tide_in_int)
      } else if(method == "tensor") {
         converted_term <- convert_interaction_tensor(variables, continuous_in_int, categorical_in_int)
      } else if(method == "by_terms") {
         converted_term <- convert_interaction_by_terms(variables, continuous_in_int, categorical_in_int)
      } else if(method == "smooth") {
         converted_term <- convert_interaction_smooth(variables, continuous_in_int)
      } else {
         converted_term <- interaction  # Keep parametric
      }
      
      converted <- c(converted, converted_term)
   }
   
   return(converted)
}

# Smart interaction conversion based on variable types and salinity modeling knowledge
convert_interaction_smart <- function(variables, continuous_in_int, categorical_in_int, 
                                      flow_in_int, stress_in_int, time_in_int, tide_in_int) {
   
   # Priority interactions for salinity modeling:
   # 1. Flow-Tide interactions (most critical) -> tensor products
   # 2. Flow-Time interactions (seasonal patterns) -> tensor products  
   # 3. Stress-Time interactions (seasonal stress patterns) -> tensor products
   # 4. Categorical-Continuous interactions -> by terms
   # 5. Other continuous-continuous -> smooth interactions
   
   if(length(flow_in_int) > 0 && length(tide_in_int) > 0) {
      # Flow-tide interaction - critical for salinity
      if(length(variables) == 2) {
         return(paste0("te(", flow_in_int[1], ", ", tide_in_int[1], ")"))
      } else {
         return(paste0("te(", paste(variables, collapse = ", "), ")"))
      }
   } else if(length(flow_in_int) > 0 && length(time_in_int) > 0) {
      # Flow-time seasonal interaction
      return(paste0("te(", flow_in_int[1], ", ", time_in_int[1], ")"))
   } else if(length(stress_in_int) > 0 && length(time_in_int) > 0) {
      # Stress-time seasonal interaction
      return(paste0("te(", stress_in_int[1], ", ", time_in_int[1], ")"))
   } else if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      # Categorical-continuous interaction -> by terms
      if(length(continuous_in_int) == 1) {
         return(paste0("s(", continuous_in_int[1], ", by = ", categorical_in_int[1], ")"))
      } else {
         return(paste0("te(", paste(continuous_in_int, collapse = ", "), 
                       ", by = ", categorical_in_int[1], ")"))
      }
   } else if(length(continuous_in_int) >= 2) {
      # Multiple continuous variables -> tensor or smooth interaction
      if(length(variables) == 2) {
         return(paste0("s(", variables[1], ", ", variables[2], ")"))
      } else {
         return(paste0("te(", paste(variables, collapse = ", "), ")"))
      }
   } else {
      # Keep as parametric interaction
      return(paste(variables, collapse = ":"))
   }
}

# Convert interaction to tensor product
convert_interaction_tensor <- function(variables, continuous_in_int, categorical_in_int) {
   if(length(continuous_in_int) >= 2) {
      if(length(categorical_in_int) > 0) {
         return(paste0("te(", paste(continuous_in_int, collapse = ", "), 
                       ", by = ", categorical_in_int[1], ")"))
      } else {
         return(paste0("te(", paste(continuous_in_int, collapse = ", "), ")"))
      }
   } else {
      return(paste(variables, collapse = ":"))  # Keep parametric
   }
}

# Convert interaction to by terms
convert_interaction_by_terms <- function(variables, continuous_in_int, categorical_in_int) {
   if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      if(length(continuous_in_int) == 1) {
         return(paste0("s(", continuous_in_int[1], ", by = ", categorical_in_int[1], ")"))
      } else {
         return(paste0("te(", paste(continuous_in_int, collapse = ", "), 
                       ", by = ", categorical_in_int[1], ")"))
      }
   } else {
      return(paste(variables, collapse = ":"))  # Keep parametric
   }
}

#' Convert interaction to smooth terms
convert_interaction_smooth <- function(variables, continuous_in_int) {
   if(length(continuous_in_int) >= 2) {
      if(length(vars) == 2) {
         return(paste0("s(", vars[1], ", ", vars[2], ")"))
      } else {
         return(paste0("te(", paste(vars, collapse = ", "), ")"))
      }
   } else {
      return(paste(variables, collapse = ":"))  # Keep parametric
   }
}

# Create strategic tensor products for key variable combinations
create_strategic_tensors <- function(vars, strategy_focus = "Salinity") {
   tensor_terms <- character(0)
   
   if(strategy_focus == "Salinity") {
      # Key interactions for salinity modeling
      flow_vars <- vars$flow_vars
      stress_vars <- vars$stress_vars
      time_vars <- vars$time_vars
      tide_vars <- vars$tide_vars
      
      # Flow-Tide interaction (most critical for salinity)
      if(length(flow_vars) > 0 && length(tide_vars) > 0) {
         tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", tide_vars[1], ")"))
      }
      
      # Flow-Time seasonal interaction
      if(length(flow_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", time_vars[1], ")"))
      }
      
      # Stress-Time seasonal interaction
      if(length(stress_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms, paste0("te(", stress_vars[1], ", ", time_vars[1], ")"))
      }
      
      # Three-way interaction if all types present
      if(length(flow_vars) > 0 && length(tide_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", tide_vars[1], ", ", time_vars[1], ")"))
      }
   }
   
   return(tensor_terms)
}