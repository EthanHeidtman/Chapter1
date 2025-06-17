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
   by_variables <- character(0) # Track the variables used in "by" terms
   
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
         result <- convert_interaction_smart(variables, continuous_in_int, categorical_in_int,
                                             flow_in_int, stress_in_int, time_in_int, tide_in_int)
      } else if(method == "tensor") {
         result <- convert_interaction_tensor(variables, continuous_in_int, categorical_in_int)
      } else if(method == "by_terms") {
         result <- convert_interaction_by_terms(variables, continuous_in_int, categorical_in_int)
      } else if(method == "smooth") {
         result <- convert_interaction_smooth(variables, continuous_in_int)
      } else {
         result <- list(term = interaction, by_var = NULL)  # Keep parametric
      }
      
      converted <- c(converted, result$term)
      if(!is.null(result$by_var)) {
         by_variables <- c(by_variables, result$by_var)
      }
   }
   
   return(list(terms = converted, by_vars = unique(by_variables)))
}

# Smart interaction conversion based on variable types and salinity modeling knowledge
convert_interaction_smart <- function(variables, continuous_in_int, categorical_in_int, 
                                      flow_in_int, stress_in_int, time_in_int, tide_in_int) {
   
   # Priority: categorical-continuous interactions become by terms
   if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      # Choose the most important categorical variable for by term
      by_var <- categorical_in_int[1]  # Take first categorical
      
      if(length(continuous_in_int) == 1) {
         term <- paste0("s(", continuous_in_int[1], ", by = ", by_var, ")")
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), ", by = ", by_var, ")")
      }
      return(list(term = term, by_var = by_var))
      
   } else if(length(flow_in_int) > 0 && length(tide_in_int) > 0) {
      # Flow-tide interaction - critical for salinity
      if(length(variables) == 2) {
         term <- paste0("te(", flow_in_int[1], ", ", tide_in_int[1], ")")
      } else {
         term <- paste0("te(", paste(variables, collapse = ", "), ")")
      }
      return(list(term = term, by_var = NULL))
      
   } else if(length(flow_in_int) > 0 && length(time_in_int) > 0) {
      # Flow-time seasonal interaction
      term <- paste0("te(", flow_in_int[1], ", ", time_in_int[1], ")")
      return(list(term = term, by_var = NULL))
      
   } else if(length(stress_in_int) > 0 && length(time_in_int) > 0) {
      # Stress-time seasonal interaction
      term <- paste0("te(", stress_in_int[1], ", ", time_in_int[1], ")")
      return(list(term = term, by_var = NULL))
      
   } else if(length(continuous_in_int) >= 2) {
      # Multiple continuous variables -> tensor or smooth interaction
      if(length(variables) == 2) {
         term <- paste0("s(", variables[1], ", ", variables[2], ")")
      } else {
         term <- paste0("te(", paste(variables, collapse = ", "), ")")
      }
      return(list(term = term, by_var = NULL))
      
   } else {
      # Keep as parametric interaction
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))
   }
}

# Convert interaction to tensor product
convert_interaction_tensor <- function(variables, continuous_in_int, categorical_in_int) {
   if(length(continuous_in_int) >= 2) {
      if(length(categorical_in_int) > 0) {
         by_var <- categorical_in_int[1]
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), ", by = ", by_var, ")")
         return(list(term = term, by_var = by_var))
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), ")")
         return(list(term = term, by_var = NULL))
      }
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))  # Keep parametric
   }
}

# Convert interaction to by terms
convert_interaction_by_terms <- function(variables, continuous_in_int, categorical_in_int) {
   if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      by_var <- categorical_in_int[1]
      if(length(continuous_in_int) == 1) {
         term <- paste0("s(", continuous_in_int[1], ", by = ", by_var, ")")
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), ", by = ", by_var, ")")
      }
      return(list(term = term, by_var = by_var))
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))  # Keep parametric
   }
}

# Convert interaction to smooth terms
convert_interaction_smooth <- function(variables, continuous_in_int) {
   if(length(continuous_in_int) >= 2) {
      if(length(continuous_in_int) == 2) {
         term <- paste0("s(", continuous_in_int[1], ", ", continuous_in_int[2], ")")
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), ")")
      }
      return(list(term = term, by_var = NULL))
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))  # Keep parametric
   }
}

# Create strategic tensor products for key variable combinations
create_strategic_tensors <- function(vars, strategy_focus = "Salinity") {
   tensor_terms <- character(0)
   by_vars <- character(0)
   
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
   
   return(list(terms = tensor_terms, by_vars = by_vars))
}