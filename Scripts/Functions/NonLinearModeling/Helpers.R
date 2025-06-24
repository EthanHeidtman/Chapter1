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
convert_interactions_to_gam <- function(interactions, vars, method = "smart", k_config) {
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
                                             flow_in_int, stress_in_int, time_in_int, tide_in_int, k_config)
         
      } else if(method == "tensor") {
         result <- convert_interaction_tensor(variables, continuous_in_int, categorical_in_int, k_config)
         
      } else if(method == "by_terms") {
         result <- convert_interaction_by_terms(variables, continuous_in_int, categorical_in_int, k_config)
         
      } else if(method == "smooth") {
         result <- convert_interaction_smooth(variables, continuous_in_int, k_config)
         
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
                                      flow_in_int, stress_in_int, time_in_int, tide_in_int, k_config) {
   
   # Priority: categorical-continuous interactions become 'by' terms (can't have terms as main predictors and 'by' terms)
   if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      # Choose the most important categorical variable for by term
      by_var <- categorical_in_int[1]  # Take first categorical
      k_val <- k_config$categorical_by
      
      if(length(continuous_in_int) == 1) {
         term <- paste0("s(", continuous_in_int[1], ", by = ", by_var, ", k = ", k_val, ")")
      } else {
         k_vals <- rep(k_val, length(continuous_in_int))
         k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", by = ", by_var, ", k = ", k_string, ")")
      }
      return(list(term = term, by_var = by_var))
      
   } else if(length(flow_in_int) > 0 && length(tide_in_int) > 0) {
      k_vals <- c(k_config$tensor_main, k_config$tensor_secondary)
      k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
      term <- paste0("te(", flow_in_int[1], ", ", tide_in_int[1], ", k = ", k_string, ")")
      return(list(term = term, by_var = NULL))
      
   } else if(length(continuous_in_int) >= 2) {
      k_vals <- rep(k_config$tensor_main, length(continuous_in_int))
      k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
      
      if(length(continuous_in_int) == 2) {
         term <- create_tensor_term(
            vars = continuous_in_int,
            k_vals = k_vals,
            bs = rep("tp", length(continuous_in_int)),
            by_var = NULL,
            k_config = k_config
         )
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", k = ", k_string, ")")
      }
      return(list(term = term, by_var = NULL))
      
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))
   }
}

# Helper function to create tensor products with k values
create_tensor_term <- function(vars, k_vals = NULL, bs = NULL, by_var = NULL, k_config = NULL) {
   if(is.null(k_vals) && !is.null(k_config)) {
      # Default k values for tensor products
      if(length(vars) == 2) {
         k_vals <- c(k_config$tensor_main, k_config$tensor_secondary)
      } else {
         k_vals <- rep(k_config$tensor_main, length(vars))
      }
   }
   
   # Ensure k_vals is not empty
   if(is.null(k_vals) || length(k_vals) == 0) {
      k_vals <- rep(10, length(vars))  # Fallback default
   }
   
   # Apply limits if k_config is available
   if(!is.null(k_config)) {
      k_vals <- pmax(k_config$min_k, pmin(k_vals, k_config$max_k))
   }
   
   # Default basis functions
   if(is.null(bs)) {
      bs <- rep("tp", length(vars))
   }
   
   # Build tensor term
   k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
   bs_string <- paste0("c('", paste(bs, collapse = "', '"), "')")
   
   if(!is.null(by_var)) {
      return(paste0("te(", paste(vars, collapse = ", "), 
                    ", k = ", k_string, ", bs = ", bs_string, 
                    ", by = ", by_var, ")"))
   } else {
      return(paste0("te(", paste(vars, collapse = ", "), 
                    ", k = ", k_string, ", bs = ", bs_string, ")"))
   }
}

# Helper function to create smooth terms with k values
create_smooth_term <- function(var, k_val = NULL, bs = "tp", by_var = NULL, k_config = NULL, var_types) {
   # Determine k value based on variable type if not specified
   if(is.null(k_val) && !is.null(k_config)) {
      if(!is.null(var_types)) {
         if(var %in% var_types$flow_vars) {
            k_val <- k_config$flow_vars
         } else if(var %in% var_types$stress_vars) {
            k_val <- k_config$stress_vars
         } else if(var %in% var_types$tide_vars) {
            k_val <- k_config$tidal_vars
         } else {
            k_val <- k_config$main_effects
         }
      } else {
         k_val <- k_config$main_effects
      }
   }
   
   # Fallback if still null
   if(is.null(k_val)) {
      k_val <- 10
   }
   
   # Apply k limits if k_config is available
   if(!is.null(k_config)) {
      k_val <- max(k_config$min_k, min(k_val, k_config$max_k))
   }
   
   # Build smooth term
   if(!is.null(by_var)) {
      # By-term: use interaction k value
      if(!is.null(k_config)) {
         k_val <- min(k_val, k_config$interactions)
      }
      return(paste0("s(", var, ", by = ", by_var, ", k = ", k_val, ", bs = '", bs, "')"))
   } else {
      return(paste0("s(", var, ", k = ", k_val, ", bs = '", bs, "')"))
   }
}


# Convert interaction to tensor product
convert_interaction_tensor <- function(variables, continuous_in_int, categorical_in_int, k_config) {
   if(length(continuous_in_int) >= 2) {
      k_vals <- rep(k_config$tensor_main, length(continuous_in_int))
      k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
      
      if(length(categorical_in_int) > 0) {
         by_var <- categorical_in_int[1]
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", by = ", by_var, ", k = ", k_string, ")")
         return(list(term = term, by_var = by_var))
      } else {
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", k = ", k_string, ")")
         return(list(term = term, by_var = NULL))
      }
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))
   }
}

# Convert interaction to by terms
convert_interaction_by_terms <- function(variables, continuous_in_int, categorical_in_int, k_config) {
   if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
      by_var <- categorical_in_int[1]
      k_val <- k_config$categorical_by
      
      if(length(continuous_in_int) == 1) {
         term <- paste0("s(", continuous_in_int[1], ", by = ", by_var, ", k = ", k_val, ")")
      } else {
         k_vals <- rep(k_val, length(continuous_in_int))
         k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", by = ", by_var, ", k = ", k_string, ")")
      }
      return(list(term = term, by_var = by_var))
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))
   }
}

# Convert interaction to smooth terms
convert_interaction_smooth <- function(variables, continuous_in_int, k_config) {
   if(length(continuous_in_int) >= 2) {
      k_val <- k_config$tensor_main
      
      if(length(continuous_in_int) == 2) {
         term <- paste0("s(", continuous_in_int[1], ", ", continuous_in_int[2], 
                        ", k = ", k_val, ")")
      } else {
         k_vals <- rep(k_val, length(continuous_in_int))
         k_string <- paste0("c(", paste(k_vals, collapse = ", "), ")")
         term <- paste0("te(", paste(continuous_in_int, collapse = ", "), 
                        ", k = ", k_string, ")")
      }
      return(list(term = term, by_var = NULL))
   } else {
      return(list(term = paste(variables, collapse = ":"), by_var = NULL))
   }
}

# Create strategic tensor products for key variable combinations
create_strategic_tensors <- function(vars, strategy_focus = "Salinity", k_config) {
   tensor_terms <- character(0)
   by_vars <- character(0)
   
   if(strategy_focus == "salinity") {
      # Key interactions for salinity modeling
      flow_vars <- vars$flow_vars
      stress_vars <- vars$stress_vars
      time_vars <- vars$time_vars
      tide_vars <- vars$tide_vars
      
      # Flow-Tide interaction (most critical for salinity)
      if(length(flow_vars) > 0 && length(tide_vars) > 0) {
         tensor_terms <- c(tensor_terms,
                           create_tensor_term(
                              vars = c(flow_vars[1], tide_vars[1]),
                              k_vals = c(k_config$tensor_main, k_config$tensor_secondary),
                              bs = c("tp", "tp"),
                              k_config = k_config
                           ))
      }
      
      # Flow-Time seasonal interaction
      if(length(flow_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms,
                           create_tensor_term(
                              vars = c(flow_vars[1], time_vars[1]),
                              k_vals = c(k_config$tensor_main, k_config$tensor_secondary),
                              bs = c("tp", "cc"),  # cc for cyclic time
                              k_config = k_config
                           ))
      }
      
      # Stress-Time seasonal interaction
      if(length(stress_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms,
                           create_tensor_term(
                              vars = c(stress_vars[1], time_vars[1]),
                              k_vals = c(k_config$tensor_main, k_config$tensor_secondary),
                              bs = c("tp", "cc"),  # cc for cyclic time
                              k_config = k_config
                           ))
      }
      
      # Three-way interaction if all types present (optional - can be computationally expensive)
      if(length(flow_vars) > 0 && length(tide_vars) > 0 && length(time_vars) > 0) {
         tensor_terms <- c(tensor_terms,
                           create_tensor_term(
                              vars = c(flow_vars[1], tide_vars[1], time_vars[1]),
                              k_vals = c(k_config$tensor_main, k_config$tensor_secondary, k_config$tensor_secondary),
                              bs = c("tp", "tp", "cc"),
                              k_config = k_config
                           ))
      }
   }
   
   return(list(terms = tensor_terms, by_vars = by_vars))
}