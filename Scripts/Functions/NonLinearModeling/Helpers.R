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
      categorical_vars = categorical_vars
   ))
}

convert_interactions_to_gam <- function(linear_model, interactions, method = "smooth") {
   if(length(interactions) == 0) return(NULL)
   
   # Initialize
   converted <- character(0)
   
   # Identify different types of predictors
   all_vars <- linear_terms$all_variables
   flow_vars <- linear_terms$flow_vars
   stress_vars <- linear_terms$stress_vars
   time_vars <- linear_terms$time_vars
   tide_vars <- linear_terms$tide_vars
   other_vars <- linear_terms$other_vars
   categorical_vars <- linear_terms$categorical_vars
   continuous_vars <- setdiff(all_vars, categorical_vars)
   
   for(interaction in interactions) {
      vars <- strsplit(interaction, ":")[[1]]
      
      if(method == "smart") {
         # Smart conversion based on variable types
         continuous_in_int <- vars[vars %in% continuous_vars]
         categorical_in_int <- vars[vars %in% categorical_vars]
         
         if(length(continuous_in_int) >= 2 && length(categorical_in_int) == 0) {
            # All continuous: use smooth interaction
            if(length(vars) == 2) {
               converted <- c(converted, paste0("s(", vars[1], ", ", vars[2], ")"))
            } else if(length(vars) == 3) {
               converted <- c(converted, paste0("te(", paste(vars, collapse = ", "), ")"))
            } else {
               converted <- c(converted, interaction)  # Keep parametric for higher order
            }
         } else if(length(categorical_in_int) > 0 && length(continuous_in_int) > 0) {
            # Mixed: use "by" parameter for categorical-continuous interactions
            if(length(continuous_in_int) == 1) {
               by_var <- categorical_in_int[1]
               smooth_var <- continuous_in_int[1]
               converted <- c(converted, paste0("s(", smooth_var, ", by = ", by_var, ")"))
            } else {
               # Multiple continuous with categorical: tensor with by
               converted <- c(converted, paste0("te(", paste(continuous_in_interaction, collapse = ", "), 
                                                ", by = ", categorical_in_interaction[1], ")"))
            }
         } else {
            # All categorical or other cases: keep parametric
            converted <- c(converted, interaction)
         }
         
      } else if(method == "mixed_flow") {
         flow_vars <- args$flow_vars %||% character(0)
         categorical_vars <- args$categorical_vars %||% character(0)
         
         if(any(vars %in% flow_vars) && any(vars %in% categorical_vars)) {
            # Flow-categorical interaction: use "by" parameter
            flow_var <- vars[vars %in% flow_vars][1]
            cat_var <- vars[vars %in% categorical_vars][1]
            converted <- c(converted, paste0("s(", flow_var, ", by = ", cat_var, ")"))
         } else if(sum(vars %in% flow_vars) >= 2) {
            # Flow-flow interaction: smooth
            flow_vars_in_int <- vars[vars %in% flow_vars]
            converted <- c(converted, paste0("s(", paste(flow_vars_in_int, collapse = ", "), ")"))
         } else {
            converted <- c(converted, interaction)
         }
         
      } else if(method == "mixed_time") {
         time_vars <- args$time_vars %||% character(0)
         categorical_vars <- args$categorical_vars %||% character(0)
         
         if(any(vars %in% time_vars) && any(vars %in% categorical_vars)) {
            # Time-categorical interaction: use "by" parameter
            time_var <- vars[vars %in% time_vars][1]
            cat_var <- vars[vars %in% categorical_vars][1]
            converted <- c(converted, paste0("s(", time_var, ", by = ", cat_var, ")"))
         } else {
            converted <- c(converted, interaction)
         }
         
      } else {
         # Default smooth conversion
         if(length(vars) == 2) {
            converted <- c(converted, paste0("s(", vars[1], ", ", vars[2], ")"))
         } else if(length(vars) >= 3) {
            converted <- c(converted, paste0("te(", paste(vars, collapse = ", "), ")"))
         } else {
            converted <- c(converted, interaction)
         }
      }
   }
   
   return(converted)
}

create_strategic_tensors <- function(flow_vars, stress_vars, time_vars, other_vars) {
   tensor_terms <- character(0)
   
   # For your model: IsHighStress * Norm_StressHours_30day_Marietta * DayOfYear
   # The key insight is that stress and time interact in complex ways
   
   # Time-based tensor (DayOfYear with continuous stress measure if available)
   stress_continuous <- setdiff(c(flow_vars, other_vars), stress_vars)
   if(length(time_vars) > 0 && length(stress_continuous) > 0) {
      tensor_terms <- c(tensor_terms, paste0("te(", time_vars[1], ", ", stress_continuous[1], ")"))
   }
   
   # Flow-time seasonal interaction
   if(length(flow_vars) > 0 && length(time_vars) > 0) {
      tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", time_vars[1], ")"))
   }
   
   return(tensor_terms)
}

create_by_interactions <- function(interactions, categorical_vars, continuous_vars) {
   by_terms <- character(0)
   
   for(interaction in interactions) {
      vars <- strsplit(interaction, ":")[[1]]
      
      cat_vars_in_int <- vars[vars %in% categorical_vars]
      cont_vars_in_int <- vars[vars %in% continuous_vars]
      
      if(length(cat_vars_in_int) > 0 && length(cont_vars_in_int) > 0) {
         # Create "by" interactions for categorical-continuous combinations
         for(cat_var in cat_vars_in_int) {
            if(length(cont_vars_in_int) == 1) {
               by_terms <- c(by_terms, paste0("s(", cont_vars_in_int[1], ", by = ", cat_var, ")"))
            } else {
               # Multiple continuous variables with categorical
               by_terms <- c(by_terms, paste0("te(", paste(cont_vars_in_int, collapse = ", "), 
                                              ", by = ", cat_var, ")"))
            }
         }
      }
   }
   
   return(unique(by_terms))
}

filter_parametric_interactions <- function(interactions, by_interactions) {
   # Return interactions that should remain parametric (not handled by "by" terms)
   parametric <- character(0)
   
   for(interaction in interactions) {
      # Check if this interaction is already handled by a "by" term
      vars <- strsplit(interaction, ":")[[1]]
      
      # Simple heuristic: if interaction involves only categorical variables, keep parametric
      # You might need to adjust this based on your specific needs
      if(length(vars) <= 2) {
         # For now, let most 2-way interactions be handled by "by" terms
         # Keep higher-order or special cases parametric
      } else {
         parametric <- c(parametric, interaction)
      }
   }
   
   return(parametric)
}

create_tensor_terms <- function(flow_vars, tidal_vars, time_vars) {
   tensor_terms <- character(0)
   
   # Flow-Tidal interactions (most important for salinity)
   if(length(flow_vars) > 0 && length(tidal_vars) > 0) {
      tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", tidal_vars[1], ")"))
   }
   
   # Flow-Time interactions (seasonal patterns)
   if(length(flow_vars) > 0 && length(time_vars) > 0) {
      tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", time_vars[1], ")"))
   }
   
   # Three-way interaction if all variable types present
   if(length(flow_vars) > 0 && length(tidal_vars) > 0 && length(time_vars) > 0) {
      tensor_terms <- c(tensor_terms, paste0("te(", flow_vars[1], ", ", tidal_vars[1], ", ", time_vars[1], ")"))
   }
   
   return(tensor_terms)
}

identify_key_interactions <- function(interactions, flow_vars, tidal_vars) {
   if(length(interactions) == 0) return(list(tensor = character(0), parametric = character(0)))
   
   tensor_interactions <- character(0)
   parametric_interactions <- character(0)
   
   for(interaction in interactions) {
      vars <- strsplit(interaction, ":")[[1]]
      
      # Key interactions for tensor products
      has_flow <- any(vars %in% flow_vars)
      has_tidal <- any(vars %in% tidal_vars)
      
      if(has_flow && has_tidal && length(vars) <= 3) {
         # Flow-tidal interactions as tensor products
         tensor_interactions <- c(tensor_interactions, paste(vars, collapse = ", "))
      } else {
         # Keep as parametric
         parametric_interactions <- c(parametric_interactions, interaction)
      }
   }
   
   return(list(
      tensor = tensor_interactions,
      parametric = parametric_interactions
   ))
}

extract_tensor_vars <- function(tensor_term) {
   # Extract variable names from tensor term
   vars_string <- gsub("te\\(|\\)", "", tensor_term)
   return(strsplit(vars_string, ",\\s*")[[1]])
}

filter_remaining_interactions <- function(interactions, tensor_terms) {
   if(length(tensor_terms) == 0) return(interactions)
   
   # Get variables already handled by tensor terms
   tensor_vars <- unlist(lapply(tensor_terms, extract_tensor_vars))
   
   # Filter out interactions already handled
   remaining <- character(0)
   for(interaction in interactions) {
      vars <- strsplit(interaction, ":")[[1]]
      if(!all(vars %in% tensor_vars)) {
         remaining <- c(remaining, interaction)
      }
   }
   
   return(remaining)
}
