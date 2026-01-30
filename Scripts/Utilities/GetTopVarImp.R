# Function to collect the top variables from each group
get_top_vars_by_group <- function(importance_df, group_dfs, n_top = 2, 
                                  importance_col = "IncMSE_OOB",
                                  show_importance = TRUE) {
   
   # Average importance across folds
   avg_importance <- importance_df %>%
      group_by(Variable) %>%
      summarise(avg_imp = mean(.data[[importance_col]], na.rm = TRUE)) %>%
      arrange(desc(avg_imp))
   
   # Function to get top n vars from a single group
   get_top_from_group <- function(group_df, n) {
      group_vars <- setdiff(colnames(group_df), "Salinity")
      
      group_importance <- avg_importance %>%
         filter(Variable %in% group_vars) %>%
         slice_head(n = n)
      
      if (show_importance) {
         return(group_importance)
      } else {
         return(group_importance$Variable)
      }
   }
   
   # Handle different input types for n_top
   if (is.list(n_top)) {
      # User provided specific n for each group
      top_vars <- mapply(get_top_from_group, 
                         group_dfs, 
                         n_top[names(group_dfs)],
                         SIMPLIFY = FALSE)
   } else {
      # Use same n for all groups
      top_vars <- lapply(group_dfs, get_top_from_group, n = n_top)
   }
   
   names(top_vars) <- names(group_dfs)
   return(top_vars)
}