count_duplicates <- function(df) {
  # Ensure input is a dataframe
  if(!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  # Add 'Nomenclature' column which is the number of duplicates
  df <- df %>%
    group_by(across(everything())) %>%
    mutate(Nomenclature = n()) %>%
    ungroup()
  
  df$Nomenclature <- paste0('x', as.character(df$Nomenclature))
  df <- df %>% select(Nomenclature, everything())
  
  
  # Remove duplicates
  df_unique <- df %>%
    distinct(across(everything()), .keep_all = TRUE)
  
  
  
  return(df_unique)
}
create_fix_cols_df <- function(df2) {
  # Ensure input is a dataframe
  if (!is.data.frame(df2)) {
    stop("Input must be a data frame.")
  }
  
  # Initialize a list to temporarily store our columns
  fix_cols_list <- list()
  
  # Loop over all pairs of columns in df
  for (i in seq(1, ncol(df2), by = 2)) {
    # Get the factor column and the corresponding management column
    factor_col <- df2[, i]
    management_col <- df2[, i + 1]
    
    # If management is 'Fix', add column to the list
    if(any(management_col == 'Fix')) {
      fix_cols_list[[paste0(names(df2)[i], " (Fix)")]] <- ifelse(management_col == 'Fix', as.character(factor_col), NA)
    }
  }
  
  # Convert the list to a dataframe
  fix_df <- as.data.frame(fix_cols_list, stringsAsFactors = FALSE)
  
  return(drop_na(fix_df))
}
create_log_cols <- function(df2) {
  # Ensure input is a dataframe
  if (!is.data.frame(df2)) {
    stop("Input must be a data frame.")
  }
  
  # Initialize a list to temporarily store our columns
  log_cols_list <- list()
  
  # Loop over all pairs of columns in df
  for (i in seq(1, ncol(df2), by = 2)) {
    # Get the factor column and the corresponding management column
    factor_col <- df2[, i]
    management_col <- df2[, i + 1]
    
    # If management is neither 'Fix' nor 'Demo', add column to the list
    if(!any(management_col %in% c('Fix', 'Demo'))) {
      log_cols_list[[paste0(names(df2)[i], " (Log)")]] <- rep('Log', length(factor_col))
    }
  }
  
  # Convert the list to a dataframe
  log_df <- as.data.frame(log_cols_list, stringsAsFactors = FALSE)
  
  return(log_df)
}
combine_dataframes <- function(df, df_fix, df_log) {
  # Ensure input is a dataframe
  if(!is.data.frame(df) | !is.data.frame(df_fix) | !is.data.frame(df_log)) {
    stop("Input must be a data frame.")
  }
  
  # Replicate rows in df_fix and df_log to match the number of rows in df
  num_rows <- nrow(df)
  df_fix_rep <- df_fix[rep(seq_len(nrow(df_fix)), length.out=num_rows), , drop = FALSE]
  df_log_rep <- df_log[rep(seq_len(nrow(df_log)), length.out=num_rows), , drop = FALSE]
  
  # Combine the data frames
  df_combined <- cbind(df, df_fix_rep, df_log_rep)
  
  
  
  df_combined <- df_combined %>% select(Nomenclature, everything())
  
  return(df_combined)
}
create_demo_cols <- function(df) {
  # Ensure input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  # Initialize a list to temporarily store our columns
  demo_cols_list <- list()
  
  # Loop over all pairs of columns in df
  for (i in seq(1, ncol(df), by = 2)) {
    # Get the factor column and the corresponding management column
    factor_col <- df[, i]
    management_col <- df[, i + 1]
    
    # If management is 'Demo', add column to the list
    if(any(management_col == 'Demo')) {
      demo_cols_list[[paste0(names(df)[i], " (Demo)")]] <- ifelse(management_col == 'Demo', as.character(factor_col), NA)
    }
  }
  
  # Convert the list to a dataframe
  demo_df <- as.data.frame(demo_cols_list, stringsAsFactors = FALSE)
  
  return(demo_df)
}
logify_demo_cols <- function(df_combined, df_demo) {
  # Change column names in df_combined
  new_col_names <- sub(" \\(.*\\)", "", names(df_combined))
  names(df_combined) <- new_col_names
  
  # Determine columns in df_combined not present in df_demo
  new_cols <- setdiff(names(df_combined), names(df_demo))
  
  # For each new column, add it to df_demo with values set to 'Log'
  for (col in new_cols) {
    df_demo[[col]] <- rep("Log", nrow(df_demo))
  }
  
  # Initialize a new dataframe
  df_new <- data.frame()
  
  # Loop over each row
  for(i in 1:nrow(df_demo)) {
    row_data <- df_demo[i, ]
    
    # Find columns that have non-'Log' values
    non_log_cols <- names(row_data)[which(row_data != 'Log')]
    
    # If there is more than one non-'Log' value, duplicate rows
    if(length(non_log_cols) > 1) {
      for(col in non_log_cols) {
        new_row <- row_data
        new_row[which(names(new_row) != col)] <- 'Log'  # Replace other non-'Log' values with 'Log'
        df_new <- rbind(df_new, new_row)  # Append the new row to the dataframe
      }
    } else {
      # If there is only one or no non-'Log' value, keep the row as is
      df_new <- rbind(df_new, row_data)
    }
  }
  
  # Replace NA with 'Log'
  df_new <- df_new %>%
    mutate(across(everything(), ~replace_na(., "Log")))
  
  rownames(df_new) <- 1:nrow(df_new)
  
  return(df_new)
}
generate_dataframes <- function(test_design, other_factors) {
  
  # Apply the first three functions to your dataframe
  df <- count_duplicates(test_design)
  df_fix <- create_fix_cols_df(other_factors)
  df_log <- create_log_cols(other_factors)
  
  # Apply the function to your dataframes
  df_combined <- combine_dataframes(df, df_fix, df_log)

  # Apply the function to your dataframe
  demo_df <- create_demo_cols(other_factors)
  
  # Apply logify_demo_cols function
  df_demo <- logify_demo_cols(df_combined, demo_df)
  df_demo <- subset(df_demo, select = -c(Nomenclature))
  
  # Create a list to store both data frames
  dataframes <- list("characterize_df" = df_combined, "problemID_df" = df_demo)
  
  return(dataframes)
}
create_demo_and_log_cols <- function(df) {
  # Ensure input is a dataframe
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  # Initialize a list to temporarily store our columns
  demo_cols_list <- list()
  log_cols_list <- list()
  
  # Loop over all pairs of columns in df
  for (i in seq(1, ncol(df), by = 2)) {
    # Get the factor column and the corresponding management column
    factor_col <- df[, i]
    management_col <- df[, i + 1]
    
    # If management is 'Demo', add column to the demo list with the value of the factor level
    if(any(management_col == 'Demo')) {
      demo_cols_list[[paste0(names(df)[i], " (Demo)")]] <- ifelse(management_col == 'Demo', as.character(factor_col), NA)
    }
    # Else add column to the log list with the value "Log"
    else {
      log_cols_list[[names(df)[i]]] <- rep("Log", length(factor_col))
    }
  }
  
  # Convert the lists to a dataframe
  demo_df <- as.data.frame(demo_cols_list, stringsAsFactors = FALSE)
  log_df <- as.data.frame(log_cols_list, stringsAsFactors = FALSE)
  
  # Merge demo_df and log_df to get a combined dataframe
  combined_df <- cbind(demo_df, log_df)
  
  # Replace NA values with "Log"
  combined_df[is.na(combined_df)] <- "Log"
  
  # Ensure only one non-"Log" value per row
  non_log_count <- rowSums(combined_df != "Log")
  combined_df[non_log_count > 1] <- "Log"
  
  # Remove rows that only contain "Log"
  combined_df <- combined_df[rowSums(combined_df != "Log") > 0, ]
  return(combined_df)
}

