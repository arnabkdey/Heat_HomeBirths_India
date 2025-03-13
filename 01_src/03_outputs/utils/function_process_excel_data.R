library(tidyverse)
library(openxlsx)

# Helper functions to process data to and from excel files ----

## Function to read all sheets from an Excel file
read_excel_sheets <- function(file_path) {
  # Get sheet names
  sheet_names <- excel_sheets(file_path)
  
  # Read all sheets, add sheet name as column, and combine
  sheet_names |>
    lapply(function(sheet) read_excel(file_path, sheet = sheet) |>
             mutate(Modifier = sheet)) |>
    bind_rows() |> 
    drop_na()
}

## Function to standardize modifier names
standardize_modifiers <- function(df) {
  # Create mapping for modifiers
  modifier_mapping <- c(
    "hh_religion_bi" = "A. Religion",
    "hh_caste_bi" = "B. Marginalized caste",
    "mat_edu_level_bi" = "C. Woman's education",
    "hh_wealth_poorest2" = "D. Household wealth",
    "rural" = "E. Residence",
    "hh_access_issue_distance" = "F. Distance is a big problem",
    "state_janani_bi" = "G. JSY high-focus state",
    "state_home_birth_bi" = "H. State home birth rate",
    "lt_mean_wbgt_tert_psu" = "I. Long-term temperature"
  )
  
  # Apply mapping
  df$Modifier <- modifier_mapping[df$Modifier]
  
  return(df)
}

## Function to standardize modifier levels
standardize_levels <- function(df) {
  # Create mapping for modifier levels
  level_mapping <- c(
    "urban" = "Urban",
    "rural" = "Rural",
    "big-problem" = "Yes",
    "not-a-big-prob" = "No",
    "1" = "Cooler",
    "2" = "Medium",
    "3" = "Warmer",
    "JSY" = "Yes",
    "Non-JSY" = "No",
    "LowHB" = "Low",
    "Medium_or_High_HB" = "High",
    "primary or higher" = "Some",
    "no education" = "None",
    "poorer2" = "Poorer",
    "richer3" = "Richer",
    "not-hindu" = "Non-hindu",
    "hindu" = "Hindu",
    "marg" = "Yes",
    "general" = "No"
  )
  
  # Apply mapping
  df$ModifierLevel <- level_mapping[df$ModifierLevel]
  
  return(df)
}

## function to standardize exposures
standardize_exposures <- function(df) {
  # Create mapping for exposures
  exposure_mapping <- c(
    "hotday_db_80" = "DBT >= 80th percentile, 1-day",
    "hotday_wb_80" = "WBGT >= 80th percentile, 1-day",
    "hw_db_80_2d" = "DBT >= 80th percentile, 2-days",
    "hw_wb_80_2d" = "WBGT >= 80th percentile, 2-days",
    "hw_db_80_3d" = "DBT >= 80th percentile, 3-days",
    "hw_wb_80_3d" = "WBGT >= 80th percentile, 3-days",
    "hw_db_80_4d" = "DBT >= 80th percentile, 4-days",
    "hw_wb_80_4d" = "WBGT >= 80th percentile, 4-days",
    "hw_db_80_5d" = "DBT >= 80th percentile, 5-days",
    "hw_wb_80_5d" = "WBGT >= 80th percentile, 5-days",
    "hotday_db_825" = "DBT >= 82.5th percentile, 1-day",
    "hotday_wb_825" = "WBGT >= 82.5th percentile, 1-day",
    "hw_db_825_2d" = "DBT >= 82.5th percentile, 2-days",
    "hw_wb_825_2d" = "WBGT >= 82.5th percentile, 2-days",
    "hw_db_825_3d" = "DBT >= 82.5th percentile, 3-days",
    "hw_wb_825_3d" = "WBGT >= 82.5th percentile, 3-days",
    "hw_db_825_4d" = "DBT >= 82.5th percentile, 4-days",
    "hw_wb_825_4d" = "WBGT >= 82.5th percentile, 4-days",
    "hw_db_825_5d" = "DBT >= 82.5th percentile, 5-days",
    "hw_wb_825_5d" = "WBGT >= 82.5th percentile, 5-days",
    "hotday_db_85" = "DBT >= 85th percentile, 1-day",
    "hotday_wb_85" = "WBGT >= 85th percentile, 1-day",
    "hw_db_85_2d" = "DBT >= 85th percentile, 2-days",
    "hw_wb_85_2d" = "WBGT >= 85th percentile, 2-days",
    "hw_db_85_3d" = "DBT >= 85th percentile, 3-days",
    "hw_wb_85_3d" = "WBGT >= 85th percentile, 3-days",
    "hw_db_85_4d" = "DBT >= 85th percentile, 4-days",
    "hw_wb_85_4d" = "WBGT >= 85th percentile, 4-days",
    "hw_db_85_5d" = "DBT >= 85th percentile, 5-days",
    "hw_wb_85_5d" = "WBGT >= 85th percentile, 5-days",
    "hotday_db_875" = "DBT >= 87.5th percentile, 1-day",
    "hotday_wb_875" = "WBGT >= 87.5th percentile, 1-day",
    "hw_db_875_2d" = "DBT >= 87.5th percentile, 2-days",
    "hw_wb_875_2d" = "WBGT >= 87.5th percentile, 2-days",
    "hw_db_875_3d" = "DBT >= 87.5th percentile, 3-days",
    "hw_wb_875_3d" = "WBGT >= 87.5th percentile, 3-days",
    "hw_db_875_4d" = "DBT >= 87.5th percentile, 4-days",
    "hw_wb_875_4d" = "WBGT >= 87.5th percentile, 4-days",
    "hw_db_875_5d" = "DBT >= 87.5th percentile, 5-days",
    "hw_wb_875_5d" = "WBGT >= 87.5th percentile, 5-days",
    "hotday_db_90" = "DBT >= 90th percentile, 1-day",
    "hotday_wb_90" = "WBGT >= 90th percentile, 1-day",
    "hw_db_90_2d" = "DBT >= 90th percentile, 2-days",
    "hw_wb_90_2d" = "WBGT >= 90th percentile, 2-days",
    "hw_db_90_3d" = "DBT >= 90th percentile, 3-days",
    "hw_wb_90_3d" = "WBGT >= 90th percentile, 3-days",
    "hw_db_90_4d" = "DBT >= 90th percentile, 4-days",
    "hw_wb_90_4d" = "WBGT >= 90th percentile, 4-days",
    "hw_db_90_5d" = "DBT >= 90th percentile, 5-days",
    "hw_wb_90_5d" = "WBGT >= 90th percentile, 5-days",
    "hotday_db_925" = "DBT >= 92.5th percentile, 1-day",
    "hotday_wb_925" = "WBGT >= 92.5th percentile, 1-day",
    "hw_db_925_2d" = "DBT >= 92.5th percentile, 2-days",
    "hw_wb_925_2d" = "WBGT >= 92.5th percentile, 2-days",
    "hw_db_925_3d" = "DBT >= 92.5th percentile, 3-days",
    "hw_wb_925_3d" = "WBGT >= 92.5th percentile, 3-days",
    "hw_db_925_4d" = "DBT >= 92.5th percentile, 4-days",
    "hw_wb_925_4d" = "WBGT >= 92.5th percentile, 4-days",
    "hw_db_925_5d" = "DBT >= 92.5th percentile, 5-days",
    "hw_wb_925_5d" = "WBGT >= 92.5th percentile, 5-days",
    "hotday_db_95" = "DBT >= 95th percentile, 1-day",
    "hotday_wb_95" = "WBGT >= 95th percentile, 1-day",
    "hw_db_95_2d" = "DBT >= 95th percentile, 2-days",
    "hw_wb_95_2d" = "WBGT >= 95th percentile, 2-days",
    "hw_db_95_3d" = "DBT >= 95th percentile, 3-days",
    "hw_wb_95_3d" = "WBGT >= 95th percentile, 3-days",
    "hw_db_95_4d" = "DBT >= 95th percentile, 4-days",
    "hw_wb_95_4d" = "WBGT >= 95th percentile, 4-days",
    "hw_db_95_5d" = "DBT >= 95th percentile, 5-days",
    "hw_wb_95_5d" = "WBGT >= 95th percentile, 5-days"
  )
  
  # Apply mapping
  df$Exposure <- exposure_mapping[df$Exposure]
  
  return(df)
}

save_df_to_excel_by_column <- function(
  df, 
  file_path, 
  split_column, 
  pivot_wider = FALSE) {
  
  # Check if the column exists in the dataframe
  if (!split_column %in% names(df)) {
    stop(paste("Column", split_column, "not found in the dataframe"))
  }
  
  # Get unique values from the specified column
  unique_values <- unique(df[[split_column]])
  
  # Create a new workbook
  wb <- createWorkbook()
  
  # For each unique value, create a sheet and add the filtered data
  for (value in unique_values) {
    # Filter data for this value
    sheet_data <- df |> 
      filter(!!sym(split_column) == value)
    
    if (pivot_wider) {
      # Store the split column name
      split_col_name <- split_column
    
      sheet_data <- as.data.table(sheet_data) |> 
          unique(by = c("Exposure", "ModifierLevel")) |>
            melt(id.vars = c("Exposure", "ModifierLevel"), 
            measure.vars = c("OR", "CI_Lower", "CI_Upper")) |>
      dcast(Exposure ~ ModifierLevel + variable, value.var = "value")

      # Add back the split column value as a constant
      # 'value' is already available from the for loop
      sheet_data[[split_col_name]] <- value
  
      # Move the column to the beginning
      col_order <- c(split_col_name, setdiff(names(sheet_data), split_col_name))
      setcolorder(sheet_data, col_order)
    }

    # Create sheet name (use the value)
    sheet_name <- as.character(value)
    
    # Some characters are not allowed in Excel sheet names
    # Replace any problematic characters if needed
    sheet_name <- gsub("[\\[\\]\\*\\?\\:\\/\\\\]", "_", sheet_name)
    
    # Limit sheet name length to 31 characters (Excel limitation)
    if (nchar(sheet_name) > 31) {
      sheet_name <- substr(sheet_name, 1, 31)
    }
    
    # Add worksheet
    addWorksheet(wb, sheet_name)
    
    # Write data to worksheet
    writeData(wb, sheet_name, sheet_data)
    
    # Auto-fit columns for better readability
    setColWidths(wb, sheet_name, cols = 1:ncol(sheet_data), 
                 widths = "auto")
  }
  
  # Save the workbook
  saveWorkbook(wb, file_path, overwrite = TRUE)
  
  # Return success message
  return(paste("Excel file saved successfully at:", file_path, 
               "with sheets separated by", split_column))
}

