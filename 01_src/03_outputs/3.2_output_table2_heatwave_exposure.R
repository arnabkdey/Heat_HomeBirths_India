# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates a descriptive table summarizing the percent of births under different combinations of percentile thresholds and durations
# @date: March 2025

# load libraries ----
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here, gt)

# set paths ----
source(here("paths.R"))

# load data ----
df_paper_final <- readRDS(here(path_project, "data", "processed_data", "1.3.1_final_data_for_paper.rds"))

# set constants ----
## list exposure variables
varlist_exposure_all <- c(
  ## Wet bulb temperature (percentile)
  "hotday_wb_80", "hw_wb_80_2d", "hw_wb_80_3d", "hw_wb_80_4d", "hw_wb_80_5d",
  "hotday_wb_825", "hw_wb_825_2d", "hw_wb_825_3d", "hw_wb_825_4d", "hw_wb_825_5d",
  "hotday_wb_85", "hw_wb_85_2d", "hw_wb_85_3d", "hw_wb_85_4d", "hw_wb_85_5d",
  "hotday_wb_875", "hw_wb_875_2d", "hw_wb_875_3d", "hw_wb_875_4d", "hw_wb_875_5d",
  "hotday_wb_90", "hw_wb_90_2d", "hw_wb_90_3d", "hw_wb_90_4d", "hw_wb_90_5d",
  "hotday_wb_925", "hw_wb_925_2d", "hw_wb_925_3d", "hw_wb_925_4d", "hw_wb_925_5d",
  "hotday_wb_95", "hw_wb_95_2d", "hw_wb_95_3d", "hw_wb_95_4d", "hw_wb_95_5d",
  ## Dry bulb temperature (percentile)
  "hotday_db_80", "hw_db_80_2d", "hw_db_80_3d", "hw_db_80_4d", "hw_db_80_5d",
  "hotday_db_825", "hw_db_825_2d", "hw_db_825_3d", "hw_db_825_4d", "hw_db_825_5d",
  "hotday_db_85", "hw_db_85_2d", "hw_db_85_3d", "hw_db_85_4d", "hw_db_85_5d",
  "hotday_db_875", "hw_db_875_2d", "hw_db_875_3d", "hw_db_875_4d", "hw_db_875_5d",
  "hotday_db_90", "hw_db_90_2d", "hw_db_90_3d", "hw_db_90_4d", "hw_db_90_5d",
  "hotday_db_925", "hw_db_925_2d", "hw_db_925_3d", "hw_db_925_4d", "hw_db_925_5d",
  "hotday_db_95", "hw_db_95_2d", "hw_db_95_3d", "hw_db_95_4d", "hw_db_95_5d"
)

## Convert list of exposure variables to factor 
df_paper_final <- df_paper_final |> 
  mutate(across(all_of(varlist_exposure_all), as.factor)) 

# create dataframe for output ----
## dataframe with the exposure variables as rows 
output_df <- data.frame(varlist_exposure_all)

## create columns to identify wet bulb and dry bulb temperature
output_df$exposure_type <- ifelse(grepl("wb", output_df$varlist_exposure_all), "Wet Bulb Globe Temperature (WBGT)", "Dry Bulb Temperature")

## Create column to identify percentile threshold
output_df <- output_df |>
  mutate(percentile = case_when(
    grepl("80", varlist_exposure_all) ~ "80",
    grepl("825", varlist_exposure_all) ~ "82.5",
    grepl("85", varlist_exposure_all) ~ "85",
    grepl("875", varlist_exposure_all) ~ "87.5",
    grepl("90", varlist_exposure_all) ~ "90",
    grepl("925", varlist_exposure_all) ~ "92.5",
    grepl("95", varlist_exposure_all) ~ "95"
  ))

## Create column to identify duration of heatwave
output_df <- output_df |>
  mutate(duration = case_when(
    grepl("2d", varlist_exposure_all) ~ "2 days",
    grepl("3d", varlist_exposure_all) ~ "3 days",
    grepl("4d", varlist_exposure_all) ~ "4 days",
    grepl("5d", varlist_exposure_all) ~ "5 days",
    TRUE ~ "1 day"
  ))

## Get mean of cases in df_paper_final for each exposure variable using apply ----
### Create a function to get the mean of cases for each exposure variable
get_mean_cases <- function(exposure_var) {
  mean_cases <- df_paper_final |> 
    filter(.data[[exposure_var]] == 1) |> 
    pull(dv_home_del_num) |> 
    mean(na.rm = TRUE)
  
  return(mean_cases)
}

### Apply the function to get the mean of cases for each exposure variable
output_df$mean_cases <- apply(output_df, 1, function(x) get_mean_cases(x[1]))

### conver to percent
output_df$percent_births <- round(output_df$mean_cases * 100, 2)

## Create column for labels
output_df$heatwave <- ifelse(grepl("hw", output_df$varlist_exposure_all), "Heatwave", "Hotday")
output_df$label <- paste0(output_df$heatwave, " (", output_df$percentile, "th percentile, ", output_df$duration, ")")
head(output_df)


# Create table ----
## Reshape the data
table_data <- output_df |>
  # Create a row identifier to keep track of which rows belong together
  mutate(row_id = row_number()) |>
  # Select only columns we need for analysis
  select(row_id, varlist_exposure_all, exposure_type, duration, percentile, heatwave, percent_births) |>
  # If these are not already in long format, we would need to use pivot_longer
  # This assumes your data is already one row per combination
  
  # Create a better label for heat metrics based on the heatwave and percentile columns
  mutate(
    heat_metric = paste0(percentile, "th percentile")
  ) |>
  # Pivot to get durations as columns
  pivot_wider(
    id_cols = c(exposure_type, heat_metric, percentile),
    names_from = duration,
    values_from = percent_births,
    values_fill = list(percent_births = NA)
  )
  
## Create the gt table
heat_table <- table_data |>
  gt(groupname_col = "exposure_type") |>  # Use exposure_type as the grouping variable
  # Add a title and subtitle
  # tab_header(
  #   title = "Percent of Births under different combinations of percentile thresholds and durations",
  #   subtitle = "Percent of Births under different combinations of percentile thresholds and durations"
  # ) |>
  # Format the values as decimals without % signs
  fmt_number(
    columns = matches("day"),
    decimals = 1
  ) |>
  # Replace NA values with em-dashes
  fmt_missing(
    columns = everything(),
    missing_text = "—"
  ) |>
  # Hide the percentile column since it's incorporated into the heat_metric
  cols_hide(columns = c(percentile)) |>
  # Rename the heat_metric column to "Percentile Thresholds"
  cols_label(
    heat_metric = html("Percentile<br>Thresholds")
  ) |>
  # set column width
  cols_width(
    heat_metric ~ px(150),
    matches("day") ~ px(80)
  ) |>
  # Style the table
  tab_style(
    style = list(
      cell_fill(color = "#f7f7f7"),
      cell_text(align = "center")
      # cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

# save table ----
gt::gtsave(heat_table, file = here(
  path_project, "outputs", "tables", "table2_heat_exposure.png"))
