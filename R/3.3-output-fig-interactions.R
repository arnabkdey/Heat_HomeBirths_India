rm(list =ls())
options(scipen=999)
options(digits=5)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(sjPlot)
library(readxl)
library(here)
library(extrafont)
library(ggbreak)
library(patchwork)
# font_import() # run only once
# loadfonts(device="win") # run only once


# Function to read interaction data from excel and combine them ----
read_excel_sheets_and_combine <- function(file_path, effect_modifier_name) {
  # Reading sheet names from the Excel file
  sheets_to_read <- excel_sheets(file_path)
  
  # Reading each sheet, adding the sheet name and effect modifier
  combined_data <- lapply(sheets_to_read, function(sheet_name) {
    read_excel(file_path, sheet = sheet_name) %>%
      mutate(tabname = sheet_name) # Add the sheet name as a column
  }) %>%
    bind_rows() %>%
    mutate(effect_modifier = effect_modifier_name) # Add the effect modifier
  
  return(combined_data)
}

# Run the function for all the effect modifiers ----
here_output_files <- here("3-outputs", "models", "models-with-interaction")
caste <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-caste.xlsx"), "Caste")
religion <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-religion.xlsx"), "Religion")
residence <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-rural.xlsx"), "Residence")
wealth <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-wealth.xlsx"), "Wealth Quintile")
access_issue_distance <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-access_issue_distance.xlsx"), "Distance is a big problem to access healthcare")
lt_tmax_mean <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-lt_tmax_mean.xlsx"), "Long-term mean temperature tertiles")

# Process data for plots ----
## Combine all the data ----
data <- rbind(caste, religion, residence, wealth, access_issue_distance, lt_tmax_mean)
## Filter and transform data for specified contrasts, calculate OR and CI, and retain relevant columns.
data <- data %>% 
  filter(contrast %in% c("OBC", "SC", "ST", "Other", 
                         "Hindu", "Not-Hindu",
                         "Rural", "Urban",
                         "Poorest", "Poorer", "Middle", "Richer", "Richest",
                         "big-problem", "not-a-big-problem",
                         "Lowest_Tertile", "Medium_Tertile", "High_Tertile")) %>% 
  rename(exposure = tabname)  

## Create variables for threshold, threshold type, and duration ----
unique(data$exposure)
data <- data |> 
  ## First extarct the first two numeric values from the exposure column as the threshold
  dplyr::mutate(tmp_threshold = str_extract(exposure, "[0-9]+")) |> 
  ## Extract the type of threshold
  dplyr::mutate(tmp_threshold_type = ifelse(str_detect(exposure, "doy"), "doy", 
                                        ifelse(str_detect(exposure, "harmo"), "harmo", "absolute"))) |>
  ## Extract the duration
  dplyr::mutate(duration = case_when(str_detect(exposure, "2d") ~ " for 2+ days",
                                    str_detect(exposure, "3d") ~ " for 3+ days",
                                    str_detect(exposure, "5d") ~ " for 5+ days",
                                    TRUE ~ " on the delivery date")) 

## Create labels for the exposure column
data <- data %>%
  mutate(exp_label = case_when(
    ## For absolute temperature
    tmp_threshold_type == "absolute" & duration == " on the delivery date" ~ paste0("Daily temperature >= ", tmp_threshold, "\U00B0", "C on the delivery date"),
    tmp_threshold_type == "absolute" & duration != " on the delivery date" ~ paste0("Daily temperature >= ", tmp_threshold, "\U00B0", "C", duration),
    ## For relative temperatures
    tmp_threshold_type != "absolute" & duration == " on the delivery date" ~ paste0("Daily temperature >= ", tmp_threshold, "th %ile on the delivery date"),
    tmp_threshold_type != "absolute" & duration != " on the delivery date" ~ paste0("Daily temperature >= ", tmp_threshold, "th %ile", duration)
))
unique(data$exp_label)

## Chage labels for lt_mean
data$contrast[data$contrast == "Lowest_Tertile"] <- "Cooler"
data$contrast[data$contrast == "Medium_Tertile"] <- "Medium"
data$contrast[data$contrast == "High_Tertile"] <- "Warmer"

## Change contrast for access issue
data$contrast[data$contrast == "big-problem"] <- "Yes"
data$contrast[data$contrast == "not-a-big-problem"] <- "No"
data$contrast[data$contrast == "Poorest"] <- "1st"
data$contrast[data$contrast == "Poorer"] <- "2nd"
data$contrast[data$contrast == "Middle"] <- "3rd"
data$contrast[data$contrast == "Richer"] <- "4th"
data$contrast[data$contrast == "Richest"] <- "5th"

## Check values
unique(data$contrast)
head(data$exp_label)

## Order the levels of the Contrast variable
ord_contrast <- c("ST", "SC", "OBC", "Other",
          "Hindu", "Not-Hindu",
          "Rural", "Urban",
          "1st", "2nd", "3rd", "4th", "5th",
          "Yes", "No",
          "Cooler", "Medium", "Warmer")

data$contrast <- factor(data$contrast, levels=rev(ord_contrast))
data <- data %>% mutate(contrast = fct_reorder(contrast, desc(contrast))) 
# unique(data$contrast)

# Create datasets for plots ----
## Function to create datasets for plots ----
func_subset_data <- function(data, threshold_type, threshold) {
    ## Subset data based on threshold and threshold type
    df_plot <- data |> filter(tmp_threshold_type == threshold_type & tmp_threshold == threshold)
    
    ### Order the levels of the exposure variables
    levels_exp <- levels(factor(df_plot$exp_label))
    ord_exposure <- c(levels_exp[4], levels_exp[1], levels_exp[2], levels_exp[3])

    df_plot$exp_label <- factor(df_plot$exp_label, levels=rev(ord_exposure))
    df_plot <- df_plot %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))
}

## Absolute -----
df_plot_abs_30 <- func_subset_data(data, "absolute", "30")
df_plot_abs_31 <- func_subset_data(data, "absolute", "31")
df_plot_abs_32 <- func_subset_data(data, "absolute", "32")

## Relative - based on DOY -----
df_plot_rel_doy_90 <- func_subset_data(data, "doy", "90")
df_plot_rel_doy_95 <- func_subset_data(data, "doy", "95")
df_plot_rel_doy_97 <- func_subset_data(data, "doy", "97")

# Create and save the plots ---- 
## Call function to plot with here
source(here("1-scripts", "6.3-function-to-plot-effect-modifiers.R"))

## Create directory -----
path_fig_out <- here("3-outputs", "figures")
if (!dir.exists(path_fig_out)) {
  # Create the directory if it does not exist
  dir.create(path_fig_out, showWarnings = TRUE, recursive = TRUE)
}

## List all objects in the enviroment that start with "df_plot"
list_df_plots <- ls(pattern = "df_plot")
list_df_plots <-  list_df_plots[!list_df_plots %in% "df_plot_abs_32"]

## Loop to create and save plots
for (df in list_df_plots) {
  plot_comb <- plot_wrapper(eval(parse(text = df)))
  ggsave(here(path_fig_out, paste0("plot_", df, ".svg")), 
         plot_comb, width = 50, height = 40, units = "cm", 
         dpi = 600, limitsize = FALSE)
}
## Save plot for df_plot_abs_32 separately
df <- df_plot_abs_32
plot_caste <- plot_effect_modifier(df, "effect_modifier", "Caste")
plot_religion <- plot_effect_modifier(df, "effect_modifier", "Religion")
plot_residence <- plot_effect_modifier(df, "effect_modifier", "Residence")
plot_wealth <- plot_effect_modifier(df, "effect_modifier", "Wealth Quintile")
plot_access <- plot_effect_modifier(df, "effect_modifier", "Distance is a big problem to access healthcare")
plot_lt_temp_mean <- plot_effect_modifier(df, "effect_modifier", "Long-term mean temperature tertiles")

## Combine the plots using patchwork
plot_out <- (((plot_caste) | (plot_religion)) / 
              (plot_residence | plot_access) / (plot_wealth)) +
              plot_annotation(tag_levels = c('a'))

## Save the plot
ggsave(here(path_fig_out, "plot_df_plot_abs_32a.svg"), 
       plot_out, width = 40, height = 25, units = "cm", 
       dpi = 600) 

ggsave(here(path_fig_out, "plot_df_plot_abs_32b.svg"), 
       plot_lt_temp_mean, width = 30, height = 25, units = "cm", 
       dpi = 600)

