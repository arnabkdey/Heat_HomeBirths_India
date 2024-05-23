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
wealth <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-wealth.xlsx"), "Wealth")
lt_tmax_mean <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-lt_tmax_mean.xlsx"), "Long-term mean temperature tertiles")
lt_tmax_median <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-lt_tmax_median.xlsx"), "Long-term median temperature tertiles")

# Process data for plots ----
## Combine all the data ----
data <- rbind(caste, religion, residence, wealth, lt_tmax_mean, lt_tmax_median)
## Filter and transform data for specified contrasts, calculate OR and CI, and retain relevant columns.
data <- data %>% 
  filter(contrast %in% c("OBC", "SC", "ST", "Other", 
                         "Hindu", "Not-Hindu",
                         "Rural", "Urban",
                         "Poorest", "Poorer", "Middle", "Richer", "Richest",
                         "Lowest_Tertile", "Medium_Tertile", "High_Tertile",
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
unique(data$contrast)
head(data$exp_label)

## Order the levels of the Contrast variable
ord_contrast <- c("ST", "SC", "OBC", "Other",
          "Hindu", "Not-Hindu",
          "Rural", "Urban",
          "Poorest", "Poorer", "Middle", "Richer", "Richest", 
          "Cooler", "Medium", "Warmer")


data$contrast <- factor(data$contrast, levels=rev(ord_contrast))
data <- data %>% mutate(contrast = fct_reorder(contrast, desc(contrast))) 
# unique(data$contrast)

# Create datasets for plots ----
## Absolute -----
df_plot_abs_30 <- data |> filter(tmp_threshold_type == "absolute") 

### Order the levels of the exposure variables
levels_abs <- levels(factor(df_plot_abs_30$exp_label))
ord_exposure_abs <- c(levels_abs[4], levels_abs[1], levels_abs[2], levels_abs[3])

df_plot_abs_30$exp_label <- factor(df_plot_abs_30$exp_label, levels=rev(ord_exposure_abs))
df_plot_abs_30 <- df_plot_abs_30 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

### Quick check
# unique(df_plot_abs_30$exposure)
# nrow(data)
# nrow(df_plot_abs_30)

## Relative - based on DOY ----- 
df_plot_rel_doy_90 <- data %>% filter(tmp_threshold_type == "doy")
nrow(df_plot_rel_doy_90)

### Order the levels of the exposure variables
levels_rel_doy <- levels(factor(df_plot_rel_doy_90$exp_label))
ord_exposure_rel_doy <- c(levels_rel_doy[4], levels_rel_doy[1], levels_rel_doy[2], levels_rel_doy[3])
df_plot_rel_doy_90$exp_label <- factor(df_plot_rel_doy_90$exp_label, levels=rev(ord_exposure_rel_doy))
df_plot_rel_doy_90 <- df_plot_rel_doy_90 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

## Relative - based on Harmonic -----
df_plot_rel_harmo_90 <- data %>% filter(tmp_threshold_type == "harmo")
nrow(df_plot_rel_harmo_90)

### Order the levels of the exposure variables
levels_rel_harmo <- levels(factor(df_plot_rel_harmo_90$exp_label))
ord_exposure_rel_harmo <- c(levels_rel_harmo[4], levels_rel_harmo[1], levels_rel_harmo[2], levels_rel_harmo[3])
df_plot_rel_harmo_90$exp_label <- factor(df_plot_rel_harmo_90$exp_label, levels=rev(ord_exposure_rel_harmo))
df_plot_rel_harmo_90 <- df_plot_rel_harmo_90 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

### Quick check
# unique(df_plot_rel$exposure)
# nrow(data)
# nrow(df_plot_rel)

# Create and save the plots ---- 
## Call function to plot with here
source(here("1-scripts", "6.3-function-to-plot-effect-modifiers.R"))

## Create directory -----
path_fig_out <- here("3-outputs", "figures")
if (!dir.exists(path_fig_out)) {
  # Create the directory if it does not exist
  dir.create(path_fig_out, showWarnings = TRUE, recursive = TRUE)
}

## Write a loop to create and save plots
list_dfs <- c("df_plot_abs_30", "df_plot_rel_doy_90", "df_plot_rel_harmo_90")
for (df in list_dfs) {  
  plot_comb <- plot_wrapper(eval(parse(text = df)))
  ggsave(here(path_fig_out, paste0("plot_", df, ".svg")), 
         plot_comb, width = 35, height = 40, units = "cm", 
         dpi = 1000)
}


