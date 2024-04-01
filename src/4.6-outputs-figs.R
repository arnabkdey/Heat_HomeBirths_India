rm(list =ls())
options(scipen=999)
options(digits=5)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(sjPlot)
library(readxl)
library(here)

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
caste <- read_excel_sheets_and_combine(here("outputs", "models", "models-with-interaction", "multcomp-cis-caste.xlsx"), "Caste")
religion <- read_excel_sheets_and_combine(here("outputs", "models", "models-with-interaction", "multcomp-cis-religion.xlsx"), "Religion")
residence <- read_excel_sheets_and_combine(here("outputs", "models", "models-with-interaction", "multcomp-cis-rural.xlsx"), "Residence")
wealth <- read_excel_sheets_and_combine(here("outputs", "models", "models-with-interaction", "multcomp-cis-wealth.xlsx"), "Wealth")
lt_tmax <- read_excel_sheets_and_combine(here("outputs", "models", "models-with-interaction", "multcomp-cis-lt_tmax.xlsx"), "Long-term temperature tertile")

# Process data for plots ----
## Combine all the data ----
data <- rbind(caste, religion, residence, wealth, lt_tmax)
## Filter and transform data for specified contrasts, calculate OR and CI, and retain relevant columns.
data <- data %>% 
  filter(contrast %in% c("OBC", "SC", "ST", "Other", 
                         "Hindu", "Not-Hindu",
                         "Rural", "Urban",
                         "Poorest", "Poorer", "Middle", "Richer", "Richest",
                         "Lowest_Tertile", "Medium_Tertile", "High_Tertile")) %>% 
  mutate(OR = estimate,
         CILow  = OR - 1.96*std.error,
         CIHigh = OR + 1.96*std.error) %>% 
  mutate(OR = exp(OR),
         CILow = exp(CILow),
         CIHigh = exp(CIHigh)) %>% 
  rename(exposure = tabname) %>% 
  dplyr::select(effect_modifier, contrast, OR, CILow, CIHigh, exposure) 

## Determine if CI contains 1 (non-sign results) and change insignificant results to NA
data <- data %>% 
  mutate(OR_sign = OR) %>% 
  dplyr::mutate(help = 1 >= CILow & 1 <= CIHigh) %>%
  dplyr::mutate(OR_sign=replace(OR_sign, help=="TRUE", NA))     
  
## Split exposure column to extract the tmp_threshold and duration columns
# unique(data$exposure)
data <- data %>% 
  dplyr::mutate(tmp_threshold = str_split(exposure, "_", simplify=T)[ , 3]) %>% 
  dplyr::mutate(duration = str_split(exposure, "_", simplify=T)[ , 5]) 

unique(data$tmp_threshold)
unique(data$duration)

## Clean suffixes for duration
data$duration[data$duration == "hh" | data$duration == "rural" | data$duration == "lt"] <- ""
data$duration[data$duration == "2d"] <- " for 2+ days"
data$duration[data$duration == "3d"] <- " for 3+ days"
data$duration[data$duration == "5d"] <- " for 5+ days"
unique(data$duration)

## Clean tmp_threshold
data$tmp_threshold[data$tmp_threshold == "30"] <- paste0("30", "\U00B0", "C")
data$tmp_threshold[data$tmp_threshold == "95"] <- "95th %ile"
unique(data$tmp_threshold)

## Create exposure column
data <- data %>% 
  mutate(exposure = paste0("Daily temperature ",  "\U2265 ", tmp_threshold, duration, sep=""))
# unique(data$exposure)

## Chage labels for lt_mean
data$contrast[data$contrast == "Lowest_Tertile"] <- "Cooler"
data$contrast[data$contrast == "Medium_Tertile"] <- "Medium"
data$contrast[data$contrast == "High_Tertile"] <- "Warmer"
unique(data$contrast)

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
df_plot_abs <- data %>%
  filter(grepl("°C", exposure)) |> 
  mutate(exposure = as.character(exposure)) %>% 
  mutate(exposure = ifelse(!str_detect(exposure, "for"), "Daily temperature ≥ 30°C on the delivery date", exposure))

### Order the levels of the exposure variables
ord_exposure_abs <- c("Daily temperature ≥ 30°C on the delivery date", 
                  "Daily temperature ≥ 30°C for 2+ days", 
                  "Daily temperature ≥ 30°C for 3+ days", 
                  "Daily temperature ≥ 30°C for 5+ days")

df_plot_abs$exposure <- factor(df_plot_abs$exposure, levels=rev(ord_exposure_abs))
df_plot_abs <- df_plot_abs %>% mutate(exposure = fct_reorder(exposure, desc(exposure)))

### Quick check
# unique(df_plot_abs$exposure)
# nrow(data)
# nrow(df_plot_abs)

## Relative ----- 
df_plot_rel <- data %>%
  filter(grepl("ile", exposure)) |> 
  mutate(exposure = as.character(exposure)) %>% 
  mutate(exposure = ifelse(!str_detect(exposure, "for"), "Daily temperature \U2265 95th %ile on the delivery date", exposure))

### Order the levels of the exposure variables
ord_exposure_rel <- c("Daily temperature \U2265 95th %ile on the delivery date", 
                  "Daily temperature \U2265 95th %ile for 2+ days", 
                  "Daily temperature \U2265 95th %ile for 3+ days", 
                  "Daily temperature \U2265 95th %ile for 5+ days")

df_plot_rel$exposure <- factor(df_plot_rel$exposure, levels=rev(ord_exposure_rel))
df_plot_rel <- df_plot_rel %>% mutate(exposure = fct_reorder(exposure, desc(exposure)))                  

### Quick check
# unique(data$exposure)
# nrow(data)
# nrow(df_plot_rel)

# Create the plots ---- 
## Call function to plot with here
source(here("src", "5.5-function-to-plot-effect-modifiers.R"))

## Create the plots for the effect modifiers ----
### Plots with absolute temperature -----
plot_caste_abs <- plot_effect_modifier(df_plot_abs, "effect_modifier", "Caste")
plot_religion_abs <- plot_effect_modifier(df_plot_abs, "effect_modifier", "Religion")
plot_residence_abs <- plot_effect_modifier(df_plot_abs, "effect_modifier", "Residence")
plot_wealth_abs <- plot_effect_modifier(df_plot_abs, "effect_modifier", "Wealth")
plot_lt_temp_abs <- plot_effect_modifier(df_plot_abs, "effect_modifier", "Long-term temperature tertile")

plot_effMod_absTmp <- ggarrange(plot_caste_abs, plot_religion_abs, 
                                plot_residence_abs, plot_wealth_abs, 
                                plot_lt_temp_abs, 
                              align = "h", ncol = 1, nrow=5, 
                              labels = c("a", "b", "c", "d", "e")) 

### Plots with relative temperatures ----- 
plot_caste_rel <- plot_effect_modifier(df_plot_rel, "effect_modifier", "Caste")
plot_religion_rel <- plot_effect_modifier(df_plot_rel, "effect_modifier", "Religion")
plot_residence_rel <- plot_effect_modifier(df_plot_rel, "effect_modifier", "Residence")
plot_wealth_rel <- plot_effect_modifier(df_plot_rel, "effect_modifier", "Wealth")
plot_lt_temp_rel <- plot_effect_modifier(df_plot_rel, "effect_modifier", "Long-term temperature tertile")

plot_effMod_relTmp <- ggarrange(plot_caste_rel, plot_religion_rel, plot_residence_rel, plot_wealth_rel, plot_lt_temp_rel, align = "h", ncol = 1, nrow=5, labels = c("f", "g", "h", "i", "j")) 

### Combine absolute and relative plots into a single plot
plot_effectMod_all <- ggarrange(plot_effMod_absTmp, plot_effMod_relTmp, align = "h", ncol = 2, nrow=1)

# Save plot ----
## Create directory -----
if (!dir.exists("./outputs/figures/")) {
  # Create the directory if it does not exist
  dir.create("./outputs/figures/", showWarnings = TRUE, recursive = TRUE)
}

## Save the plot
ggsave(here("outputs", "figures", "plot_effect_mod_comb.svg"), 
  plot_effectMod_all, width = 55, height = 40, units = "cm", dpi = 1000)