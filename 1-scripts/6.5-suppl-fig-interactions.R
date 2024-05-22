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
here_output_files <- here("3-outputs", "supplements", "models", "models-with-interaction")
caste <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-caste.xlsx"), "Caste")
religion <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-religion.xlsx"), "Religion")
residence <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-rural.xlsx"), "Residence")
wealth <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-wealth.xlsx"), "Wealth")
lt_tmax_mean <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-lt_tmax_mean.xlsx"), "Long-term mean temperature tertiles")
lt_tmax_median <- read_excel_sheets_and_combine(here(here_output_files, "multcomp-cis-lt_tmax_median.xlsx"), "Long-term median temperature tertiles")

# Process data for plots ----
## Combine all the data ----
data <- rbind(caste, religion, residence, wealth, lt_tmax_mean, lt_tmax_median)
colnames(data)
min(data$conf.low)
max(data$conf.high)
exp(-3.7164)
exp(3.8271)
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
## Absolute - 31 -----
df_plot_abs_31 <- data |> filter(tmp_threshold_type == "absolute" & tmp_threshold == "31") 

### Order the levels of the exposure variables
levels_abs <- levels(factor(df_plot_abs_31$exp_label))
ord_exposure_abs <- c(levels_abs[4], levels_abs[1], levels_abs[2], levels_abs[3])

df_plot_abs_31$exp_label <- factor(df_plot_abs_31$exp_label, levels=rev(ord_exposure_abs))
df_plot_abs_31 <- df_plot_abs_31 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

## Absolute - 32 -----
df_plot_abs_32 <- data |> filter(tmp_threshold_type == "absolute" & tmp_threshold == "32") 

### Order the levels of the exposure variables
levels_abs <- levels(factor(df_plot_abs_32$exp_label))
ord_exposure_abs <- c(levels_abs[4], levels_abs[1], levels_abs[2], levels_abs[3])

df_plot_abs_32$exp_label <- factor(df_plot_abs_32$exp_label, levels=rev(ord_exposure_abs))
df_plot_abs_32 <- df_plot_abs_32 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

## Relative - based on DOY - 95th ----- 
df_plot_rel_doy_95 <- data %>% filter(tmp_threshold_type == "doy" & tmp_threshold == "95")
nrow(df_plot_rel_doy_95)

### Order the levels of the exposure variables
levels_rel_doy <- levels(factor(df_plot_rel_doy_95$exp_label))
ord_exposure_rel_doy <- c(levels_rel_doy[4], levels_rel_doy[1], levels_rel_doy[2], levels_rel_doy[3])
df_plot_rel_doy_95$exp_label <- factor(df_plot_rel_doy_95$exp_label, levels=rev(ord_exposure_rel_doy))
df_plot_rel_doy_95 <- df_plot_rel_doy_95 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

## Relative - based on DOY - 97th ----- 
df_plot_rel_doy_97 <- data %>% filter(tmp_threshold_type == "doy" & tmp_threshold == "97")
nrow(df_plot_rel_doy_97)

### Order the levels of the exposure variables
levels_rel_doy <- levels(factor(df_plot_rel_doy_97$exp_label))
ord_exposure_rel_doy <- c(levels_rel_doy[4], levels_rel_doy[1], levels_rel_doy[2], levels_rel_doy[3])
df_plot_rel_doy_97$exp_label <- factor(df_plot_rel_doy_97$exp_label, levels=rev(ord_exposure_rel_doy))
df_plot_rel_doy_97 <- df_plot_rel_doy_97 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))


## Relative - based on Harmonic - 95th -----
df_plot_rel_harmo_95 <- data %>% filter(tmp_threshold_type == "harmo" & tmp_threshold == "95")
nrow(df_plot_rel_harmo_95)

### Order the levels of the exposure variables
levels_rel_harmo <- levels(factor(df_plot_rel_harmo_95$exp_label))
ord_exposure_rel_harmo <- c(levels_rel_harmo[4], levels_rel_harmo[1], levels_rel_harmo[2], levels_rel_harmo[3])
df_plot_rel_harmo_95$exp_label <- factor(df_plot_rel_harmo_95$exp_label, levels=rev(ord_exposure_rel_harmo))
df_plot_rel_harmo_95 <- df_plot_rel_harmo_95 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

## Relative - based on Harmonic - 97th -----
df_plot_rel_harmo_97 <- data %>% filter(tmp_threshold_type == "harmo" & tmp_threshold == "97")
nrow(df_plot_rel_harmo_97)

### Order the levels of the exposure variables
levels_rel_harmo <- levels(factor(df_plot_rel_harmo_97$exp_label))
ord_exposure_rel_harmo <- c(levels_rel_harmo[4], levels_rel_harmo[1], levels_rel_harmo[2], levels_rel_harmo[3])
df_plot_rel_harmo_97$exp_label <- factor(df_plot_rel_harmo_97$exp_label, levels=rev(ord_exposure_rel_harmo))
df_plot_rel_harmo_97 <- df_plot_rel_harmo_97 %>% mutate(exp_label = fct_reorder(exp_label, desc(exp_label)))

# Create and save the plots ---- 
## Call function to plot with here
source(here("1-scripts", "5.3-function-to-plot-effect-modifiers.R"))

## Create directory -----
path_fig_out <- here("3-outputs", "supplements", "figures")
if (!dir.exists(path_fig_out)) {
  # Create the directory if it does not exist
  dir.create(path_fig_out, showWarnings = TRUE, recursive = TRUE)
}

## Write a loop to create and save plots
list_dfs <- c("df_plot_abs_31", "df_plot_abs_32",
                "df_plot_rel_doy_95", "df_plot_rel_harmo_95",
                "df_plot_rel_doy_97", "df_plot_rel_harmo_97")

for (df in list_dfs) {
  plot_comb <- plot_wrapper(eval(parse(text = df)))
  ggsave(here(path_fig_out, paste0("plot_", df, ".svg")), 
         plot_comb, width = 35, height = 40, units = "cm", dpi = 1000)
}

## toolbox 

View(df_plot_abs_31)
data_cur <- df_plot_abs_32
effect_modifier_col <- "effect_modifier"
effect_modifier_value <- "Long-term mean temperature tertiles"

effect_modifier_col_sym <- sym(effect_modifier_col)
filtered_data <- data_cur %>% filter(!!effect_modifier_col_sym == effect_modifier_value)
View(filtered_data)

filtered_data <- filtered_data %>%
    mutate(OR = exp(estimate),
         CILow  = exp(conf.low),
         CIHigh = exp(conf.high))
View(filtered_data)
exp(-0.2581)
  # Calculate the min and max for CILow and CIHigh
  y_min <- min(filtered_data$CILow, na.rm = TRUE)
  y_max <- max(filtered_data$CIHigh, na.rm = TRUE)

ggplot(filtered_data, aes(y=contrast, x=OR)) +
  facet_wrap(~exp_label, labeller = label_wrap_gen(width=22), nrow = 1) +
  geom_vline(xintercept=1, colour="darkgray") +  
  geom_pointrange(aes(xmin=CILow, xmax=CIHigh), position = position_dodge(width = 0.65), size = 0.5) +
  labs(y=effect_modifier_value, x="aOR") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, linetype="dashed") +
  theme_classic() +
  theme(
    panel.grid.major = element_line(linewidth=0.25), 
    panel.grid.minor.x = element_line(linewidth=0.15),
    strip.background = element_blank(),  
    strip.placement = "outside",  
    strip.text.y.left = element_text(angle = 0, family="Calibri"),
    text = element_text(size = 13, family="Calibri"),  
    axis.ticks = element_blank(), 
    axis.title.x = element_text(margin = margin(t = 15)), # Increase gap by adjusting the top margin
    panel.border = element_blank(),
    legend.position="none",
    panel.spacing=unit(0, "cm"), 
    plot.title = element_text(size=13, family="Calibri"),
  ) +
  # scale_x_continuous(limits = c(0.4, 1.6)) +
  scale_x_continuous(limits = c(y_min, y_max)) +
  coord_flip()
