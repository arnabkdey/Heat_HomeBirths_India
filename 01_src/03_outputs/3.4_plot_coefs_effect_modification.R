# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Anna Dimitrova, Arnab K. Dey
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script plots the odds ratios and RERI for the effect modification analysis
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, readxl, ggpubr)

# set paths ----
source(here("paths.R"))

# load data ----
## odds ratios ----
### Define the file path
file_path_or <- "subgroup_odds_ratios.xlsx"
file_path_or <- here(path_project, "outputs", 
  "models", "effect_modification", "results_main",
  "subgroup_odds_ratios.xlsx")

### Get the names of all sheets in the Excel file
sheet_names_or <- excel_sheets(file_path_or)

### Read all sheets, add sheet name as a column, and combine into one dataframe
df_or <- sheet_names_or %>%
  lapply(function(sheet) read_excel(file_path_or, sheet = sheet) %>%
           mutate(Modifier = sheet)) %>%
  bind_rows() %>% 
  drop_na()

## RERI ----
### Define the file path
file_path_reri <- here(path_project, "outputs", 
  "models", "effect_modification", "results_main",
  "subgroup_reri.xlsx")

### Get the names of all sheets in the Excel file
sheet_names_reri <- excel_sheets(file_path_reri)

### Read all sheets, add sheet name as a column, and combine into one dataframe
df_reri <- sheet_names_reri %>%
  lapply(function(sheet) read_excel(file_path_reri, sheet = sheet) %>%
           mutate(Modifier = sheet)) %>%
  bind_rows() %>% 
  drop_na()

# Clean the data ----
## odds ratio data 
### Modifiers
df_or$Modifier[df_or$Modifier=="rural"] = "A. Residence"
df_or$Modifier[df_or$Modifier=="hh_access_issue_distance"] = "B. Distance is a big problem"
df_or$Modifier[df_or$Modifier=="lt_mean_wbgt_tert_psu"] = "I. Long-term temperature"
df_or$Modifier[df_or$Modifier=="state_janani_bi"] = "G. JSY high-focus state"
df_or$Modifier[df_or$Modifier=="state_home_birth_bi"] = "H. State home birth rate"
df_or$Modifier[df_or$Modifier=="mat_edu_level_bi"] = "C. Woman's education"
df_or$Modifier[df_or$Modifier=="hh_wealth_poorest2"] = "D. Household wealth"
df_or$Modifier[df_or$Modifier=="hh_religion_bi"] = "E. Religion"
df_or$Modifier[df_or$Modifier=="hh_caste_bi"] = "F. Marginalized caste"

### modifier levels
df_or$ModifierLevel[df_or$ModifierLevel=="urban"] = "Urban"
df_or$ModifierLevel[df_or$ModifierLevel=="rural"] = "Rural"
df_or$ModifierLevel[df_or$ModifierLevel=="big-problem"] = "Yes"
df_or$ModifierLevel[df_or$ModifierLevel=="not-a-big-prob"] = "No"
df_or$ModifierLevel[df_or$ModifierLevel=="1"] = "Cooler"
df_or$ModifierLevel[df_or$ModifierLevel=="2"] = "Medium"
df_or$ModifierLevel[df_or$ModifierLevel=="3"] = "Warmer"
df_or$ModifierLevel[df_or$ModifierLevel=="JSY"] = "Yes"
df_or$ModifierLevel[df_or$ModifierLevel=="Non-JSY"] = "No"
df_or$ModifierLevel[df_or$ModifierLevel=="LowHB"] = "Low"
df_or$ModifierLevel[df_or$ModifierLevel=="Medium_or_High_HB"] = "High"
df_or$ModifierLevel[df_or$ModifierLevel=="primary or higher"] = "Some"
df_or$ModifierLevel[df_or$ModifierLevel=="no education"] = "None"
df_or$ModifierLevel[df_or$ModifierLevel=="poorer2"] = "Poorer"
df_or$ModifierLevel[df_or$ModifierLevel=="richer3"] = "Richer"
df_or$ModifierLevel[df_or$ModifierLevel=="not-hindu"] = "Non-hindu"
df_or$ModifierLevel[df_or$ModifierLevel=="hindu"] = "Hindu"
df_or$ModifierLevel[df_or$ModifierLevel=="marg"] = "Yes"
df_or$ModifierLevel[df_or$ModifierLevel=="general"] = "No"

### filter exposures
df_or_db <- df_or %>% 
  filter(Exposure=="hw_db_85_4d")

df_or_wb <- df_or %>% 
  filter(Exposure=="hw_wb_85_4d")

## RERI data
### modifiers
df_reri$Modifier[df_reri$Modifier=="rural"] = "A. Residence"
df_reri$Modifier[df_reri$Modifier=="hh_access_issue_distance"] = "B. Distance is a big problem"
df_reri$Modifier[df_reri$Modifier=="lt_mean_wbgt_tert_psu"] = "I. Long-term temperature"
df_reri$Modifier[df_reri$Modifier=="state_janani_bi"] = "G. JSY high-focus state"
df_reri$Modifier[df_reri$Modifier=="state_home_birth_bi"] = "H. State home birth rate"
df_reri$Modifier[df_reri$Modifier=="mat_edu_level_bi"] = "C. Woman's education"
df_reri$Modifier[df_reri$Modifier=="hh_wealth_poorest2"] = "D. Household wealth"
df_reri$Modifier[df_reri$Modifier=="hh_religion_bi"] = "E. Religion"
df_reri$Modifier[df_reri$Modifier=="hh_caste_bi"] = "F. Marginalized caste"

### modifier levels
df_reri$ModifierLevel[df_reri$ModifierLevel=="urban"] = "Urban"
df_reri$ModifierLevel[df_reri$ModifierLevel=="rural"] = "Rural"
df_reri$ModifierLevel[df_reri$ModifierLevel=="big-problem"] = "Yes"
df_reri$ModifierLevel[df_reri$ModifierLevel=="not-a-big-prob"] = "No"
df_reri$ModifierLevel[df_reri$ModifierLevel=="1"] = "Cooler"
df_reri$ModifierLevel[df_reri$ModifierLevel=="2"] = "Medium"
df_reri$ModifierLevel[df_reri$ModifierLevel=="3"] = "Warmer"
df_reri$ModifierLevel[df_reri$ModifierLevel=="JSY"] = "Yes"
df_reri$ModifierLevel[df_reri$ModifierLevel=="Non-JSY"] = "No"
df_reri$ModifierLevel[df_reri$ModifierLevel=="LowHB"] = "Low"
df_reri$ModifierLevel[df_reri$ModifierLevel=="Medium_or_High_HB"] = "High"
df_reri$ModifierLevel[df_reri$ModifierLevel=="primary or higher"] = "Some"
df_reri$ModifierLevel[df_reri$ModifierLevel=="no education"] = "None"
df_reri$ModifierLevel[df_reri$ModifierLevel=="poorer2"] = "Poorer"
df_reri$ModifierLevel[df_reri$ModifierLevel=="richer3"] = "Richer"
df_reri$ModifierLevel[df_reri$ModifierLevel=="not-hindu"] = "Non-hindu"
df_reri$ModifierLevel[df_reri$ModifierLevel=="hindu"] = "Hindu"
df_reri$ModifierLevel[df_reri$ModifierLevel=="marg"] = "Yes"
df_reri$ModifierLevel[df_reri$ModifierLevel=="general"] = "No"

### filter exposures
df_reri_db <- df_reri %>% 
  filter(Exposure=="hw_db_85_4d")


df_reri_wb <- df_reri %>% 
  filter(Exposure=="hw_wb_85_4d")


# Plotting ----

## Define the modifier categories
modifiers <- c(
  "A. Residence", 
  "B. Distance is a big problem", 
  "C. Woman's education", 
  "D. Household wealth", 
  "E. Religion", 
  "F. Marginalized caste", 
  "G. JSY high-focus state", 
  "H. State home birth rate", 
  "I. Long-term temperature"
)

## Dry bulb plot ----

### Create an empty list to store final plots
final_plots_db <- list()

### Loop through each modifier to create separate plots
for (modifier in modifiers) {
  
  # Filter datasets
  df_or_db_filtered <- df_or_db %>% filter(Modifier == modifier)
  df_reri_db_filtered <- df_reri_db %>% filter(Modifier == modifier)
  
  # Generate OR plot with the title
  ggplot_or <- ggplot(df_or_db_filtered, aes(y = ModifierLevel)) +
    geom_linerange(aes(xmin = OR_LowerCI, xmax = OR_UpperCI, y = ModifierLevel), 
                   color = "#A11217", size = 1, position = position_dodge(width = 0.7)) +
    geom_point(aes(x = OR), size = 3.5, color = "#A11217", position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 1, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(title = modifier, y = "", x = "OR with 95% CI") +  # Title added here
    xlim(0.7, 1.4) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0),  # Centered title
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  # Generate RERI plot
  ggplot_reri <- ggplot(df_reri_db_filtered, aes(y = ModifierLevel)) +
    geom_segment(aes(x = 0, xend = RERI, y = ModifierLevel, yend = ModifierLevel), 
                 size = 5, color = "#6FAEF5", position = position_dodge(width = 0.7)) +
    geom_linerange(aes(xmin = RERI_LowerCI, xmax = RERI_UpperCI, y = ModifierLevel), 
                   color = "#102A6B", size = 1.2, position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 0, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(y = "", x = "RERI with 95% CI") +
    xlim(-0.75, 1) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  # Combine OR and RERI plots
  final_plot <- plot_grid(
    ggplot_or,
    ggplot_reri,
    ncol = 1, 
    align = "v", 
    rel_heights = c(1, 0.7)  # Adjust heights for better balance
  )
  
  # Store in list
  final_plots_db[[modifier]] <- final_plot
}

### Combine all final plots into one large figure with a white background
all_final_plots_db <- plot_grid(plotlist = final_plots_db, ncol = 3, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save the combined final plot for dry bulb temperature
ggsave(here(path_project, "outputs", "figures", "Plot_EM_db.png"), 
  plot = all_final_plots_db, 
  width = 8, 
  height = 8, 
  dpi = 500,
  bg = "white")


## Wet bulb plot ----

### Create an empty list to store final plots
final_plots_wb <- list()

### Loop through each modifier to create separate plots
for (modifier in modifiers) {
  
  # Filter datasets
  df_or_wb_filtered <- df_or_wb %>% filter(Modifier == modifier)
  df_reri_wb_filtered <- df_reri_wb %>% filter(Modifier == modifier)
  
  # Generate OR plot with the title
  ggplot_or <- ggplot(df_or_wb_filtered, aes(y = ModifierLevel)) +
    geom_linerange(aes(xmin = OR_LowerCI, xmax = OR_UpperCI, y = ModifierLevel), 
                   color = "#A11217", size = 1, position = position_dodge(width = 0.7)) +
    geom_point(aes(x = OR), size = 3.5, color = "#A11217", position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 1, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(title = modifier, y = "", x = "OR with 95% CI") +  # Title added here
    xlim(0.7, 1.4) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0),  # Centered title
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  # Generate RERI plot
  ggplot_reri <- ggplot(df_reri_wb_filtered, aes(y = ModifierLevel)) +
    geom_segment(aes(x = 0, xend = RERI, y = ModifierLevel, yend = ModifierLevel), 
                 size = 5, color = "#6FAEF5", position = position_dodge(width = 0.7)) +
    geom_linerange(aes(xmin = RERI_LowerCI, xmax = RERI_UpperCI, y = ModifierLevel), 
                   color = "#102A6B", size = 1.2, position = position_dodge(width = 0.7)) +
    geom_vline(xintercept = 0, color = "black") +
    facet_wrap(~ Modifier, scales = "free_y", ncol = 3) +
    labs(y = "", x = "RERI with 95% CI") +
    xlim(-0.75, 1) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = "dotted", color = "gray", size = 0.4),
      axis.text.y = element_text(size = 8.5),
      axis.text.x = element_text(size = 8.5, face = "bold"),
      axis.title.x = element_text(size = 8.5),
      axis.title.y = element_text(size = 8.5),
      strip.text = element_blank(),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )
  
  # Combine OR and RERI plots
  final_plot <- plot_grid(
    ggplot_or,
    ggplot_reri,
    ncol = 1, 
    align = "v", 
    rel_heights = c(1, 0.7)  # Adjust heights for better balance
  )
  
  # Store in list
  final_plots_wb[[modifier]] <- final_plot
}

### Combine all final plots into one large figure with a white background
all_final_plots_wb <- plot_grid(plotlist = final_plots_wb, ncol = 3, align = "v") +
  theme(plot.background = element_rect(fill = "white", color = "white"))

### Save the combined final plot for wet bulb temperature
ggsave(here(path_project, "outputs", "figures", "Plot_EM_wbgt.png"),
  plot = all_final_plots_wb, 
  width = 8, 
  height = 8, 
  dpi = 500)