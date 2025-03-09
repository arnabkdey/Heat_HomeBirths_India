# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates a descriptive table for the paper summarizing the distribution of key socio-economic variables by place of birth
# @date: March 2025

# Libraries ----
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here, survey, gt)

# set paths ----
source(here("paths_mac.R"))

# Load datasets ----
df_paper_final <- readRDS(here(path_project, "data", "processed_data", "1.3.1_final_data_for_paper.rds"))

# Load function for weighted tables ----
function_path <- here("src", "03_outputs", "utils", "function_wtd_comparegroups.R")
source(function_path)

# process data ----
## relevel hh_caste_club 
df_paper_final$hh_caste_club <- as.factor(df_paper_final$hh_caste_club)
df_paper_final$hh_caste_club <- fct_relevel(df_paper_final$hh_caste_club, "SC", "ST", "OBC", "Other")

# create weighted table ----
## Create list of variables
varlist_ses <- c("mat_age_grp_at_birth", "mat_edu_level", 
                "mat_parity_fac", "mat_birth_order", 
                "mat_media_exp_any",
                "hh_caste_club",
                "hh_religion_club", "hh_access_issue_distance", 
                "hh_wealth_quintile_ru_og", "rural")

## Create survey object
svy_object <- svydesign(ids = ~1,
                data = df_paper_final,
              weights = df_paper_final$wt_final)

## generate table
table1 <- compareGroups_wtd(data = df_paper_final,
                            dep_var = "dv_home_del_fac",
                            varlist = c(varlist_ses),
                            survey_object = svy_object,
                            output_type = "full", 
                            n_digits = 1)

# format table ----
## retain relevant columns
table1_output <- table1 |>
  select(levels = Levels_or_Mean_SD,
         overall = Overall_Prop_or_Mean,
         fac_birth = '0',
         home_birth = '1')

## create a vector of variable_names that captures 
var_names <- c()
for (i in varlist_ses) {
  n_levels <- length(unique(df_paper_final[[i]]))
  var_names <- c(var_names, rep(i, n_levels))
}

## append variable names to the table
table1_output$variable <- var_names

## clean names
table1_output <- table1_output |>
  mutate(var_label = case_when(
    variable == "mat_age_grp_at_birth" ~ "Mother's age group at birth (in years)",
    variable == "mat_edu_level" ~ "Mother's education level",
    variable == "mat_parity_fac" ~ "Number of children",
    variable == "mat_birth_order" ~ "Birth order of current child",
    variable == "mat_media_exp_any" ~ "Mother's media exposure",
    variable == "hh_caste_club" ~ "Caste",
    variable == "hh_religion_club" ~ "Religion",
    variable == "hh_access_issue_distance" ~ "Healthcare access",
    variable == "hh_wealth_quintile_ru_og" ~ "Wealth quintile",
    variable == "rural" ~ "Rural/Urban residence"
  ))

## remove % signs from the columns
table1_output$overall <- gsub(" %", "", table1_output$overall)
table1_output$fac_birth <- gsub(" %", "", table1_output$fac_birth)
table1_output$home_birth <- gsub(" %", "", table1_output$home_birth)

## create a new variable for variable levels
table1_output <- table1_output |>
  mutate(levels_clean = case_when(
    levels == "no education" ~ "No education",
    levels == "primary" ~ "Primary",
    levels == "secondary" ~ "Secondary",
    levels == "higher" ~ "Higher",
    levels == "hindu" ~ "Hindu",
    levels == "muslim" ~ "Muslim",
    levels == "christian" ~ "Christian",
    levels == "other" ~ "Other",
    levels == "no" ~ "No",
    levels == "yes" ~ "Yes",
    levels == "big-problem" ~ "Distance is a big problem",
    levels == "not-a-big-prob" ~ "Distance is not a big problem",
    levels == "poorest" ~ "Poorest",
    levels == "poorer" ~ "Poorer",
    levels == "middle" ~ "Middle",
    levels == "richer" ~ "Richer",
    levels == "richest" ~ "Richest",
    levels == "rural" ~ "Rural",
    levels == "urban" ~ "Urban",
    levels == "One" ~ "First child",
    levels == "Two" ~ "Second child",
    levels == "Three" ~ "Third child",
    levels == "Four or more" ~ "Fourth child or higher",
    levels == "OBC" ~ "Other Backward Class",
    levels == "SC" ~ "Scheduled Caste",
    levels == "ST" ~ "Scheduled Tribe",
    levels == "Other" ~ "General",
    TRUE ~ levels
  ))


# create a gt table ----
gt_table <- table1_output |>
  select(var_label, levels_clean, overall, fac_birth, home_birth) |>
  gt(groupname_col = "var_label") |>
  cols_label(
    # var_label = "Variable label",
    levels_clean = "Variable levels",
    overall = html("Overall sample<br>N (weighted %)"),
    fac_birth = html("Health facility-based<br>births<br>n (weighted %)"),
    home_birth = html("Home-based<br>births<br>n (weighted %)")
  ) |>
  fmt_number(
    columns = c(overall, fac_birth, home_birth),
    decimals = 1
  ) |>
  # add footer
  tab_footnote(
    "Note: The number of cases reported in each cell is unweighted, and the percentages were calculated using survey weights."
  ) |>
  tab_style(
  style = list(
    cell_fill(color = "#f7f7f7")
  ),
  locations = cells_row_groups()
  ) |>
  tab_style(
    style = list(
      cell_text(weight = "bold", align = "center")
    ),
    locations = cells_column_labels(everything())
  ) 

# Save the table ----
gtsave(gt_table, here(path_project, "outputs", "tables",
  "table1_descriptive_stats.png"))
