# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates new variables for the analysis
# @date: March 2025

# load-packages ----
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here, mice)

# set paths ----
source(here("paths.R"))

# load the data ----
df_IR_long <- read_fst(here(
  path_project, "data", "processed_data", 
  "1.1.1_dhs_IR_long_raw_filtered.fst"))

# Create variables for full dataset ----
## Final weight ----
setDT(df_IR_long)
df_IR_long <- df_IR_long[, wt_final := wt_raw / 1000000]

## Caste ----
### Impute missing values for caste using the mice package
#### Convert to factor
df_IR_long$hh_caste <- as.factor(df_IR_long$hh_caste)

#### Check the number of missing values
sum(is.na(df_IR_long$hh_caste))
tabyl(df_IR_long$hh_caste)

#### Convert don't know to NA while retaining the factor levels
df_IR_long$hh_caste <- fct_recode(df_IR_long$hh_caste, NULL = "don't know")
tabyl(df_IR_long$hh_caste) 
dim(df_IR_long)

### Run the imputation
imp <- mice(df_IR_long, m=1, maxit=5, 
            method=c(hh_caste="cart"), 
            printFlag=TRUE)

### Extract one of the completed datasets
df_IR_long_imputed <- complete(imp, 1)
dim(df_IR_long_imputed)

### Check the distribution of the imputed values in the first dataset
tabyl(df_IR_long_imputed$hh_caste)

### Compare original vs imputed values
df_IR_long |> 
  filter(!is.na(hh_caste)) |>
  tabyl(hh_caste) 

### Create caste variables
df_IR_long_imputed <- df_IR_long_imputed |>
  ## Caste classification
  mutate(
    hh_caste_club = case_when(
      hh_caste == "schedule caste" ~ "SC",
      hh_caste == "schedule tribe" ~ "ST",
      hh_caste == "obc" ~ "OBC",
      TRUE ~ "Other"
    )
  ) |>
  ## Two level variables for caste
  mutate(
    hh_caste_bi = if_else(hh_caste == "none of them", "general", "marg")
  ) |>
  # Three level variable for religion
  mutate(
    hh_religion_tri = case_when(
      hh_religion == "hindu" ~ "hindu",
      hh_religion == "muslim" ~ "muslim",
      TRUE ~ "other"
    )
  ) |>
  # Three level variable for caste
  mutate(
    hh_caste_tri = case_when(
      hh_caste == "obc" ~ "OBC",
      hh_caste == "none of then" ~ "Other",
      TRUE ~ "SC/ST"
    )
  )

## Institutional delivery ----
df_IR_long_imputed <- df_IR_long_imputed |> 
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del =
                      ifelse((m15 == "respondent's home" | 
                              m15 == "other home" | 
                              m15 == "parents' home"), 1, 0)) |>
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del_fac = as.factor(dv_home_del)) |>
    dplyr::mutate(dv_home_del_num = as.numeric(dv_home_del_fac) - 1)

## Religion ----
df_IR_long_imputed <- df_IR_long_imputed |>
  # Religion classification
  mutate(
    hh_religion_club = case_when(
      hh_religion == "hindu" ~ "hindu",
      hh_religion == "muslim" ~ "muslim",
      hh_religion == "christian" ~ "christian",
      TRUE ~ "other"
    )
  ) |>
  # Relevel Religion levels
  mutate(
    hh_religion_club = fct_relevel(hh_religion_club, 
    "hindu", "muslim", "christian", "other")) |>
  # Two level variables for religion
  mutate(
    hh_religion_bi = if_else(hh_religion == "hindu", "hindu", "not-hindu")) 

## Household wealth ----
df_IR_long_imputed <- df_IR_long_imputed |>
  mutate(hh_wealth_poorest2 = case_when(
    hh_wealth_quintile_ru_og %in% c("poorest", "poorer") ~ "poorer2",
    TRUE ~ "richer3"
  )) |>
  mutate(hh_wealth_poorest2 = factor(hh_wealth_poorest2, 
    levels = c("poorer2", "richer3"))) |>
  mutate(hh_wealth_poorest3 = case_when(
    hh_wealth_quintile_ru_og %in% c(
      "poorest", "poorer", "middle") ~ "poorer3",
    TRUE ~ "richer2"
  )) |>
  mutate(hh_wealth_poorest3 = factor(hh_wealth_poorest3, 
    levels = c("poorer3", "richer2"))) |>
  mutate(hh_wealth_poorest4 = case_when(
    hh_wealth_quintile_ru_og %in% c(
      "poorest", "poorer", "middle", "richer") ~ "poorer4",
    TRUE ~ "richest"
  )) |>
  mutate(hh_wealth_poorest4 = factor(hh_wealth_poorest4, 
    levels = c("poorer4", "richest")))

## Maternal education 
df_IR_long_imputed$mat_edu_level_bi <- ifelse(
  df_IR_long_imputed$mat_edu_level == "no education", 
    "no education", "primary or higher")

## Women's age related variables ----
### Current age at interview
df_IR_long_imputed <- df_IR_long_imputed |> 
                    mutate(mat_age_grp = case_when(
                      mat_age < 25  ~ "15-24",
                      mat_age >= 25 & mat_age < 35  ~ "25-34",
                      mat_age >= 35 & mat_age < 50  ~ "35-49"))

### Age at birth 
df_IR_long_imputed <- df_IR_long_imputed |> 
    # First difference between date of interview and date of birth (in days)
    mutate(diff_doi_dob = doi - dob) |> 
    # Calculate age at birth
    mutate(mat_age_at_birth = mat_age - as.numeric(
      diff_doi_dob, units = "days")/365.25) |> 
    mutate(mat_age_at_birth = round(mat_age_at_birth, 0))


### Women's age group at birth
df_IR_long_imputed <- df_IR_long_imputed |> 
  mutate(mat_age_grp_at_birth = case_when(
    mat_age_at_birth < 20  ~ "15-19",
    mat_age_at_birth >= 20 & mat_age_at_birth < 25  ~ "20-24",
    mat_age_at_birth >= 25 & mat_age_at_birth < 30  ~ "25-29",
    mat_age_at_birth >= 30 ~ "30-49"))

### binary variable for age at birth
df_IR_long_imputed <- df_IR_long_imputed |> 
  mutate(mat_age_at_birth_bi = case_when(
    mat_age_at_birth < 25  ~ "15-24",
    mat_age_at_birth >= 25  ~ "25-49"))                      

## Access to healthcare ----
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::mutate(hh_access_issue_distance = 
    ifelse(hh_access_dist == "big problem", "big-problem", "not-a-big-prob"))

## Birth order ----
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::mutate(mat_birth_order = case_when(
    bord == 1 ~ "One",
    bord == 2 ~ "Two",
    bord == 3 ~ "Three",
    bord >= 4 ~ "Four or more"))

### relevel factors 
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::mutate(mat_birth_order = factor(mat_birth_order, 
    levels = c("One", "Two", "Three", "Four or more")))

## Parity (number of children) ----
df_IR_long_imputed <- df_IR_long_imputed |>
  mutate(mat_parity_fac = case_when(
    mat_parity == 1 ~ "1 child",
    mat_parity == 2 ~ "2 children",
    mat_parity == 3 ~ "3 children",
    mat_parity >= 4 ~ "4+ children"
  ) |> as.factor())

## Exposure to mass media ----
tabyl(df_IR_long_imputed$mat_media_newspaper)
df_IR_long_imputed[, media_newspaper := fifelse(mat_media_newspaper == "not at all", 0, 1)]
df_IR_long_imputed[, media_radio := fifelse(mat_media_radio == "not at all", 0, 1)]
df_IR_long_imputed[, media_tv := fifelse(mat_media_tv == "not at all", 0, 1)]
df_IR_long_imputed[, media_internet := fifelse(mat_media_internet == "never", 0, 1)]

#### create any exposure to mass media
df_IR_long_imputed <- df_IR_long_imputed |>
  mutate(mat_media_exp_any = case_when(
    media_newspaper == 1 | media_radio == 1 | media_tv == 1 | media_internet == 1 ~ "yes", 
    is.na(media_internet) ~ "no",
    TRUE ~ "no"
    ))

## Contextual variables ----
### Create a variable for season of birth for India
df_IR_long_imputed$birth_season <- case_when(
  df_IR_long_imputed$dob_month_birth %in% c(1, 2) ~ "winter",
  df_IR_long_imputed$dob_month_birth %in% c(3, 4, 5) ~ "pre-monsoon",
  df_IR_long_imputed$dob_month_birth %in% c(6, 7, 8, 9) ~ "monsoon",
  df_IR_long_imputed$dob_month_birth %in% c(10, 11, 12) ~ "post-monsoon"
) |>
as.factor()

### Create harmonic month variable 
df_IR_long_imputed <- df_IR_long_imputed |> 
  mutate(month_birth_radians = 2 * pi * as.numeric(dob_month_birth) / 12,
    month_sin1 = sin(month_birth_radians),
    month_cos1 = cos(month_birth_radians),
    month_sin2 = sin(2 * month_birth_radians),
    month_cos2 = cos(2 * month_birth_radians))

### Categorize states as high, medium, low based on home-birth 
df_state <- df_IR_long_imputed |> 
  dplyr::group_by(state_name) |> 
    dplyr::summarize(home_birth_rate = mean(dv_home_del, na.rm = T)) |> 
    dplyr::ungroup() |>
  dplyr::mutate(state_home_birth_cat = case_when(
    home_birth_rate < 0.1 ~ "low",
    home_birth_rate >= 0.1 & home_birth_rate < 0.4 ~ "medium",
    home_birth_rate >= 0.4 ~ "high"
  )) |>
  dplyr::select(state_name, home_birth_rate, state_home_birth_cat) 

#### Merge state-level home-birth categories
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::left_join(df_state, by = "state_name")

#### Create a binary variable for home-birth rate
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::mutate(state_home_birth_bi = case_when(
    state_home_birth_cat == "high" ~ "Medium_or_High_HB",
    state_home_birth_cat == "medium" ~ "Medium_or_High_HB",
    state_home_birth_cat == "low" ~ "LowHB"
  ))

### JSY states 
df_IR_long_imputed <- df_IR_long_imputed |> 
  dplyr::mutate(state_janani_bi = case_when(
    state_name %in% c(
      "assam", "bihar", "chhattisgarh", "jharkhand", "madhya pradesh", 
      "odisha", "rajasthan", "uttar pradesh", "uttarakhand",
      "jammu & kashmir") ~ "JSY",
    TRUE ~ "Non-JSY"
  ))

# Step-6: Convert variables to factor --------
setDT(df_IR_long_imputed)
df_IR_long_imputed$psu_fac <- as.factor(df_IR_long_imputed$psu)
df_IR_long_imputed$caseid <- as.factor(df_IR_long_imputed$caseid)
df_IR_long_imputed$dist_name_fac <- as.factor(df_IR_long_imputed$dist_name)
df_IR_long_imputed$state_name_fac <- as.factor(df_IR_long_imputed$state_name)
df_IR_long_imputed$month_birth_fac <- as.factor(df_IR_long_imputed$dob_month_birth)
df_IR_long_imputed$year_birth_fac <- as.factor(df_IR_long_imputed$year_birth)

# Step-7: Save datafile ----
df_IR_long_imputed |> write_fst(path = here(
  path_project, "data", "processed_data", 
  "1.1.2_dhs_IR_vars_created_imp.fst"))