# -------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey (arnabxdey@gmail.com), Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script processes loads relevant variables from the DHS data and filters cases for analysis
# @date: Nov 12, 2024

# load-packages ---------------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)

# set paths ------------------------------------------------------------------
source(here("paths-mac.R"))

# load the data --------------------------------------------------------------
df_IR_long <- read_fst(here(path_project, "data", "processed-data", "1.1-dhs-IR-long-raw-filtered.fst"))
setDT(df_IR_long)

# Create variables for full dataset -------------------------------------------

## Final weight -------------------------------------------
df_IR_long <- df_IR_long[, wt_final := wt_raw / 1000000]

## Institutional delivery -----------------------
df_IR_long <- df_IR_long |> 
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del =
                      ifelse((m15 == "respondent's home" | 
                              m15 == "other home" | 
                              m15 == "parents' home"), 1, 0)) |>
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del_fac = as.factor(dv_home_del)) |>
    dplyr::mutate(dv_inst_del = ifelse(dv_home_del == 1, 0, 1))

## Religion and Caste ---------------------------
df_IR_long <- df_IR_long |>
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
    hh_religion_club = fct_relevel(hh_religion_club, "hindu", "muslim", "christian", "other")
  ) |>
  # Caste classification
  mutate(
    hh_caste_club = case_when(
      hh_caste == "schedule caste" ~ "SC",
      hh_caste == "schedule tribe" ~ "ST",
      hh_caste == "obc" ~ "OBC",
      TRUE ~ "Other"
    )
  ) |>
  # Two level variables for religion
  mutate(
    hh_religion_bi = if_else(hh_religion == "hindu", "hindu", "not-hindu")
  ) |>
  # Two level variables for caste
  mutate(
    hh_caste_bi = if_else(hh_caste == "none of them" | hh_caste == "don't know" | is.na(hh_caste), 1, 0)
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
      hh_caste == "OBC" ~ "OBC",
      hh_caste == "Other" ~ "Other",
      TRUE ~ "SC/ST"
    )
  )

## Maternal education ---------------------------
df_IR_long$mat_edu_level_bi <- ifelse(df_IR_long$mat_edu_level == "no education", 
  "no education", "primary or higher")

## Women's age related variables ---------------------------
### Current age at interview
df_IR_long <- df_IR_long |> 
                    mutate(mat_age_grp = case_when(
                      mat_age < 25  ~ "15-24",
                      mat_age >= 25 & mat_age < 35  ~ "25-34",
                      mat_age >= 35 & mat_age < 50  ~ "35-49"))

### Age at birth 
df_IR_long <- df_IR_long |> 
    # First difference between date of interview and date of birth (in days)
    mutate(diff_doi_dob = doi - dob) |> 
    # Calculate age at birth
    mutate(mat_age_at_birth = mat_age - as.numeric(diff_doi_dob, units = "days")/365.25) |> 
    mutate(mat_age_at_birth = round(mat_age_at_birth, 0))


### Women's age group at birth
df_IR_long <- df_IR_long |> 
                    mutate(mat_age_grp_at_birth = case_when(
                      mat_age_at_birth < 20  ~ "15-19",
                      mat_age_at_birth >= 20 & mat_age_at_birth < 25  ~ "20-24",
                      mat_age_at_birth >= 25 & mat_age_at_birth < 30  ~ "25-29",
                      mat_age_at_birth >= 30 ~ "30-49"))

### binary variable for age at birth
df_IR_long <- df_IR_long |> 
                    mutate(mat_age_at_birth_bi = case_when(
                      mat_age_at_birth < 25  ~ "15-24",
                      mat_age_at_birth >= 25  ~ "25-49"))                      


## Access to healthcare ---------------------------
df_IR_long <- df_IR_long |> 
                dplyr::mutate(hh_access_issue_distance = 
                  ifelse(hh_access_dist == "big problem", "big-problem", "not-a-big-prob"))

## Birth order ---------------------------
tabyl(df_IR_long, bord)
df_IR_long <- df_IR_long |> 
                dplyr::mutate(mat_birth_order = case_when(
                  bord == 1 ~ "One",
                  bord == 2 ~ "Two",
                  bord == 3 ~ "Three",
                  bord >= 4 ~ "Four or more"))

### relevel factors 
df_IR_long <- df_IR_long |> 
  dplyr::mutate(mat_birth_order = factor(mat_birth_order, levels = c("One", "Two", "Three", "Four or more")))

tabyl(df_IR_long, mat_birth_order)

## Empowerment variables ----
### Decisions related to healthcare ----
tabyl(df_IR_long, mat_emp_dec_health)
df_IR_long[, mat_emp_healthcare := 
        case_when(mat_emp_dec_health == "respondent alone" ~ "high",
                grepl("and", mat_emp_dec_health, fixed = TRUE) ~ "moderate",
                is.na(mat_emp_dec_health) ~ NA_character_,
                TRUE ~ "low")]
tabyl(df_IR_long$mat_emp_healthcare)

### Decisions related to large household purchases ----
tabyl(df_IR_long, mat_emp_dec_purchase_large)
df_IR_long[, mat_emp_purchases := 
        case_when(mat_emp_dec_purchase_large == "respondent alone" ~ "high",
                grepl("and", mat_emp_dec_purchase_large, fixed = TRUE) ~ "moderate",
                is.na(mat_emp_dec_purchase_large) ~ NA_character_,
                TRUE ~ "low")]
tabyl(df_IR_long$mat_emp_purchases)

### Decisions related to visits to family or relatives ----
tabyl(df_IR_long, mat_emp_dec_visit)
df_IR_long[, mat_emp_visits := 
        case_when(mat_emp_dec_visit == "respondent alone" ~ "high",
                grepl("and", mat_emp_dec_visit, fixed = TRUE) ~ "moderate",
                is.na(mat_emp_dec_visit) ~ NA_character_,
                TRUE ~ "low")]
tabyl(df_IR_long$mat_emp_visits)

### Decisions related to what to do with money husband earns ----
tabyl(df_IR_long, mat_emp_dec_spend_husband)
df_IR_long[, mat_emp_money_husb := 
        case_when(mat_emp_dec_spend_husband == "respondent alone" ~ "high",
                grepl("and", mat_emp_dec_spend_husband, fixed = TRUE) ~ "moderate",
                is.na(mat_emp_dec_spend_husband) ~ NA_character_,
                TRUE ~ "low")]
tabyl(df_IR_long$mat_emp_money_husb)

### Decisions related to what to do with self earnings ----
tabyl(df_IR_long, mat_emp_spend_self)
df_IR_long[, mat_emp_money_self := 
        case_when(mat_emp_spend_self == "respondent alone" ~ "high",
                grepl("and", mat_emp_spend_self, fixed = TRUE) ~ "moderate",
                is.na(mat_emp_spend_self) ~ NA_character_,
                TRUE ~ "low")]
tabyl(df_IR_long$mat_emp_money_self)

## Mass media exposure ---------------------------
tabyl(df_IR_long$mat_media_newspaper)
df_IR_long[, media_newspaper := fifelse(mat_media_newspaper == "not at all", 0, 1)]
df_IR_long[, media_radio := fifelse(mat_media_radio == "not at all", 0, 1)]
df_IR_long[, media_tv := fifelse(mat_media_tv == "not at all", 0, 1)]
df_IR_long[, media_internet := fifelse(mat_media_internet == "never", 0, 1)]

### create any exposure to mass media
df_IR_long <- df_IR_long |>
  mutate(mat_media_exp_any = case_when(
    media_newspaper == 1 | media_radio == 1 | media_tv == 1 | media_internet == 1 ~ "yes", 
    is.na(media_internet) ~ "no",
    TRUE ~ "no"
    ))

tabyl(df_IR_long$mat_media_exp_any)

## Categorize states as high, medium, low based on home-birth ----
df_state <- df_IR_long |> 
  dplyr::group_by(state_name) |> 
    dplyr::summarize(home_birth_rate = mean(dv_home_del, na.rm = T)) |> 
    dplyr::ungroup() |>
  dplyr::mutate(state_home_birth_cat = case_when(
    home_birth_rate < 0.1 ~ "low",
    home_birth_rate >= 0.1 & home_birth_rate < 0.4 ~ "medium",
    home_birth_rate >= 0.4 ~ "high"
  )) |>
  dplyr::select(state_name, home_birth_rate, state_home_birth_cat) 

tabyl(df_state$state_home_birth_cat)

## Merge state-level home-birth categories
df_IR_long <- df_IR_long |> 
  dplyr::left_join(df_state, by = "state_name")

## Create a binary variable for home-birth rate
df_IR_long <- df_IR_long |> 
  dplyr::mutate(state_home_birth_bi = case_when(
    state_home_birth_cat == "high" ~ "high",
    state_home_birth_cat == "medium" ~ "high",
    state_home_birth_cat == "low" ~ "low"
  ))

# Step-6: Convert variables to factor --------
df_IR_long$psu_fac <- as.factor(df_IR_long$psu)
df_IR_long$caseid <- as.factor(df_IR_long$caseid)
df_IR_long$dist_name_fac <- as.factor(df_IR_long$dist_name)
df_IR_long$state_name_fac <- as.factor(df_IR_long$state_name)
df_IR_long$month_birth_fac <- as.factor(df_IR_long$dob_month_birth)
df_IR_long$year_birth_fac <- as.factor(df_IR_long$year_birth)

# Step-7: Save datafile ----
df_IR_long |> write_fst(path = here(path_project, "data", "processed-data", "1.2-dhs-IR-vars-created.fst"))