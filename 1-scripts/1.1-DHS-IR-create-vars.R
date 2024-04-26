# title: "Create variables for the IR dataset"

# load-packages
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
rm(list = ls())

# Step-1: Identify variables for the paper ----
## Meta variables
varlist_meta <- c("caseid", "v021", "v025", "v023", "sdist", "v024", "v005", "v008a")

## Birth History variables
varlist_birth_history <- c(c(paste0("bidx_0",c(1:9))),
                               c(paste0("bidx_",c(10:20))),
                              c(paste0("b18_0",c(1:9))),
                               c(paste0("b18_",c(10:20))))

## Insitutional delivery variables
varlist_mat_health_utiliz <- c("m15_1", "m15_2", "m15_3", "m15_4", "m15_5", "m15_6")

## SES variables
varlist_ses <- c("v012", "v201", "v106", "v130", "s116", "v190", "v190a", "v135")

## Combine the individual varlists
varlist_select <- c(varlist_meta, varlist_birth_history, varlist_mat_health_utiliz, varlist_ses)

# Step-2: Read the data ----
## Load raw dataset
df_dhs_IR_raw <- haven::read_dta(here("2-data", "2.1-raw-data", "dhs_india_IR", "IAIR7EFL.DTA"),
                    col_select = all_of(varlist_select))

## Convert to factors
df_dhs_IR_raw <- as_factor(df_dhs_IR_raw) 
df_dhs_IR_raw_dt <- setDT(df_dhs_IR_raw)
nrow(df_dhs_IR_raw_dt) # 724,115

# Step-3: Filter cases for analysis ---- 
df_dhs_IR_valid <- df_dhs_IR_raw_dt |>
    dplyr::filter(v135 == "usual resident") |> # dropped 18312 women who were not usual residents
    dplyr::filter(!is.na(m15_1)) |>  # dropped 537290 women who did not have a birth in the last 5 years or did not have info on place of delivery 
    dplyr::filter(!is.na(s116)) |> # dropped 8602 women whose caste was missing
    dplyr::select(-v135)

# nrow(df_dhs_IR_raw_dt)-nrow(df_dhs_IR_valid)-18312-537290
nrow(df_dhs_IR_valid)  # 159,911
# 724115-18312-537290-8602

# Step-4: Convert from wide to long ----
df_IR_long <- df_dhs_IR_valid |>
    dplyr::select("caseid", date_int_cdc = v008a,
                    wt_raw = v005, psu = v021, rural = v025, strata = v023,
                    dist_name = sdist, state_name = v024,
                    mat_age = v012, mat_parity = v201, mat_edu_level = v106, 
                    hh_religion = v130, hh_caste = s116, 
                    hh_wealth_quintile = v190, hh_wealth_quintile_ru_og = v190a,
                    everything()) |>  
    ### Reshape from wide to long to get all births
    tidyr::pivot_longer(cols = bidx_01:m15_6, 
                    names_to = c('.value', 'Birth'), 
                    names_pattern = '(.*)(\\_+)') |>
    dplyr::select(-Birth) |> 
    # retain only births that are valid
    dplyr::filter(!is.na(m15)) 

nrow(df_IR_long)  # 211,406

## Quick check variables and denominators
dput(colnames(df_IR_long))
nrow(df_IR_long)
sum(is.na(df_IR_long))

# Step-5: Create variables ----
## Birth related vars
df_IR_long <- df_IR_long |>
    ### create UID
    dplyr::mutate(strata_id = as.numeric(factor(paste0(caseid, bidx)))) |>
    ### Filter missing births
    dplyr::filter(!is.na(m15)) |>
    ### Calculate DOB
    dplyr::mutate(dob = as.Date(b18, origin = "1900-01-01")) |>
    ### Calculate Date of Interview
    dplyr::mutate(doi = as.Date(date_int_cdc, origin = "1900-01-01")) |>
    ### Get Day of the Week, month and year
    dplyr::mutate(week_day = lubridate::wday(dob, label = TRUE)) |>
    dplyr::mutate(week_of_year = lubridate::week(dob)) |>
    dplyr::mutate(month_birth = lubridate::month(dob)) |>
    dplyr::mutate(year_birth = lubridate::year(dob)) 


## Institutional delivery
df_IR_long <- df_IR_long |> 
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del =
                      ifelse((m15 == "respondent's home" | 
                              m15 == "other home" | 
                              m15 == "parents' home"), 1, 0)) |>
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del_fac = as.factor(dv_home_del)) |>
    dplyr::mutate(dv_inst_del = ifelse(dv_home_del == 1, 0, 1))

## Religion and Caste
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
                                               
## Women's age
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

## Final weight
setDT(df_IR_long)
df_IR_long <- df_IR_long[, wt_final := wt_raw / 1000000]


# Step-6: Convert variables to factor ----
df_IR_long$psu_fac <- as.factor(df_IR_long$psu)
df_IR_long$dist_name_fac <- as.factor(df_IR_long$dist_name)
df_IR_long$state_name_fac <- as.factor(df_IR_long$state_name)
df_IR_long$month_birth_fac <- as.factor(df_IR_long$month_birth)
df_IR_long$year_birth_fac <- as.factor(df_IR_long$year_birth)

# Step-7: Save datafile ----
## Check if the directory exists and create if not
path_processed_data <- here("2-data", "2.2-processed-data")
if (!dir.exists(path_processed_data)) {
  # Create the directory if it does not exist
  dir.create(path_processed_data, showWarnings = TRUE, recursive = TRUE)
}

## Save the file
write_fst(df_IR_long, path = here(path_processed_data, "1.1-dhs-IR-vars-created.fst"))