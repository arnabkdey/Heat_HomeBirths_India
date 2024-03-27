# title: "Create variables for the IR dataset"

## load-packages
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive)

# Step-0: Download DHS files from Google Drive to local files ----
## Create a local folder to download files
if (!dir.exists("./data/gdrive_temp_files_input/dhs_india_IR")) {
  # Create the directory if it does not exist
  dir.create("./data/gdrive_temp_files_input/dhs_india_IR", showWarnings = TRUE, recursive = TRUE)
}

## List all files in the Google Drive folder
folder_id_dhs_india_IR <- "1hBVjceKjs-TIskYC9ehR-8v3roW6rHjj"
files_dhs_india_IR <- googledrive::drive_ls(as_id(folder_id_dhs_india_IR))
file_id_dta <- files_dhs_india_IR %>% dplyr::filter(str_detect(name, ".DTA")) %>% dplyr::pull(id)
file_name_dta <- files_dhs_india_IR %>% dplyr::filter(str_detect(name, ".DTA")) %>% dplyr::pull(name)

## Download the relevant file to the local folder
drive_download(as_id(file_id_dta), path = paste0("./data/gdrive_temp_files_input/dhs_india_IR/", file_name_dta))

## Step-1: Identify variables for the paper
### Meta
varlist_meta <- c("caseid", "v021", "v025", "v023", "sdist", "v024", "v005", "v040", "v008a")

### Birth History
varlist_birth_history <- c(c(paste0("bidx_0",c(1:9))),
                               c(paste0("bidx_",c(10:20))),
                              c(paste0("b18_0",c(1:9))),
                               c(paste0("b18_",c(10:20))))

### Maternal Healthcare Utilization
varlist_mat_health_utiliz <- c(
							"m15_1", "m15_2", "m15_3",
                            "m15_4", "m15_5", "m15_6", 
							"m61_1", "m61_2", "m61_3", 
							"m61_4","m61_5", "m61_6",
							"m62_1", "m62_2", "m62_3", 
							"m62_4","m62_5", "m62_6")
# m15: place of delivery
# m61: time spent at place of delivery
# m62: respondents' health checked before discharge                            

### SES variables
varlist_ses <- c("v012", "v201", "v106", "v130", "s116", 
            "v190", "v191", "v190a")

### Distance to healthcare
varlist_access_health <- c("v467d")

### Assets
varlist_assets <- c("v119", "v122", "v124", "v125")

### Combine the individual varlists
varlist_select <- c(varlist_meta, varlist_birth_history, varlist_mat_health_utiliz, varlist_ses, varlist_access_health, varlist_assets)

## Step-2: Read the data
### Load raw dataset
df_dhs_IR_raw <- haven::read_dta("./data/gdrive_temp_files_input/dhs_india_IR/IAIR7EFL.DTA",
                    col_select = all_of(varlist_select))
### Convert to factors
df_dhs_IR_raw <- as_factor(df_dhs_IR_raw) 
df_dhs_IR_raw_dt <- setDT(df_dhs_IR_raw)

## Step-3: Rename basic variables
## Rename Basic Variables
df_dhs_IR_raw_dt <- 
  setnames(df_dhs_IR_raw_dt,
    old = c("v021", "v025", "v023", "sdist", 
            "v024", "v005", "v040", "v012", 
            "v201", "v106", "v130", "s116", 
            "v190", "v191", "v190a", 
            "v119", "v122", "v124", "v125"),
    new = c("psu", "rural", "strata", 
            "dist_name", "state_name", 
            "wt_raw", "psu_alt_mts", "mat_age", 
            "mat_parity", "mat_edu_level", 
            "hh_religion", "hh_caste",
            "hh_wealth_quintile_og",
            "hh_wealth_score_og", 
            "hh_wealth_quintile_ru_og", 
            "electricity", "refrigerator", "motorcycle", "car"))
# dput(colnames(df_dhs_IR_raw_dt))

## Step-4: Convert from wide to long
df_IR_long <- df_dhs_IR_raw_dt |>
    dplyr::select("caseid", "wt_raw", "mat_age", "psu", "strata", "state_name",
            "rural", "psu_alt_mts", "mat_edu_level", "electricity", "refrigerator",
            "motorcycle", "car", "hh_religion", "hh_wealth_quintile_og",
            "hh_wealth_score_og", "hh_wealth_quintile_ru_og", "mat_parity", "v467d",
            "dist_name", "hh_caste", everything()) |>
    ### Reshape from wide to long to get all births
    tidyr::pivot_longer(cols = bidx_01:m62_6, 
                    names_to = c('.value', 'Birth'), 
                    names_pattern = '(.*)(\\_+)') |>
    dplyr::select(-Birth)

## Step-5: Create variables
### Birth related vars
df_IR_long <- df_IR_long |>
    ### create UID
    dplyr::mutate(strata_id = as.numeric(factor(paste0(caseid, bidx)))) |>
    ### Filter missing births
    dplyr::filter(!is.na(m15)) |>
    ### Calculate DOB
    dplyr::mutate(dob = as.Date(b18, origin = "1900-01-01")) |>
    ### Calculate Date of Interview
    dplyr::mutate(doi = as.Date(v008a, origin = "1900-01-01")) |>
    ### Get Day of the Week, month and year
    dplyr::mutate(week_day = lubridate::wday(dob, label = TRUE)) |>
    dplyr::mutate(week_of_year = lubridate::week(dob)) |>
    dplyr::mutate(month_birth = lubridate::month(dob)) |>
    dplyr::mutate(year_birth = lubridate::year(dob)) |>
    dplyr::mutate(month_cat = case_when(
                    month_birth < 4 ~ "Jan-Mar",
                    month_birth > 3 & month_birth < 10 ~ "Apr-Sep",
                    month_birth > 9 ~ "Oct-Dec")) |>
    dplyr::mutate(year_birth_club = case_when(
                    year_birth == 2014 ~ "2014-15",
                    year_birth == 2015 ~ "2014-15",
                    year_birth == 2016 ~ "2016",
                    year_birth == 2017 ~ "2017",
                    year_birth == 2018 ~ "2018",
                    year_birth == 2019 ~ "2019",
                    TRUE ~ "2020-21")) |>                   
    dplyr::mutate(summer = ifelse(month_birth > 4 & month_birth < 10, "yes", "no")) 

### Outcome Variables

#### institutional delivery
df_IR_long <- df_IR_long |> 
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del =
                      ifelse((m15 == "respondent's home" | 
                              m15 == "other home" | 
                              m15 == "parents' home"), 1, 0)) |>
    ### Create variable for institutional delivery
    dplyr::mutate(dv_home_del_fac = as.factor(dv_home_del)) |>
    dplyr::mutate(dv_inst_del = ifelse(dv_home_del == 1, 0, 1))

# freq(df_IR_long$dv_home_del)
# freq(df_IR_long$dv_inst_del)
# ctable(as.factor(df_IR_long$dv_home_del), df_IR_long$m62)

### SES variables
df_IR_long <- setDT(df_IR_long)
#### Social
df_IR_long <- df_IR_long[                     
                    # Religion
                    ,   hh_religion_club := fcase(
                            hh_religion == "hindu", "hindu",
                            hh_religion == "muslim", "muslim",
                            hh_religion == "christian", "christian",
                            default = "other")][
                    ## Relevel Religion levels
                    , hh_religion_club := 
                        fct_relevel(hh_religion_club, "hindu", 
                                    "muslim", "christian", "other")][
                    # Caste
                    , hh_caste_club := fcase(
                        hh_caste == "schedule caste", "SC",
                        hh_caste == "schedule tribe", "ST",
                        hh_caste == "obc", "OBC",
                        default = "Other")][
                    # Two level variables for religion and caste
                    ,   hh_religion_bi := ifelse(hh_religion == "hindu", "hindu", "not-hindu")][
                    ,    hh_caste_bi := ifelse(hh_caste == "none of them" | 
                                                hh_caste == "don't know" | is.na(hh_caste), 1, 0)][
                    # Three level variable for religion and caste
                    ,   hh_religion_tri := case_when(hh_religion == "hindu" ~ "hindu",
                                                    hh_religion == "muslim" ~ "muslim",
                                                    TRUE ~ "other")][
                    ,   hh_caste_tri := case_when(hh_caste == "OBC" ~ "OBC",
                                                hh_caste == "Other" ~ "Other",
                                                TRUE ~ "SC/ST")]
                                               
#### Access to healthcare 
df_IR_long <- df_IR_long[, access_issue_distance := ifelse(v467d == "big problem", "big-problem", "not-a-big-prob")]

#### Women's age group ----
df_IR_long <- df_IR_long |> 
                    mutate(mat_age_grp = case_when(
                      mat_age < 25  ~ "15-24",
                      mat_age >= 25 & mat_age < 35  ~ "25-34",
                      mat_age >= 35 & mat_age < 50  ~ "35-49"))

#### Women's age at birth 
df_IR_long <- df_IR_long |> 
    # First difference between date of interview and date of birth (in days)
    mutate(diff_doi_dob = doi - dob) |> 
    # Calculate age at birth
    mutate(mat_age_at_birth = mat_age - as.numeric(diff_doi_dob, units = "days")/365.25) |> 
    mutate(mat_age_at_birth = round(mat_age_at_birth, 0))


#### Women's age group at birth ----
df_IR_long <- df_IR_long |> 
                    mutate(mat_age_grp_at_birth = case_when(
                      mat_age_at_birth < 20  ~ "15-19",
                      mat_age_at_birth >= 20 & mat_age_at_birth < 25  ~ "20-24",
                      mat_age_at_birth >= 25 & mat_age_at_birth < 30  ~ "25-29",
                      mat_age_at_birth >= 30 ~ "30-49"))


### PSU related variables ----- 
#### centered variable for PSU altitude in meters
df_IR_long$altitude_center <- scale(df_IR_long$psu_alt_mts, center = TRUE)

#### Create num_cases per PSU >= 10 ----
df_IR_long <- df_IR_long |> 
                group_by(psu) |> 
                    mutate(num_cases = n()) |> 
                    ungroup() |>
                mutate(num_case_10 = ifelse(num_cases >= 10, 1, 0)) 


### Convert variables to factor ---
df_IR_long$psu_fac <- as.factor(df_IR_long$psu)
df_IR_long$dist_name_fac <- as.factor(df_IR_long$dist_name)
df_IR_long$state_name_fac <- as.factor(df_IR_long$state_name)
df_IR_long$month_birth_fac <- as.factor(df_IR_long$month_birth)
df_IR_long$year_birth_fac <- as.factor(df_IR_long$year_birth)


### Final weight
setDT(df_IR_long)
df_IR_long <- df_IR_long[, wt_final := wt_raw / 1000000]

## Step-4: Save datafile

### Check if the directory exists and create if not
if (!dir.exists("./data/processed-data/")) {
  # Create the directory if it does not exist
  dir.create("./data/processed-data/", showWarnings = TRUE, recursive = TRUE)
}

### Save the file
write_fst(df_IR_long, path = "./data/processed-data/1.1-dhs-IR-vars-created.fst")

