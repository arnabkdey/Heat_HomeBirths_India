# -------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey (arnabxdey@gmail.com), Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script processes loads relevant variables from the DHS data and filters cases for analysis
# @date: Nov 12, 2024

# load-packages ---------------------------------------------------------------
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ------------------------------------------------------------------
source(here("paths-mac.R"))

# Step-1: Identify variables for the paper ------------------------------------
## Meta variables
varlist_meta <- c("caseid", "sdist", "v005", "v008a", "v021", "v023", "v024", "v025")

## Birth History variables
varlist_birth_history <- c(c(paste0("bidx_0",c(1:9))),
                               c(paste0("bidx_",c(10:20))),
                              c(paste0("b18_0",c(1:9))),
                               c(paste0("b18_",c(10:20))),
                               c(paste0("bord_0", c(1:9))),
                               c(paste0("bord_", c(10:20))),
                               c(paste0("midx_", c(1:6))))

## Insitutional delivery variables
varlist_home_birth <- c("m15_1", "m15_2", "m15_3", "m15_4", "m15_5", "m15_6")

## Reason not deliver in institution
varlist_reason_home_birth <- c(c(paste0("m65a_",c(1:6))),
                                 c(paste0("m65b_",c(1:6))),
                                  c(paste0("m65c_",c(1:6))),
                                  c(paste0("m65d_",c(1:6))),
                                  c(paste0("m65e_",c(1:6))),
                                  c(paste0("m65f_",c(1:6))),
                                  c(paste0("m65g_",c(1:6))),
                                  c(paste0("m65h_",c(1:6))),
                                  c(paste0("m65i_",c(1:6))))

## ANC variables
varlist_anc <- c("m14_1", "m14_2", "m14_3", "m14_4", "m14_5", "m14_6")

## SES variables
varlist_ses <- c("s116", "v012", "v106", "v130",  "v135", "v190", "v190a", "v201")

## Access related variables
varlist_access <- c("v467d")

## Empowerment related variables
varlist_empowerment <- c("v739", "v743a", "v743b", "v743c", "v743d", "v743e", "v743f")

## Mass media exposure
varlist_media <- c("v157", "v158", "v159", "v171a")

## Combine the individual varlists
varlist_select <- c(varlist_meta, varlist_birth_history, 
        varlist_home_birth, varlist_reason_home_birth, 
        varlist_anc, varlist_ses, varlist_access,
        varlist_empowerment, varlist_media)

# Step-2: load data ----
## Load raw dataset
df_dhs_IR_raw <- haven::read_dta(here(path_dhs_india_IR, "IAIR7EFL.DTA"),
                    col_select = all_of(varlist_select))

## Convert to factors
df_dhs_IR_raw <- haven::as_factor(df_dhs_IR_raw)
nrow(df_dhs_IR_raw) # 724,115

## Rename and reorganize columns for easy reference
df_dhs_IR_raw <- df_dhs_IR_raw |>
    dplyr::select("caseid", 
        date_int_cdc = v008a,
        wt_raw = v005, 
        psu = v021, 
        rural = v025, 
        strata = v023,
        dist_name = sdist, 
        state_name = v024,
        mat_age = v012, 
        mat_parity = v201, 
        mat_edu_level = v106, 
        mat_emp_spend_self = v739,
        mat_emp_dec_health = v743a,
        mat_emp_dec_purchase_large = v743b,
        mat_emp_dec_purchase_daily = v743c,
        mat_emp_dec_visit = v743d,
        mat_emp_dec_cook = v743e,
        mat_emp_dec_spend_husband = v743f,
        mat_media_newspaper = v157,
        mat_media_radio = v158,
        mat_media_tv = v159,
        mat_media_internet = v171a,
        hh_access_dist = v467d,
        hh_religion = v130, 
        hh_caste = s116, 
        hh_wealth_quintile = v190, 
        hh_wealth_quintile_ru_og = v190a,   
        everything())


# Step-3: Convert from wide to long ----
df_IR_long <- df_dhs_IR_raw |>
    ### Reshape from wide to long to get all births
    tidyr::pivot_longer(cols = bidx_01:m65i_6, 
                    names_to = c('.value', 'Birth'), 
                    names_pattern = '(.*)(\\_+)') |>
    dplyr::select(-Birth) |> 
    # retain only births that are valid
    dplyr::filter(!is.na(m15)) 

nrow(df_IR_long)  # 232,920

## Quick check variables and denominators
dput(colnames(df_IR_long))
nrow(df_IR_long)
sum(is.na(df_IR_long$m15))

# Step-4: Create variables necessary for filtering cases -----------------------
## date of birth and related variables 
df_IR_long <- df_IR_long |>
    ### create UID
    dplyr::mutate(strata_id = as.numeric(factor(paste0(caseid, bidx)))) |>
    ### Filter missing births
    # dplyr::filter(!is.na(m15)) |>
    ### Calculate DOB
    dplyr::mutate(dob = as.Date(b18, origin = "1900-01-01")) |>
    ### Calculate Date of Interview
    dplyr::mutate(doi = as.Date(date_int_cdc, origin = "1900-01-01")) |>
    ### Get Day of the Week, month and year
    dplyr::mutate(dob_week_day = lubridate::wday(dob, label = TRUE)) |>
    dplyr::mutate(dob_week_of_year = lubridate::week(dob)) |>
    dplyr::mutate(dob_month_birth = lubridate::month(dob)) |>
    dplyr::mutate(dob_year_birth = lubridate::year(dob)) 

## variable to identify covid period
df_IR_long <- df_IR_long |>
    dplyr::mutate(covid_period = ifelse(dob >= as.Date("2020-03-25"), 1, 0))

# Step-5: Filter cases for analysis ---- 
## Remove cases where birth happened under COVID
tabyl(df_IR_long$covid_period)
nrow(df_IR_long)  # 232,920
df_IR_long_filtered <- df_IR_long |> dplyr::filter(covid_period == 0)
nrow(df_IR_long_filtered)  # 219,677; 13,243 births dropped during COVID

## remove cases when non-resident
tabyl(df_IR_long_filtered$v135)
df_IR_long_filtered <- df_IR_long_filtered |> dplyr::filter(v135 == "usual resident")
nrow(df_IR_long_filtered)  # 209,643; 10,034 births dropped for non-resident

## remove cases where caste is missing 
tabyl(df_IR_long_filtered$hh_caste)
df_IR_long_filtered <- df_IR_long_filtered |> dplyr::filter(!is.na(hh_caste))
nrow(df_IR_long_filtered)  # 199,345; 10,298 births dropped for missing caste

## remove cases where birth happened neither in facility not at home
tabyl(df_IR_long_filtered$m15)
df_IR_long_filtered <- df_IR_long_filtered |> dplyr::filter(m15 != "other")
nrow(df_IR_long_filtered)  # 198,889; 456 births dropped for other reasons

# Save the data
df_IR_long_filtered |> write_fst(here(path_project, "data", "processed-data", "1.1-dhs-IR-long-raw-filtered.fst"))