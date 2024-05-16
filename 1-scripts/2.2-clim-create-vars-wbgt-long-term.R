# This script takes about 34 hours to run on a 32 core PC.

## Title: "Create temperature related variables specific to PSU and week/month"

# This script creates climate vars based on long term trends
# That is, this script create variables to capture temperature extremes local to space AND time.

# load-packages ---------------------------------------------------------
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
library(forecast)
library(quantreg)
print(Sys.time())
rm(list = ls())

# Constants ---------------------------------------------------------
path_processed <- here("2-data", "2.2-processed-data")

# load script with functions ---------------------------------------------------------
source(here("1-scripts", "5.4-function-for-lycday-and-harmonics.R"))
source(here("1-scripts", "5.5-function-for-parallel-proc-quantile-reg.R"))


# load datasets needed for this script --------------------------------
df_psu_temp_precip_vars_added <- read_fst(here(path_processed, "2.1-daily-temp-precip-1980-21-extracted-dhs-psu.fst"), as.data.table = T)
colnames(df_psu_temp_precip_vars_added)
print("loading complete")
print(Sys.time())

## Retain complete cases only
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[complete.cases(df_psu_temp_precip_vars_added), ]

# Step-0: Data Preparation --------------------------------
## Create basic date variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, ':=' (
                                    week_of_year = lubridate::week(date),
                                    month = lubridate::month(date),
                                    day_of_year = lubridate::yday(date))]

# Step-1: Create cutoffs based on harmonic quantile regression --------------------------------

## Create Lyc Day variable --------------------------------
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, lycday := lycday(date)]

## Append Harmonics --------------------------------
annual_period_in_days <- 365.25
df_psu_temp_precip_vars_added <- append_harmonics(df = df_psu_temp_precip_vars_added, n_harmonics = 4)

## Create a formula for harmonic quantile regression ---------------------------
formula <- stats::as.formula(paste0("max_temp_wb ~ ",
                                     paste(names(dplyr::select(.data = df_psu_temp_precip_vars_added,
                                                               dplyr::starts_with("harmonic"))),
                                           collapse = " + ")))


## Create cutoff variables using quantile harmonic approach ---------------------------
### 90th percentile
df_psu_temp_precip_vars_added <- parallel_quantile_regression(df = df_psu_temp_precip_vars_added, 
                                                               formula = formula, 
                                                               variable_name = "cutoff_tmax_wb_harmo_", 
                                                               quantile = 0.90)
### 95th percentile
df_psu_temp_precip_vars_added <- parallel_quantile_regression(df = df_psu_temp_precip_vars_added, 
                                                               formula = formula, 
                                                               variable_name = "cutoff_tmax_wb_harmo_", 
                                                               quantile = 0.95)

### 97th percentile
df_psu_temp_precip_vars_added <- parallel_quantile_regression(df = df_psu_temp_precip_vars_added, 
                                                               formula = formula, 
                                                               variable_name = "cutoff_tmax_wb_harmo_", 
                                                               quantile = 0.97)
print("step-1-complete")
print(Sys.time())

# Step-2: Create cutoff variables at 90th, 95th, and 97th percentiles using 'day of year' approach ---------------------------
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , cutoff_tmax_wb_doy_90 := quantile(max_temp_wb, probs = 0.90, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_doy_95 := quantile(max_temp_wb, probs = 0.95, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_doy_97 := quantile(max_temp_wb, probs = 0.97, na.rm = T), by = c("psu", "day_of_year")]
    
print("step-2-complete")
print(Sys.time())

# Step-3: Calculate long-term mean ---------------------------------
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## long-term means by week by PSU
    , lt_mean_max_temp_wb := mean(max_temp_wb, na.rm = T), by = c("psu", "day_of_year")]


print("step-3-complete")
print(Sys.time())

# Step-4: Calculate long-term median using quantile regression approach ---------------------------------
df_psu_temp_precip_vars_added <- parallel_quantile_regression(df = df_psu_temp_precip_vars_added, 
                                                               formula = formula, 
                                                               variable_name = "lt_median_max_temp_wb_", 
                                                               quantile = 0.50)

## Rename variable 
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , lt_median_max_temp_wb := lt_median_max_temp_wb_50]

print("step-4-complete")
print(Sys.time())

# Save Work ---------------------------------------------------------
write_fst(df_psu_temp_precip_vars_added, path = here(path_processed, "2.2-daily-clim-vars-long-term-by-day.fst"))

print("Script 2.2 complete")
print(Sys.time())