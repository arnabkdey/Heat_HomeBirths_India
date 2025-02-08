
## Title: "Create temperature related variables specific to PSU and week/month"

# This script creates climate vars based on long term trends
# That is, this script create variables to capture temperature extremes local to space AND time.

# load-packages ---------------------------------------------------------
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, openxlsx, here, forecast, quantreg)
pacman::p_load(parallel, doParallel, foreach)
print(Sys.time())

source(here("paths-mac.R"))

# Constants ---------------------------------------------------------
path_processed <- here(path_project, "data", "processed-data")

# load script with functions ---------------------------------------------------------
source(here("R", "6.5-function-for-lycday-and-harmonics.R"))
source(here("R", "6.6-function-for-parallel-proc-quantile-reg.R"))

# load datasets needed for this script --------------------------------
df_psu_temp_precip_vars_added <- read_fst(here(path_processed, "1.4-daily-temp-precip-1980-21-extracted-dhs-psu.fst"), 
                                          as.data.table = T)
print("loading complete")
print(Sys.time())

## Retain complete cases only
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[complete.cases(df_psu_temp_precip_vars_added), 
                                                               c("psu", "date", "max_temp_wb", "max_temp", "mean_precip")]


# Step-1: Create percentile cutoffs for WBGT tmax ---------------------------
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , cutoff_tmax_wb_90 := quantile(max_temp_wb, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_95 := quantile(max_temp_wb, probs = 0.95, na.rm = T), by = c("psu")][
    , cutoff_tmax_wb_97 := quantile(max_temp_wb, probs = 0.97, na.rm = T), by = c("psu")]

# Step-2: Create percentile cutoffs for WBGT tmax ---------------------------
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    , cutoff_tmax_db_90 := quantile(max_temp, probs = 0.90, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_95 := quantile(max_temp, probs = 0.95, na.rm = T), by = c("psu")][
    , cutoff_tmax_db_97 := quantile(max_temp, probs = 0.97, na.rm = T), by = c("psu")]

head(df_psu_temp_precip_vars_added)
print("step-2-complete")
print(Sys.time())

# Step-3: Calculate long-term mean ---------------------------------
## Create basic date variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, ':=' (
                                    day_of_year = lubridate::yday(date))]
## calculate LT mean
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## long-term means by week by PSU
    , lt_mean_max_temp_wb := mean(max_temp_wb, na.rm = T), by = c("psu", "day_of_year")]


print("step-3-complete")
print(Sys.time())

# Save Work ---------------------------------------------------------
df_psu_temp_precip_vars_added |> write_fst(path = here(path_project, "data", "processed-data", "1.5-daily-clim-vars-long-term.fst"))
print("Script 1.5 complete")
print(Sys.time())