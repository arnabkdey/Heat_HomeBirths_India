## Title: "Create temperature related variables specific to PSU and week/month"

# This script creates climate vars based on long term trends
# That is, this script create variables to capture temperature extremes local to space AND time.

# load-packages
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)
library(forecast)
print(Sys.time())
rm(list = ls())

# load datasets needed for this script
path_processed <- here("2-data", "2.2-processed-data")
df_psu_temp_precip_vars_added <- read_fst(here(path_processed, "2.2-daily-temp-precip-1980-21-extracted-dhs-psu.fst"), as.data.table = T)
colnames(df_psu_temp_precip_vars_added)
print("loading complete")
print(Sys.time())

# Retain complete cases only
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[complete.cases(df_psu_temp_precip_vars_added), ]

# Create basic date variables
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[, ':=' (
                                    week_of_year = lubridate::week(date),
                                    month = lubridate::month(date),
                                    day_of_year = lubridate::yday(date))]

# Step-1: Create temperature variables that need long-term data
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## 90th/95th percentile variables by week by PSU
    ### Tmax-Wb
    , cutoff_tmax_wb_90 := quantile(max_temp_wb, probs = 0.90, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_95 := quantile(max_temp_wb, probs = 0.95, na.rm = T), by = c("psu", "day_of_year")][
    , cutoff_tmax_wb_97 := quantile(max_temp_wb, probs = 0.97, na.rm = T), by = c("psu", "day_of_year")]
    
print("step-1-complete")
print(Sys.time())

# Step-2: Calculate long-term means
df_psu_temp_precip_vars_added <- df_psu_temp_precip_vars_added[
    ## long-term means by week by PSU
    , lt_mean_max_temp_wb := mean(max_temp_wb, na.rm = T), by = c("psu", "day_of_year")]

print("step-2-complete")
print(Sys.time())

# Save Work
write_fst(df_psu_temp_precip_vars_added, path = here(path_processed, "2.3-daily-clim-vars-long-term-by-day.fst"))

print("Script 2.3 complete")
print(Sys.time())