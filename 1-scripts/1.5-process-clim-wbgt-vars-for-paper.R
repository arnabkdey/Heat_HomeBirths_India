# This code takes about 15 minutes to run on a 32 core PC.

# load-packages ---- 
print(Sys.time())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)

# Read dataset ---- 
rm(list = ls())
## First read the date column
path_processed <- here("2-data", "2.2-processed-data")
df_psu_temp_precip_paper <- read_fst(here(path_processed, "1.4-daily-clim-vars-long-term-by-day.fst"), as.data.table = TRUE)

## Filter cases to 2014 onwards
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[base::`>=`(df_psu_temp_precip_paper$date, as.Date("2014-01-01")), ]

# Step-1: Create terciles ---- 

## Long-term Mean ----- 
df_psu_temp_precip_paper[, lt_tmax_mean_cat_tert_wb := frank(lt_mean_max_temp_wb, ties.method = "min") * 3 / .N, by = day_of_year]
df_psu_temp_precip_paper[, lt_tmax_mean_cat_tert_wb := ceiling(lt_tmax_mean_cat_tert_wb)]
df_psu_temp_precip_paper$lt_tmax_mean_cat_tert_wb <- as.factor(df_psu_temp_precip_paper$lt_tmax_mean_cat_tert_wb)

## Long-term Median -----
df_psu_temp_precip_paper[, lt_tmax_median_cat_tert_wb := frank(lt_median_max_temp_wb, ties.method = "min") * 3 / .N, by = day_of_year]
df_psu_temp_precip_paper[, lt_tmax_median_cat_tert_wb := ceiling(lt_tmax_median_cat_tert_wb)]
df_psu_temp_precip_paper$lt_tmax_median_cat_tert_wb <- as.factor(df_psu_temp_precip_paper$lt_tmax_median_cat_tert_wb)

print("terciles created")
print(Sys.time())

# Step-2: Create extreme heat variables based on absolute temperature cutoffs --------------------------------
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_wb_30 := ifelse(max_temp_wb >= 30, 1, 0)][
        , hotday_wb_31 := ifelse(max_temp_wb >= 31, 1, 0)][
        , hotday_wb_32 := ifelse(max_temp_wb >= 32, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_wb_30 := ifelse(hotday_wb_30 == 1, 1:.N, 0L), by = rleid(hotday_wb_30)][
        , consec_days_wb_31 := ifelse(hotday_wb_31 == 1, 1:.N, 0L), by = rleid(hotday_wb_31)][
        , consec_days_wb_32 := ifelse(hotday_wb_32 == 1, 1:.N, 0L), by = rleid(hotday_wb_32)][
        ## Create heatwave vars for 2, 3, and 5 days
        ### 30 degrees
        , hw_wb_30_2d := ifelse(consec_days_wb_30 >= 2, 1, 0)][
        , hw_wb_30_3d := ifelse(consec_days_wb_30 >= 3, 1, 0)][
        , hw_wb_30_5d := ifelse(consec_days_wb_30 >= 5, 1, 0)][
        ### 31 degrees
        , hw_wb_31_2d := ifelse(consec_days_wb_31 >= 2, 1, 0)][
        , hw_wb_31_3d := ifelse(consec_days_wb_31 >= 3, 1, 0)][
        , hw_wb_31_5d := ifelse(consec_days_wb_31 >= 5, 1, 0)][
        ### 32 degrees
        , hw_wb_32_2d := ifelse(consec_days_wb_32 >= 2, 1, 0)][
        , hw_wb_32_3d := ifelse(consec_days_wb_32 >= 3, 1, 0)][
        , hw_wb_32_5d := ifelse(consec_days_wb_32 >= 5, 1, 0)]

print("Absolute heatwave vars created")
print(Sys.time())

# Step-3: Create extreme heat variables based on n-tiles --------------------------------
## Using cutoffs from day of year approach --------------------------------
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_wb_90_doy := ifelse(max_temp_wb >= cutoff_tmax_wb_doy_90, 1, 0)][
        , hotday_wb_95_doy := ifelse(max_temp_wb >= cutoff_tmax_wb_doy_95, 1, 0)][
        , hotday_wb_97_doy := ifelse(max_temp_wb >= cutoff_tmax_wb_doy_97, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_wb_90_doy := ifelse(hotday_wb_90_doy == 1, 1:.N, 0L), by = rleid(hotday_wb_90_doy)][
        , consec_days_wb_95_doy := ifelse(hotday_wb_95_doy == 1, 1:.N, 0L), by = rleid(hotday_wb_95_doy)][
        , consec_days_wb_97_doy := ifelse(hotday_wb_97_doy == 1, 1:.N, 0L), by = rleid(hotday_wb_97_doy)][
        ## Create heatwave vars for 2, 3, and 5 days
        ### 90th percentile
        , hw_wb_90_doy_2d := ifelse(consec_days_wb_90_doy >= 2, 1, 0)][
        , hw_wb_90_doy_3d := ifelse(consec_days_wb_90_doy >= 3, 1, 0)][
        , hw_wb_90_doy_5d := ifelse(consec_days_wb_90_doy >= 5, 1, 0)][
        ### 95th percentile
        , hw_wb_95_doy_2d := ifelse(consec_days_wb_95_doy >= 2, 1, 0)][
        , hw_wb_95_doy_3d := ifelse(consec_days_wb_95_doy >= 3, 1, 0)][
        , hw_wb_95_doy_5d := ifelse(consec_days_wb_95_doy >= 5, 1, 0)][
        ### 97th percentile
        , hw_wb_97_doy_2d := ifelse(consec_days_wb_97_doy >= 2, 1, 0)][
        , hw_wb_97_doy_3d := ifelse(consec_days_wb_97_doy >= 3, 1, 0)][
        , hw_wb_97_doy_5d := ifelse(consec_days_wb_97_doy >= 5, 1, 0)]

print("Doy heatwave vars created")
print(Sys.time())

## Using cutoffs from quantile harmonic regression approach --------------------------------
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_wb_90_harmo := ifelse(max_temp_wb >= cutoff_tmax_wb_harmo_90, 1, 0)][
        , hotday_wb_95_harmo := ifelse(max_temp_wb >= cutoff_tmax_wb_harmo_95, 1, 0)][
        , hotday_wb_97_harmo := ifelse(max_temp_wb >= cutoff_tmax_wb_harmo_97, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_wb_90_harmo := ifelse(hotday_wb_90_harmo == 1, 1:.N, 0L), by = rleid(hotday_wb_90_harmo)][
        , consec_days_wb_95_harmo := ifelse(hotday_wb_95_harmo == 1, 1:.N, 0L), by = rleid(hotday_wb_95_harmo)][
        , consec_days_wb_97_harmo := ifelse(hotday_wb_97_harmo == 1, 1:.N, 0L), by = rleid(hotday_wb_97_harmo)][
        ## Create heatwave vars for 2, 3, and 5 days
        ## 90th percentile
        , hw_wb_90_harmo_2d := ifelse(consec_days_wb_90_harmo >= 2, 1, 0)][
        , hw_wb_90_harmo_3d := ifelse(consec_days_wb_90_harmo >= 3, 1, 0)][
        , hw_wb_90_harmo_5d := ifelse(consec_days_wb_90_harmo >= 5, 1, 0)][
        ## 95th percentile
        , hw_wb_95_harmo_2d := ifelse(consec_days_wb_95_harmo >= 2, 1, 0)][
        , hw_wb_95_harmo_3d := ifelse(consec_days_wb_95_harmo >= 3, 1, 0)][
        , hw_wb_95_harmo_5d := ifelse(consec_days_wb_95_harmo >= 5, 1, 0)][
        ## 97th percentile
        , hw_wb_97_harmo_2d := ifelse(consec_days_wb_97_harmo >= 2, 1, 0)][
        , hw_wb_97_harmo_3d := ifelse(consec_days_wb_97_harmo >= 3, 1, 0)][
        , hw_wb_97_harmo_5d := ifelse(consec_days_wb_97_harmo >= 5, 1, 0)]

print("Harmonic heatwave vars created")
print(Sys.time())

# Save dataset ---- 
write_fst(df_psu_temp_precip_paper, path = here(path_processed, "1.5-dhs-psu-paper.fst"))
print("All tasks complete for 1.5")