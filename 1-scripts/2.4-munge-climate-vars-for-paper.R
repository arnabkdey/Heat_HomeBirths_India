# load-packages ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, here)

# Read dataset ---- 
rm(list = ls())
## First read the date column
path_processed <- here("2-data", "2.2-processed-data")
df_psu_temp_precip_paper <- read_fst(here(path_processed, "2.3-daily-clim-vars-long-term-by-day.fst"), as.data.table = TRUE)

## Filter cases to 2014 onwards
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[base::`>=`(df_psu_temp_precip_paper$date, as.Date("2014-01-01")), ]

# Step-1: Create terciles ---- 

### Long-term Tmax ----- 
#### Wet Bulb
df_psu_temp_precip_paper[, lt_tmax_cat_tert_wb := frank(lt_mean_max_temp_wb, ties.method = "min") * 3 / .N, by = day_of_year]
df_psu_temp_precip_paper[, lt_tmax_cat_tert_wb := ceiling(lt_tmax_cat_tert_wb)]
df_psu_temp_precip_paper$lt_tmax_cat_tert_wb <- as.factor(df_psu_temp_precip_paper$lt_tmax_cat_tert_wb)

### Tmax (on the day of) -----
#### Wet Bulb ----- 
df_psu_temp_precip_paper[, tmax_cat_tert_wb := frank(max_temp_wb, ties.method = "min") * 3 / .N, by = day_of_year]
df_psu_temp_precip_paper[, tmax_cat_tert_wb := ceiling(tmax_cat_tert_wb)]
df_psu_temp_precip_paper$tmax_cat_tert_wb <- as.factor(df_psu_temp_precip_paper$tmax_cat_tert_wb)

print("terciles created")
print(Sys.time())

# Step-2: Create extreme heat days ----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        ### Wetbulb
        , hotday_90_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_90, 1, 0)][
        , hotday_95_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_95, 1, 0)][
        , hotday_97_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_97, 1, 0)][
        ## Based on absoulte values
        ### Wetbulb
        , hotday_30_wb := ifelse(max_temp_wb >= 30, 1, 0)][
        , hotday_31_wb := ifelse(max_temp_wb >= 31, 1, 0)][
        , hotday_32_wb := ifelse(max_temp_wb >= 32, 1, 0)]

print("extreme heat days created")



# Step-4: Create Heatwave vars for 2,3, and 5 days -----
## First create consecutive days of extreme temperature ----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        ### Wetbulb
        , consec_days_90_wb := ifelse(hotday_90_wb == 1, 1:.N, 0L), by = rleid(hotday_90_wb)][
        , consec_days_95_wb := ifelse(hotday_95_wb == 1, 1:.N, 0L), by = rleid(hotday_95_wb)][
        , consec_days_97_wb := ifelse(hotday_97_wb == 1, 1:.N, 0L), by = rleid(hotday_97_wb)][
        ## Based on absoulte values
        ### Wetbulb
        , consec_days_30_wb := ifelse(hotday_30_wb == 1, 1:.N, 0L), by = rleid(hotday_30_wb)][
        , consec_days_31_wb := ifelse(hotday_31_wb == 1, 1:.N, 0L), by = rleid(hotday_31_wb)][
        , consec_days_32_wb := ifelse(hotday_32_wb == 1, 1:.N, 0L), by = rleid(hotday_32_wb)]

df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        ### Wetbulb
        , hw_90_wb_2d := ifelse(consec_days_90_wb >= 2, 1, 0)][
        , hw_90_wb_3d := ifelse(consec_days_90_wb >= 3, 1, 0)][
        , hw_90_wb_5d := ifelse(consec_days_90_wb >= 5, 1, 0)][
        , hw_95_wb_2d := ifelse(consec_days_95_wb >= 2, 1, 0)][
        , hw_95_wb_3d := ifelse(consec_days_95_wb >= 3, 1, 0)][
        , hw_95_wb_5d := ifelse(consec_days_95_wb >= 5, 1, 0)][
        , hw_97_wb_2d := ifelse(consec_days_97_wb >= 2, 1, 0)][
        , hw_97_wb_3d := ifelse(consec_days_97_wb >= 3, 1, 0)][
        , hw_97_wb_5d := ifelse(consec_days_97_wb >= 5, 1, 0)][
        ## Based on absolute values
        ## Wetbulb
        , hw_30_wb_2d := ifelse(consec_days_30_wb >= 2, 1, 0)][
        , hw_30_wb_3d := ifelse(consec_days_30_wb >= 3, 1, 0)][
        , hw_30_wb_5d := ifelse(consec_days_30_wb >= 5, 1, 0)][
        , hw_31_wb_2d := ifelse(consec_days_31_wb >= 2, 1, 0)][
        , hw_31_wb_3d := ifelse(consec_days_31_wb >= 3, 1, 0)][
        , hw_31_wb_5d := ifelse(consec_days_31_wb >= 5, 1, 0)][
        , hw_32_wb_2d := ifelse(consec_days_32_wb >= 2, 1, 0)][
        , hw_32_wb_3d := ifelse(consec_days_32_wb >= 3, 1, 0)][
        , hw_32_wb_5d := ifelse(consec_days_32_wb >= 5, 1, 0)]

print("heatwave vars created")
print(Sys.time())

# Save dataset ---- 
write_fst(df_psu_temp_precip_paper, path = here(path_processed, "2.4-dhs-psu-paper.fst"))
print("All tasks complete for 2.4")