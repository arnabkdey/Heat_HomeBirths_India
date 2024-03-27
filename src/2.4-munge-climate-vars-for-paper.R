# load-packages ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed)

# Read dataset ---- 
rm(list = ls())
## First read the date column
df_psu_temp_precip_paper <- read_fst("./data/processed-data/2.3-daily-clim-vars-long-term-by-day.fst", 
                                as.data.table = TRUE)
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
        , hotday_85_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_85, 1, 0)][
        , hotday_90_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_90, 1, 0)][
        , hotday_95_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_95, 1, 0)][
        , hotday_97_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_97, 1, 0)][
        , hotday_99_wb := ifelse(max_temp_wb >= cutoff_tmax_wb_99, 1, 0)][
        ## Based on absoulte values
        ### Wetbulb
        , hotday_26_wb := ifelse(max_temp_wb >= 26, 1, 0)][
        , hotday_28_wb := ifelse(max_temp_wb >= 28, 1, 0)][
        , hotday_30_wb := ifelse(max_temp_wb >= 30, 1, 0)][
        , hotday_32_wb := ifelse(max_temp_wb >= 32, 1, 0)]

print("extreme heat days created")

# Step-3: Create rainy days ----
## First convert mm to cm ----- 
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[, `:=` (
        mean_precip_cm = mean_precip / 10,
        lt_mean_precip_cm = lt_mean_precip/10)][
    # Center the mean precip variable
    , mean_precip_center := scale(mean_precip_cm, center = TRUE)]

## Then create rainy day vars ----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        , rainyday_85 := ifelse(mean_precip >= cutoff_mean_precip_85, 1, 0)][
        , rainyday_90 := ifelse(mean_precip >= cutoff_mean_precip_90, 1, 0)][
        , rainyday_95 := ifelse(mean_precip >= cutoff_mean_precip_95, 1, 0)][
        ## Based on absoulte values
        , rain_any := ifelse(mean_precip_cm > 0, 1, 0)][
        , rain_1cm := ifelse(mean_precip_cm >= 1, 1, 0)][
        , rain_2cm := ifelse(mean_precip_cm >= 2, 1, 0)][
        , rain_3cm := ifelse(mean_precip_cm >= 3, 1, 0)][
        , rain_5cm := ifelse(mean_precip_cm >= 5, 1, 0)]

print("rainyday based on ntile and abs precip created for current day")

# Step-4: Create Heatwave vars for 2,3, and 5 days -----
## First create consecutive days of extreme temperature ----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        ### Wetbulb
        , consec_days_85_wb := ifelse(hotday_85_wb == 1, 1:.N, 0L), by = rleid(hotday_85_wb)][
        , consec_days_90_wb := ifelse(hotday_90_wb == 1, 1:.N, 0L), by = rleid(hotday_90_wb)][
        , consec_days_95_wb := ifelse(hotday_95_wb == 1, 1:.N, 0L), by = rleid(hotday_95_wb)][
        , consec_days_97_wb := ifelse(hotday_97_wb == 1, 1:.N, 0L), by = rleid(hotday_97_wb)][
        , consec_days_99_wb := ifelse(hotday_99_wb == 1, 1:.N, 0L), by = rleid(hotday_99_wb)][
        ## Based on absoulte values
        ### Wetbulb
        , consec_days_26_wb := ifelse(hotday_26_wb == 1, 1:.N, 0L), by = rleid(hotday_26_wb)][
        , consec_days_28_wb := ifelse(hotday_28_wb == 1, 1:.N, 0L), by = rleid(hotday_28_wb)][
        , consec_days_30_wb := ifelse(hotday_30_wb == 1, 1:.N, 0L), by = rleid(hotday_30_wb)][
        , consec_days_32_wb := ifelse(hotday_32_wb == 1, 1:.N, 0L), by = rleid(hotday_32_wb)]

df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Based on n-tiles
        ### Wetbulb
        , hw_85_wb_2d := ifelse(consec_days_85_wb >= 2, 1, 0)][
        , hw_85_wb_3d := ifelse(consec_days_85_wb >= 3, 1, 0)][
        , hw_85_wb_5d := ifelse(consec_days_85_wb >= 5, 1, 0)][
        , hw_90_wb_2d := ifelse(consec_days_90_wb >= 2, 1, 0)][
        , hw_90_wb_3d := ifelse(consec_days_90_wb >= 3, 1, 0)][
        , hw_90_wb_5d := ifelse(consec_days_90_wb >= 5, 1, 0)][
        , hw_95_wb_2d := ifelse(consec_days_95_wb >= 2, 1, 0)][
        , hw_95_wb_3d := ifelse(consec_days_95_wb >= 3, 1, 0)][
        , hw_95_wb_5d := ifelse(consec_days_95_wb >= 5, 1, 0)][
        , hw_97_wb_2d := ifelse(consec_days_97_wb >= 2, 1, 0)][
        , hw_97_wb_3d := ifelse(consec_days_97_wb >= 3, 1, 0)][
        , hw_97_wb_5d := ifelse(consec_days_97_wb >= 5, 1, 0)][
        , hw_99_wb_2d := ifelse(consec_days_99_wb >= 2, 1, 0)][
        , hw_99_wb_3d := ifelse(consec_days_99_wb >= 3, 1, 0)][
        , hw_99_wb_5d := ifelse(consec_days_99_wb >= 5, 1, 0)][
        ## Based on absolute values
        ## Wetbulb
        , hw_26_wb_2d := ifelse(consec_days_26_wb >= 2, 1, 0)][
        , hw_26_wb_3d := ifelse(consec_days_26_wb >= 3, 1, 0)][
        , hw_26_wb_5d := ifelse(consec_days_26_wb >= 5, 1, 0)][
        , hw_28_wb_2d := ifelse(consec_days_28_wb >= 2, 1, 0)][
        , hw_28_wb_3d := ifelse(consec_days_28_wb >= 3, 1, 0)][
        , hw_28_wb_5d := ifelse(consec_days_28_wb >= 5, 1, 0)][
        , hw_30_wb_2d := ifelse(consec_days_30_wb >= 2, 1, 0)][
        , hw_30_wb_3d := ifelse(consec_days_30_wb >= 3, 1, 0)][
        , hw_30_wb_5d := ifelse(consec_days_30_wb >= 5, 1, 0)][
        , hw_32_wb_2d := ifelse(consec_days_32_wb >= 2, 1, 0)][
        , hw_32_wb_3d := ifelse(consec_days_32_wb >= 3, 1, 0)][
        , hw_32_wb_5d := ifelse(consec_days_32_wb >= 5, 1, 0)]

print("heatwave vars created")
print(Sys.time())

# Save dataset ---- 
write_fst(df_psu_temp_precip_paper, path = "./data/processed-data/2.4-dhs-psu-paper.fst")
print("All tasks complete for 2.3")