# This code takes about 15 minutes to run on a 32 core PC.

# load-packages ---- 
print(Sys.time())
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, here)

# set path ----
source(here("paths-mac.R"))

# Read dataset ---- 
## First read the date column
df_psu_temp_precip_paper <- read_fst(path = here(path_project, "processed-data", "1.5-daily-clim-vars-long-term.fst"), 
                                as.data.table = T)


## Filter cases to 2014 onwards
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[base::`>=`(df_psu_temp_precip_paper$date, as.Date("2014-01-01")), ]

# Step-1: Create terciles ---- 
## Long-term Mean ----- 
df_psu_temp_precip_paper[, lt_tmax_mean_cat_tert_wb := frank(lt_mean_max_temp_wb, ties.method = "min") * 3 / .N, by = day_of_year]
df_psu_temp_precip_paper[, lt_tmax_mean_cat_tert_wb := ceiling(lt_tmax_mean_cat_tert_wb)]
df_psu_temp_precip_paper$lt_tmax_mean_cat_tert_wb <- as.factor(df_psu_temp_precip_paper$lt_tmax_mean_cat_tert_wb)

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
## For wetbulb (wb)  --------------------------------
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_wb_90 := ifelse(max_temp_wb >= cutoff_tmax_wb_90, 1, 0)][
        , hotday_wb_95 := ifelse(max_temp_wb >= cutoff_tmax_wb_95, 1, 0)][
        , hotday_wb_97 := ifelse(max_temp_wb >= cutoff_tmax_wb_97, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_wb_90 := ifelse(hotday_wb_90 == 1, 1:.N, 0L), by = rleid(hotday_wb_90)][
        , consec_days_wb_95 := ifelse(hotday_wb_95 == 1, 1:.N, 0L), by = rleid(hotday_wb_95)][
        , consec_days_wb_97 := ifelse(hotday_wb_97 == 1, 1:.N, 0L), by = rleid(hotday_wb_97)][
        ## Create heatwave vars for 2, 3, and 5 days
        ## 90th percentile
        , hw_wb_90_2d := ifelse(consec_days_wb_90 >= 2, 1, 0)][
        , hw_wb_90_3d := ifelse(consec_days_wb_90 >= 3, 1, 0)][
        , hw_wb_90_5d := ifelse(consec_days_wb_90 >= 5, 1, 0)][
        ## 95th percentile
        , hw_wb_95_2d := ifelse(consec_days_wb_95 >= 2, 1, 0)][
        , hw_wb_95_3d := ifelse(consec_days_wb_95 >= 3, 1, 0)][
        , hw_wb_95_5d := ifelse(consec_days_wb_95 >= 5, 1, 0)][
        ## 97th percentile
        , hw_wb_97_2d := ifelse(consec_days_wb_97 >= 2, 1, 0)][
        , hw_wb_97_3d := ifelse(consec_days_wb_97 >= 3, 1, 0)][
        , hw_wb_97_5d := ifelse(consec_days_wb_97 >= 5, 1, 0)]

print("WBGT heatwave vars created")
print(Sys.time())

## For drybulb (db)  --------------------------------
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_db_90 := ifelse(max_temp >= cutoff_tmax_db_90, 1, 0)][
        , hotday_db_95 := ifelse(max_temp >= cutoff_tmax_db_95, 1, 0)][
        , hotday_db_97 := ifelse(max_temp >= cutoff_tmax_db_97, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_db_90 := ifelse(hotday_db_90 == 1, 1:.N, 0L), by = rleid(hotday_db_90)][
        , consec_days_db_95 := ifelse(hotday_db_95 == 1, 1:.N, 0L), by = rleid(hotday_db_95)][
        , consec_days_db_97 := ifelse(hotday_db_97 == 1, 1:.N, 0L), by = rleid(hotday_db_97)][
        ## Create heatwave vars for 2, 3, and 5 days
        ## 90th percentile
        , hw_db_90_2d := ifelse(consec_days_db_90 >= 2, 1, 0)][
        , hw_db_90_3d := ifelse(consec_days_db_90 >= 3, 1, 0)][
        , hw_db_90_5d := ifelse(consec_days_db_90 >= 5, 1, 0)][
        ## 95th percentile
        , hw_db_95_2d := ifelse(consec_days_db_95 >= 2, 1, 0)][
        , hw_db_95_3d := ifelse(consec_days_db_95 >= 3, 1, 0)][
        , hw_db_95_5d := ifelse(consec_days_db_95 >= 5, 1, 0)][
        ## 97th percentile
        , hw_db_97_2d := ifelse(consec_days_db_97 >= 2, 1, 0)][
        , hw_db_97_3d := ifelse(consec_days_db_97 >= 3, 1, 0)][
        , hw_db_97_5d := ifelse(consec_days_db_97 >= 5, 1, 0)]

# Save dataset ---- 
df_psu_temp_precip_paper |> write_fst(path = here(path_project, "data", "processed-data", "1.6-dhs-psu-paper-tmax-added.fst"))
print("All tasks complete for 1.6")

# toolbox
df_psu_temp_precip_paper <-  read_fst(path = here(path_project, "data", "processed-data", "1.6-dhs-psu-paper-tmax-added.fst"), from = 980000, to = 985000)
head(tabyl(df_psu_temp_precip_paper, psu) |> filter(percent != 0))
df_test <- df_psu_temp_precip_paper |> filter(psu == "901")
summary(df_test |> select(contains("hotday")))
range(df_test$date)

View(df_test |> select(psu, date, max_temp_wb, contains("wb")))

tabyl(df_test$hotday_wb_90)
