# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script creates heatwave definitions based on wet-bulb and dry-bulb temperature percentiles
# @date: March 2025

# load-packages ---- 
print(Sys.time())
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, here)

# set path ----
source(here("paths_mac.R"))

# Read dataset ---- 
## First read the data
df_psu_temp_precip_paper <- read_fst(path = here(path_project, "data", "processed_data", 
        "1.2.2_clim_data_LT_trunc_2014_onwards.fst"), 
        as.data.table = T)
print("Data loaded")
print(Sys.time())

# Step-1: Create terciles ---- 
## Long-term Mean by PSU ----- 
### Get unique PSUs with both temperature variables
psu_temps <- unique(df_psu_temp_precip_paper[, .(psu, lt_mean_wbgt_max_psu, lt_mean_db_max_psu)])

### Create tertiles for WBGT
psu_temps[, lt_mean_wbgt_tert_psu := cut(lt_mean_wbgt_max_psu, 
                                        breaks = quantile(lt_mean_wbgt_max_psu, probs = seq(0, 1, 1/3)), 
                                        labels = 1:3, 
                                        include.lowest = TRUE)]

### Create tertiles for dry bulb temperature
psu_temps[, lt_mean_db_tert_psu := cut(lt_mean_db_max_psu, 
                                      breaks = quantile(lt_mean_db_max_psu, probs = seq(0, 1, 1/3)), 
                                      labels = 1:3, 
                                      include.lowest = TRUE)]

### Join back to main dataset
df_psu_temp_precip_paper <- merge(df_psu_temp_precip_paper, 
                                 psu_temps[, .(psu, lt_mean_wbgt_tert_psu, lt_mean_db_tert_psu)], 
                                 by = "psu")

print("PSU terciles created - Mean")
print(Sys.time())

## Long-term median by PSU -----
# Get unique PSUs with both median temperature variables
psu_medians <- unique(df_psu_temp_precip_paper[, .(psu, lt_median_wbgt_max_psu, lt_median_db_max_psu)])

# Create tertiles for WBGT medians
psu_medians[, lt_median_wbgt_tert_psu := cut(lt_median_wbgt_max_psu, 
                                           breaks = quantile(lt_median_wbgt_max_psu, probs = seq(0, 1, 1/3)), 
                                           labels = 1:3, 
                                           include.lowest = TRUE)]

# Create tertiles for dry bulb medians (assuming lt_median_db_max_psu is the variable name, not lt_median_db_tert_psu)
psu_medians[, lt_median_db_tert_psu := cut(lt_median_db_max_psu, 
                                         breaks = quantile(lt_median_db_max_psu, probs = seq(0, 1, 1/3)), 
                                         labels = 1:3, 
                                         include.lowest = TRUE)]

# Join back to main dataset
df_psu_temp_precip_paper <- merge(df_psu_temp_precip_paper, 
                                psu_medians[, .(psu, lt_median_wbgt_tert_psu, lt_median_db_tert_psu)], 
                                by = "psu")

print("PSU terciles created - Median")
print(Sys.time())

# Step-2: Create extreme heat variables based on n-tiles -----
## For wetbulb (wb)  -----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_wb_80 := ifelse(max_temp_wb >= cutoff_tmax_wb_80, 1, 0)][
        , hotday_wb_825 := ifelse(max_temp_wb >= cutoff_tmax_wb_825, 1, 0)][
        , hotday_wb_85 := ifelse(max_temp_wb >= cutoff_tmax_wb_85, 1, 0)][
        , hotday_wb_875 := ifelse(max_temp_wb >= cutoff_tmax_wb_875, 1, 0)][
        , hotday_wb_90 := ifelse(max_temp_wb >= cutoff_tmax_wb_90, 1, 0)][
        , hotday_wb_925 := ifelse(max_temp_wb >= cutoff_tmax_wb_925, 1, 0)][
        , hotday_wb_95 := ifelse(max_temp_wb >= cutoff_tmax_wb_95, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_wb_80 := ifelse(hotday_wb_80 == 1, 1:.N, 0L), by = rleid(hotday_wb_80)][
        , consec_days_wb_825 := ifelse(hotday_wb_825 == 1, 1:.N, 0L), by = rleid(hotday_wb_825)][
        , consec_days_wb_85 := ifelse(hotday_wb_85 == 1, 1:.N, 0L), by = rleid(hotday_wb_85)][
        , consec_days_wb_875 := ifelse(hotday_wb_875 == 1, 1:.N, 0L), by = rleid(hotday_wb_875)][
        , consec_days_wb_90 := ifelse(hotday_wb_90 == 1, 1:.N, 0L), by = rleid(hotday_wb_90)][
        , consec_days_wb_925 := ifelse(hotday_wb_925 == 1, 1:.N, 0L), by = rleid(hotday_wb_925)][
        , consec_days_wb_95 := ifelse(hotday_wb_95 == 1, 1:.N, 0L), by = rleid(hotday_wb_95)][
        ## Create heatwave vars for 2, 3, and 5 days
        ## 80th percentile
        , hw_wb_80_2d := ifelse(consec_days_wb_80 >= 2, 1, 0)][
        , hw_wb_80_3d := ifelse(consec_days_wb_80 >= 3, 1, 0)][
        , hw_wb_80_4d := ifelse(consec_days_wb_80 >= 4, 1, 0)][
        , hw_wb_80_5d := ifelse(consec_days_wb_80 >= 5, 1, 0)][
        ## 82.5th percentile
        , hw_wb_825_2d := ifelse(consec_days_wb_825 >= 2, 1, 0)][
        , hw_wb_825_3d := ifelse(consec_days_wb_825 >= 3, 1, 0)][
        , hw_wb_825_4d := ifelse(consec_days_wb_825 >= 4, 1, 0)][
        , hw_wb_825_5d := ifelse(consec_days_wb_825 >= 5, 1, 0)][
        ## 85th percentile
        , hw_wb_85_2d := ifelse(consec_days_wb_85 >= 2, 1, 0)][
        , hw_wb_85_3d := ifelse(consec_days_wb_85 >= 3, 1, 0)][
        , hw_wb_85_4d := ifelse(consec_days_wb_85 >= 4, 1, 0)][
        , hw_wb_85_5d := ifelse(consec_days_wb_85 >= 5, 1, 0)][
        ## 87.5th percentile
        , hw_wb_875_2d := ifelse(consec_days_wb_875 >= 2, 1, 0)][
        , hw_wb_875_3d := ifelse(consec_days_wb_875 >= 3, 1, 0)][
        , hw_wb_875_4d := ifelse(consec_days_wb_875 >= 4, 1, 0)][
        , hw_wb_875_5d := ifelse(consec_days_wb_875 >= 5, 1, 0)][
        ## 90th percentile
        , hw_wb_90_2d := ifelse(consec_days_wb_90 >= 2, 1, 0)][
        , hw_wb_90_3d := ifelse(consec_days_wb_90 >= 3, 1, 0)][
        , hw_wb_90_4d := ifelse(consec_days_wb_90 >= 4, 1, 0)][
        , hw_wb_90_5d := ifelse(consec_days_wb_90 >= 5, 1, 0)][
        ## 92.5th percentile
        , hw_wb_925_2d := ifelse(consec_days_wb_925 >= 2, 1, 0)][
        , hw_wb_925_3d := ifelse(consec_days_wb_925 >= 3, 1, 0)][
        , hw_wb_925_4d := ifelse(consec_days_wb_925 >= 4, 1, 0)][
        , hw_wb_925_5d := ifelse(consec_days_wb_925 >= 5, 1, 0)][
        ## 95th percentile
        , hw_wb_95_2d := ifelse(consec_days_wb_95 >= 2, 1, 0)][
        , hw_wb_95_3d := ifelse(consec_days_wb_95 >= 3, 1, 0)][
        , hw_wb_95_4d := ifelse(consec_days_wb_95 >= 4, 1, 0)][
        , hw_wb_95_5d := ifelse(consec_days_wb_95 >= 5, 1, 0)]

print("Perc-WBGT heatwave vars created")
print(Sys.time())

## For drybulb (db)  -----
df_psu_temp_precip_paper <- df_psu_temp_precip_paper[
        ## Identify extreme heat days
        , hotday_db_80 := ifelse(max_temp >= cutoff_tmax_db_80, 1, 0)][
        , hotday_db_825 := ifelse(max_temp >= cutoff_tmax_db_825, 1, 0)][
        , hotday_db_85 := ifelse(max_temp >= cutoff_tmax_db_85, 1, 0)][
        , hotday_db_875 := ifelse(max_temp >= cutoff_tmax_db_875, 1, 0)][
        , hotday_db_90 := ifelse(max_temp >= cutoff_tmax_db_90, 1, 0)][
        , hotday_db_925 := ifelse(max_temp >= cutoff_tmax_db_925, 1, 0)][
        , hotday_db_95 := ifelse(max_temp >= cutoff_tmax_db_95, 1, 0)][
        ## Identify consecutive days of extreme heat
        , consec_days_db_80 := ifelse(hotday_db_80 == 1, 1:.N, 0L), by = rleid(hotday_db_80)][
        , consec_days_db_825 := ifelse(hotday_db_825 == 1, 1:.N, 0L), by = rleid(hotday_db_825)][
        , consec_days_db_85 := ifelse(hotday_db_85 == 1, 1:.N, 0L), by = rleid(hotday_db_85)][
        , consec_days_db_875 := ifelse(hotday_db_875 == 1, 1:.N, 0L), by = rleid(hotday_db_875)][
        , consec_days_db_90 := ifelse(hotday_db_90 == 1, 1:.N, 0L), by = rleid(hotday_db_90)][
        , consec_days_db_925 := ifelse(hotday_db_925 == 1, 1:.N, 0L), by = rleid(hotday_db_925)][
        , consec_days_db_95 := ifelse(hotday_db_95 == 1, 1:.N, 0L), by = rleid(hotday_db_95)][
        ## Create heatwave vars for 2, 3, and 5 days
        ## 80th percentile
        , hw_db_80_2d := ifelse(consec_days_db_80 >= 2, 1, 0)][
        , hw_db_80_3d := ifelse(consec_days_db_80 >= 3, 1, 0)][
        , hw_db_80_4d := ifelse(consec_days_db_80 >= 4, 1, 0)][
        , hw_db_80_5d := ifelse(consec_days_db_80 >= 5, 1, 0)][
        ## 82.5th percentile
        , hw_db_825_2d := ifelse(consec_days_db_825 >= 2, 1, 0)][
        , hw_db_825_3d := ifelse(consec_days_db_825 >= 3, 1, 0)][
        , hw_db_825_4d := ifelse(consec_days_db_825 >= 4, 1, 0)][
        , hw_db_825_5d := ifelse(consec_days_db_825 >= 5, 1, 0)][
        ## 85th percentile
        , hw_db_85_2d := ifelse(consec_days_db_85 >= 2, 1, 0)][
        , hw_db_85_3d := ifelse(consec_days_db_85 >= 3, 1, 0)][
        , hw_db_85_4d := ifelse(consec_days_db_85 >= 4, 1, 0)][
        , hw_db_85_5d := ifelse(consec_days_db_85 >= 5, 1, 0)][
        ## 87.5th percentile
        , hw_db_875_2d := ifelse(consec_days_db_875 >= 2, 1, 0)][
        , hw_db_875_3d := ifelse(consec_days_db_875 >= 3, 1, 0)][
        , hw_db_875_4d := ifelse(consec_days_db_875 >= 4, 1, 0)][
        , hw_db_875_5d := ifelse(consec_days_db_875 >= 5, 1, 0)][
        ## 90th percentile
        , hw_db_90_2d := ifelse(consec_days_db_90 >= 2, 1, 0)][
        , hw_db_90_3d := ifelse(consec_days_db_90 >= 3, 1, 0)][
        , hw_db_90_4d := ifelse(consec_days_db_90 >= 4, 1, 0)][
        , hw_db_90_5d := ifelse(consec_days_db_90 >= 5, 1, 0)][
        ## 92.5th percentile
        , hw_db_925_2d := ifelse(consec_days_db_925 >= 2, 1, 0)][
        , hw_db_925_3d := ifelse(consec_days_db_925 >= 3, 1, 0)][
        , hw_db_925_4d := ifelse(consec_days_db_925 >= 4, 1, 0)][
        , hw_db_925_5d := ifelse(consec_days_db_925 >= 5, 1, 0)][
        ## 95th percentile
        , hw_db_95_2d := ifelse(consec_days_db_95 >= 2, 1, 0)][
        , hw_db_95_3d := ifelse(consec_days_db_95 >= 3, 1, 0)][
        , hw_db_95_4d := ifelse(consec_days_db_95 >= 4, 1, 0)][
        , hw_db_95_5d := ifelse(consec_days_db_95 >= 5, 1, 0)]

# Save dataset ---- 
df_psu_temp_precip_paper |> write_fst(
        path = here(path_project, "data", 
        "processed_data", "1.2.3_clim_data_vars.fst"))
nrow(df_psu_temp_precip_paper)
print("All tasks complete for 1.2.3")

# Summarize cutoffs by psu ----
df_cutoffs_summary <- df_psu_temp_precip_paper[, 
        lapply(.SD, function(x) x[1]), 
        by = psu, 
        .SDcols = patterns("^cutoff")]

# merge with psu lat/long ----
## read shape file for psu
df_dhs_geo_raw <- read_sf(here(path_dhs_india_shp, "IAGE7AFL.shp"))

## rename variables 
df_dhs_geo_raw <- df_dhs_geo_raw |> 
  dplyr::select(psu = DHSCLUST, 
    lat = LATNUM, long = LONGNUM) |>
  as.data.table() |>
  mutate(psu = as.factor(psu))

## merge datasets ----
df_merged <- merge(df_dhs_geo_raw, df_cutoffs_summary, by = "psu")

# save dataset ----
writexl::write_xlsx(df_merged, here(path_project, 
        "data", "processed_data", 
        "1.2.3_psu_cutoffs_summary_geo.xlsx"))
