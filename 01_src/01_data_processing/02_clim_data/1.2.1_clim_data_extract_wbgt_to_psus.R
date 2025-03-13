# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for each PSU.
# @date: March 2025

# load packages ----
rm(list = ls())
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths.R"))

# load-function to extract climate data to DHS PSUs ----
source(here("01_src", "01_data_processing",
  "utils", "function_to_extract_climate_data_for_psus.R"))

# step-1: load-datasets ----
df_dhs_psu_geo_sf <- readRDS(here(path_project, "data", 
  "processed_data", "1.1.3.a_df_dhs_psu_geo.rds"))
india_boundary_buf <- readRDS(here(path_project, "data", 
  "processed_data", "1.1.3.b_ind_boundary_0_buf.rds"))

# step-2: run the function to extract climate data for each PSU ----
## Tmax - WB
df_psu_tmax_wb <- merge_dhs_climate(path = here(path_wbgt_max_raw), 
  clim_var = "max_temp_wb")
write_fst(df_psu_tmax_wb, path = here(path_project, "data", 
  "processed_data", "1.2.1.a_df_psu_tmax_wb.fst"))
rm(df_psu_tmax_wb)

print("finished Step-2a: tmax-wb")

## Tmax - air temperature
df_psu_tmax <- merge_dhs_climate(path = here(path_tmax_raw), 
  clim_var = "max_temp")
write_fst(df_psu_tmax, path = here(path_project, "data", 
  "processed_data", "1.2.1.b_df_psu_tmax.fst"))
rm(df_psu_tmax)
print("finished Step-2b: tmax")

# step-3: merge all the datasets ----
rm(list = ls())
## read Tmax - WB
df_psu_tmax_wb <- read_fst(here(path_project, "data", 
  "processed_data", "1.2.1.a_df_psu_tmax_wb.fst"), 
  columns = c("psu", "date", "max_temp_wb"))

## read Tmax - dry bulb temperature
df_psu_tmax <- read_fst(here(path_project, "data", 
  "processed_data", "1.2.1.b_df_psu_tmax.fst"), 
  columns = c("psu", "date", "max_temp"))  

## merge the datasets
df_list <- list(df_psu_tmax_wb, df_psu_tmax)
df_psu_temp_merged <- reduce(df_list, left_join, by = c("psu", "date"))
df_psu_temp_merged <- df_psu_temp_merged |> mutate(psu = as.factor(psu))

## remove missing values using data.table
setDT(df_psu_temp_merged)
df_psu_temp_merged <- df_psu_temp_merged[!is.na(max_temp),]
sum(is.na(df_psu_temp_merged))

## arrange the data by psu and date
df_psu_temp_merged$psu <- as.factor(df_psu_temp_merged$psu)
df_psu_temp_merged <- df_psu_temp_merged[order(psu, date)]

print("finished Step-3: merge")

# save the merged file ---- 
df_psu_temp_merged |> write_fst(path = here(
  path_project, "data", "processed_data", 
  "1.2.1_daily_wbgt_tmax_1980_21_extracted_dhs_psu.fst"))
