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

# load-datasets ----
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.1.3.a_df_dhs_psu_geo.rds"))
india_boundary_buf <- readRDS(here(path_processed, "1.1.3.b_ind_boundary_0_buf.rds"))

# step-1: run the function to extract climate data for each PSU ----
## Tmax - WBGT
### Extract the data
df_psu_tmax_wbgt <- merge_dhs_climate(
  path = here(path_tmax_wbgt_raw),
  clim_var = "tmax_wbgt",
  from_index = 1, to_index = 42
)
### basic cleaning
setDT(df_psu_tmax_wbgt)
df_psu_tmax_wbgt <- df_psu_tmax_wbgt[
  , .(psu = as.factor(psu), date, tmax_wbgt)][
    order(psu, date)]

### save the data    
df_psu_tmax_wbgt |> write_fst(path = here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"))
rm(df_psu_tmax_wbgt)
print("finished Step-1a: tmax-wbgt")

## Tmax - DB air temperature (ERA5)
### Extract the data
df_psu_tmax_era5 <- merge_dhs_climate(
  path = here(path_tmax_era5),
  clim_var = "tmax_db_era5",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmax_era5)
df_psu_tmax_era5 <- df_psu_tmax_era5[
  , .(psu = as.factor(psu), date, tmax_era5 = tmax_era5 - 273.15)][
    order(psu, date)]

### save the data
df_psu_tmax_era5 |> write_fst(path = here(
  path_processed, 
  "1.2.1.b_df_psu_tmax_db_era5.fst"))
rm(df_psu_tmax_era5)
print("finished Step-1b: tmax-era5")

## Tmax - DB air temperature (NOAA)
### Extract the data
df_psu_tmax_db_noaa <- merge_dhs_climate(
  path = here(path_tmax_db_noaa),
  clim_var = "tmax_db_noaa",
  from_index = 1, to_index = 42
)

### basic cleaning
setDT(df_psu_tmax_db_noaa)
df_psu_tmax_db_noaa <- df_psu_tmax_db_noaa[
  , .(psu = as.factor(psu), date, tmax_db_noaa)][
    order(psu, date)]

### save the data
df_psu_tmax_db_noaa |> write_fst(path = here(path_processed, 
"1.2.1.c_df_psu_tmax_db_noaa.fst"))
rm(df_psu_tmax_db_noaa)
print("finished Step-1c: tmax db - noaa")


# step-2: merge the WBGT and ERA5 datasets ----
rm(list = ls())
source(here("paths.R"))

## read Tmax - WBGT
df_psu_tmax_wbgt <- read_fst(here(path_processed, "1.2.1.a_df_psu_tmax_wbgt.fst"), 
  columns = c("psu", "date", "tmax_wbgt"), 
  as.data.table = TRUE)

## read Tmax - dry bulb temperature (ERA5)
df_psu_tmax_era5 <- read_fst(here(path_processed, "1.2.1.b_df_psu_tmax_db_era5.fst"), 
  columns = c("psu", "date", "tmax_db_era5"),
  as.data.table = TRUE)  

## merge the datasets
df_psu_temp_merged <- merge(df_psu_tmax_wbgt, df_psu_tmax_era5, by = c("psu", "date"))

## remove missing values using data.table
setDT(df_psu_temp_merged)

## remove missing values using data.table
setDT(df_psu_temp_merged)
### print summary of missing values to be removed by psu
cat(paste("total missing values to be removed:", sum(is.na(df_psu_temp_merged))), "\n")
cat(paste("By PSU:", df_psu_temp_merged[ , .(missing = sum(is.na(tmax_wbgt))), by = psu]), "\n")

### remove missing values
df_psu_temp_merged <- df_psu_temp_merged[!is.na(tmax_wbgt),]
cat(paste("total missing values removed:", sum(is.na(df_psu_temp_merged))), "\n")

print("finished Step-3: merge")

# save the merged file ---- 
df_psu_temp_merged |> write_fst(path = here(
  path_processed,
  "1.2.1_wbgt_era5_extracted_to_psus.fst"))