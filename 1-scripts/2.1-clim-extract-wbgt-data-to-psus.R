# title: "Link the daily temp data with IR data"
# This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for the past n years for each PSU. 
# The result is a HUGE dataset where each PSU has n\*365 rows of data, where n is the number of years of climate data available.

# Load Packages ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
rm(list = ls())

# Step-0: Point to folders that contain the raw WBGT and precip data ----
here_tmax_wb_raw <- here("2-data", "2.1-raw-data", "wbgt_max_raw")
here_precip <- here("2-data", "2.1-raw-data", "precip_raw")
path_processed <- here("2-data", "2.2-processed-data")

# Step-1: Load-datasets ---- 
df_dhs_psu_geo_sf <- readRDS(here(path_processed, "1.2-a-df-dhs-psu-geo.rds"))
india_boundary_buf <- readRDS(here(path_processed, "1.2-b-ind-boundary-0-buf.rds"))
# head(df_dhs_psu_geo_sf)

# Step-2: load-function to extract climate data to DHS PSUs ----
source(here("1-scripts", "5.4-function-to-extract-climate-data-for-psus.R"))

# Step-3: Run the function to extract climate data for each PSU ----
## Tmax - WB
df_psu_tmax_wb <- merge_dhs_climate(path = here_tmax_wb_raw, clim_var = "max_temp_wb")
write_fst(df_psu_tmax_wb, path = here(path_processed, "2.1.1-df_psu_tmax_wb.fst"))
rm(df_psu_tmax_wb)

print("finished Step-2: tmax-wb")

## Precipitation
df_psu_precip <- merge_dhs_climate(path = here_precip, clim_var = "mean_precip")
write_fst(df_psu_precip, path = here(path_processed, "2.1.2-df_psu_precip.fst"))
rm(df_psu_precip)
print("finished Step-2: precip")

# Step-4: Merge all the datasets ----
rm(list = ls())
## Read Tmax - WB
df_psu_tmax_wb <- read_fst(here(path_processed, "2.1.1-df_psu_tmax_wb.fst"), columns = c("psu", "date", "dist_name", "lat", "long", "max_temp_wb"))
min(df_psu_tmax_wb$date)
max(df_psu_tmax_wb$date)
## Read Precipitation
df_psu_precip <- read_fst(here(path_processed, "2.1.2-df_psu_precip.fst"), columns = c("psu", "date", "mean_precip"))
min(df_psu_precip$date)
max(df_psu_precip$date)

## Merge the datasets
df_list <- list(df_psu_tmax_wb, df_psu_precip)
df_psu_temp_precip <- reduce(df_list, left_join, by = c("psu", "date"))
min(df_psu_temp_precip$date)
max(df_psu_temp_precip$date)
head(df_psu_temp_precip)

## Remove missing precipitation values using data.table
setDT(df_psu_temp_precip)
df_psu_temp_precip <- df_psu_temp_precip[!is.na(mean_precip),]
min(df_psu_temp_precip$date)
max(df_psu_temp_precip$date)
head(df_psu_temp_precip)
sum(is.na(df_psu_temp_precip$mean_precip))
print("finished Step-3: merge")

# Save your final work ---- 
write_fst(df_psu_temp_precip, path = here(path_processed, "2.1-daily-temp-precip-1980-21-extracted-dhs-psu.fst"))
print("finished Script 2.1")