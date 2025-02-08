# title: "Link the daily temp data with IR data"
# This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for the past n years for each PSU. 
# The result is a HUGE dataset where each PSU has n\*365 rows of data, where n is the number of years of climate data available.

# Load Packages --------------------------------------------------------------
pacman::p_load(tidyverse, janitor, data.table, fst, openxlsx, here)
rm(list = ls())

# set paths ------------------------------------------------------------------
source(here("paths-mac.R"))

# load-function to extract climate data to DHS PSUs ----
source(here("R", "6.1-function-to-extract-climate-data-for-psus.R"))

# step-1: load-datasets ------------------------------------------------------ 
df_dhs_psu_geo_sf <- readRDS(here(path_project, "data", "processed-data", "1.3-a-df-dhs-psu-geo.rds"))
india_boundary_buf <- readRDS(here(path_project, "data", "processed-data", "1.3-b-ind-boundary-0-buf.rds"))


# Step-2: Run the function to extract climate data for each PSU ----
## Tmax - WB
df_psu_tmax_wb <- merge_dhs_climate(path = here(path_wbgt_max_raw), clim_var = "max_temp_wb")
write_fst(df_psu_tmax_wb, path = here(path_project, "data", "processed-data", "1.4.1-df_psu_tmax_wb.fst"))
rm(df_psu_tmax_wb)

print("finished Step-2a: tmax-wb")

## Precipitation
df_psu_precip <- merge_dhs_climate(path = here(path_precip_raw), clim_var = "mean_precip")
write_fst(df_psu_precip, path = here(path_project, "data", "processed-data", "1.4.2-df_psu_precip.fst"))
rm(df_psu_precip)
print("finished Step-2b: precip")

## Tmax - air temperature
df_psu_tmax <- merge_dhs_climate(path = here(path_tmax_raw), clim_var = "max_temp")
write_fst(df_psu_tmax, path = here(path_project, "data", "processed-data", "1.4.3-df_psu_tmax.fst"))
rm(df_psu_tmax)
print("finished Step-2c: tmax")

# Step-4: Merge all the datasets ----
rm(list = ls())
## Read Tmax - WB
df_psu_tmax_wb <- read_fst(here(path_project, "data", "processed-data", "1.4.1-df_psu_tmax_wb.fst"), 
  columns = c("psu", "date", "max_temp_wb"))

## Read Precipitation
df_psu_precip <- read_fst(here(path_project, "data", "processed-data", "1.4.2-df_psu_precip.fst"), 
  columns = c("psu", "date", "mean_precip"))

## Read Tmax - air temperature
df_psu_tmax <- read_fst(here(path_project, "data", "processed-data", "1.4.3-df_psu_tmax.fst"), 
  columns = c("psu", "date", "max_temp"))  

## Merge the datasets
df_list <- list(df_psu_tmax_wb, df_psu_precip, df_psu_tmax)
df_psu_temp_precip <- reduce(df_list, left_join, by = c("psu", "date"))
df_psu_temp_precip <- df_psu_temp_precip |> mutate(psu = as.factor(psu))

## Remove missing values using data.table
setDT(df_psu_temp_precip)
df_psu_temp_precip <- df_psu_temp_precip[!is.na(mean_precip),]
df_psu_temp_precip <- df_psu_temp_precip[!is.na(max_temp),]
sum(is.na(df_psu_temp_precip))

## arrange the data by psu and date
df_psu_temp_precip$psu <- as.factor(df_psu_temp_precip$psu)
df_psu_temp_precip <- df_psu_temp_precip[order(psu, date)]

print("finished Step-3: merge")

# Save your final work ---- 
df_psu_temp_precip |> write_fst(path = here(path_project, "data", "processed-data", "1.4-daily-wbgt-precip-tmax-1980-21-extracted-dhs-psu.fst"))
print("finished Script 1.4")