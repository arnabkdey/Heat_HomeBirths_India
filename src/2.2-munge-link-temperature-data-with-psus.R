# title: "Link the daily temp data with IR data"

####### This script reads the geocoded PSU data from DHS and extracts daily gridded climate data for the past n years for each PSU. The result is a HUGE dataset where each PSU has n\*365 rows of data, where n is the number of years of climate data available.

# Load Packages ----
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# Step-0: Download raw temperature datasets ----
## Create a local folder to download files
### Tmax
dir.create("./data/gdrive_temp_files_input/tmax_wb_raw", showWarnings = TRUE, recursive = TRUE)
dir.create("./data/gdrive_temp_files_input/precip_raw", showWarnings = TRUE, recursive = TRUE)

## List all files in the Google Drive folder
### Tmax - WB
folder_id_tmax_wb <- "1K0MrLG5RhFVnoDOE3F0amlz5CiyTuqQ_"
files_tmax_wb <- googledrive::drive_ls(as_id(folder_id_tmax_wb)) 

### Precipitation
folder_id_precip <- "1rZfAiTFb28bgWYuXOmf5LksLEIdhhW7T"
files_precip <- googledrive::drive_ls(as_id(folder_id_precip))

## Download the relevant file to the local folder
### Tmax - WB
for (i in seq_len(nrow(files_tmax_wb))) {
    cur_file <- files_tmax_wb[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = paste0("./data/gdrive_temp_files_input/tmax_wb_raw/", cur_file$name))}

### Precipitation
for (i in seq_len(nrow(files_precip))) {
    cur_file <- files_precip[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = paste0("./data/gdrive_temp_files_input/precip_raw/", cur_file$name))}

# Step-1: Load-datasets ---- 
rm(list = ls())
df_dhs_psu_geo_sf <- readRDS("./data/processed-data/2.1-a-df-dhs-psu-geo.rds")
india_boundary_buf <- readRDS("./data/processed-data/2.1-b-ind-boundary-0-buf.rds")
# head(df_dhs_psu_geo_sf)

# Step-2: load-function to extract climate data to DHS PSUs ----
source("./src/5.4-function-to-extract-climate-data-for-psus.R")

# Step-3: Run the function to extract climate data for each PSU ----
## Tmax - WB
df_psu_tmax_wb <- merge_dhs_climate(path = "./data/gdrive_temp_files_input/tmax_wb_raw/", clim_var = "max_temp_wb")
head(df_psu_tmax_wb)
write_fst(df_psu_tmax_wb, path = "./data/processed-data/2.2.1-df_psu_tmax_wb.fst")
rm(df_psu_tmax_wb)
print("finished Step-2: tmax-wb")

## Precipitation
df_psu_precip <- merge_dhs_climate(path = "./data/gdrive_temp_files_input/precip_raw/", clim_var = "mean_precip")
write_fst(df_psu_precip, path = "./data/processed-data/2.2.2-df_psu_precip.fst")
rm(df_psu_precip)
print("finished Step-2: precip")

# Step-4: Merge all the datasets ----
rm(list = ls())
df_psu_tmax_wb <- read_fst("./data/processed-data/2.2.1-df_psu_tmax_wb.fst", columns = c("psu", "date", "dist_name", "lat", "long", "max_temp"))
df_psu_precip <- read_fst("./data/processed-data/2.2.2-df_psu_precip.fst", columns = c("psu", "date", "mean_precip"))

df_list <- list(df_psu_tmax_wb, df_psu_precip)
df_psu_temp_precip <- reduce(df_list, left_join, by = c("psu", "date"))
print("finished Step-3: merge")

# Save your final work ---- 
write_fst(df_psu_temp_precip, path = "./data/processed-data/2.2-daily-temp-precip-1980-21-extracted-dhs-psu.fst")
print("finished Script 2.2")