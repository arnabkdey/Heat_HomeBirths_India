library(googledrive)
library(here)
library(rdhs)
library(httr)
library(zen4R)

# Script 1.1: Download DHS files from Google Drive to local files ----
## Create a local folder to download files
path_dhs_india_IR <- here("2-data", "2.1-raw-data", "dhs_india_IR")
dir.create(path_dhs_india_IR, showWarnings = TRUE, recursive = TRUE)

## List all files in the Google Drive folder
folder_id_dhs_india_IR <- "1hBVjceKjs-TIskYC9ehR-8v3roW6rHjj"
files_dhs_india_IR <- googledrive::drive_ls(as_id(folder_id_dhs_india_IR))
file_id_dta <- files_dhs_india_IR %>% dplyr::filter(str_detect(name, ".DTA")) %>% dplyr::pull(id)
file_name_dta <- files_dhs_india_IR %>% dplyr::filter(str_detect(name, ".DTA")) %>% dplyr::pull(name)

## Download the relevant file to the local folder
drive_download(as_id(file_id_dta), path = here(path_dhs_india_IR, file_name_dta))

# Script 1.2: Download the Beck_KG climate zone files from Google Drive to local files ----
## Create a local folder to download files
here_beck_kg_files <- here("2-data", "2.1-raw-data", "Beck_KG_files")
dir.create(here_beck_kg_files, showWarnings = TRUE, recursive = TRUE)

## List all files in the Google Drive folder
folder_id_clim_zones <- "1hTtzU4LtzE4mHBKcaOuZlgeFmDSgewe5"
files_clim_zone <- googledrive::drive_ls(as_id(folder_id_clim_zones)) |> filter(str_detect(name, "Beck_KG_V1_present_0p0083"))

## Download the relevant file to the local folder
drive_download(as_id(files_clim_zone$id), path = here(here_beck_kg_files, files_clim_zone$name))

# Script 2.1: Download India shape files from Google Drive to local files ----
## List all files in the Google Drive folder
folder_id_dhs_shp_india <- "1gn1gVQLq98VFxp5IKWant_U9W1GDibJt"
files_shp_india <- googledrive::drive_ls(as_id(folder_id_dhs_shp_india)) |> filter(str_detect(name, "IAGE7AFL") & !str_detect(name, "Zone"))

## Create a local folder to download files
here_dhs_shp_files <- here("2-data", "2.1-raw-data", "dhs_shp_files_india")
dir.create(here_dhs_shp_files, showWarnings = TRUE, recursive = TRUE)

## Download all files to the local folder
for (i in seq_len(nrow(files_shp_india))) {
    cur_file <- files_shp_india[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = here(here_dhs_shp_files, cur_file$name))
}

# Script-2.2: Download raw climate datasets ----
## WBGT-max 
### List all files in the Google drive folder
folder_id_wbgt_max <- "1K0MrLG5RhFVnoDOE3F0amlz5CiyTuqQ_"
files_wbgt_max <- googledrive::drive_ls(as_id(folder_id_wbgt_max))

### Create a local folder to download files
here_wbgt_max_raw <- here("2-data", "2.1-raw-data", "wbgt_max_raw")
dir.create(here_wbgt_max_raw, showWarnings = TRUE, recursive = TRUE)

### Download the relevant files to the local folder
for (i in seq_len(nrow(files_wbgt_max))) {
    cur_file <- files_wbgt_max[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = here(here_wbgt_max_raw, cur_file$name))
}

## Precipitation - CHIRPS
### Create a local folder to download files
here_precip_raw <- here("2-data", "2.1-raw-data", "precip_raw")
dir.create(here_precip_raw, showWarnings = TRUE, recursive = TRUE)

### Download the relevant files to the local folder
year = c(1981:2021)
## This downloads the file in the root directory of the project
for (y in year){
  download.file(paste("https://2-data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/chirps-v2.0."
                ,y,".days_p05.nc",sep=""),
                destfile= here(here_precip_raw, paste("precip_",y,".nc",sep="")), method="curl")}

