# title: "Read spatial datasets"

# Load Packageslibrary(tidyverse)
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# Step-0: Download India shape files from Google Drive to local files ----
## List all files in the Google Drive folder
folder_id_dhs_shp_india <- "1gn1gVQLq98VFxp5IKWant_U9W1GDibJt"
files_shp_india <- googledrive::drive_ls(as_id(folder_id_dhs_shp_india)) |> filter(str_detect(name, "IAGE7AFL"))

## Create a local folder to download files
if (!dir.exists("./data/gdrive_temp_files_input/dhs_shp_files_india/")) {
  # Create the directory if it does not exist
  dir.create("./data/gdrive_temp_files_input/dhs_shp_files_india/", showWarnings = TRUE, recursive = TRUE)
}

## Download all files to the local folder
for (i in seq_len(nrow(files_shp_india))) {
    cur_file <- files_shp_india[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = paste0("./data/gdrive_temp_files_input/dhs_shp_files_india/", cur_file$name))
}

# Read Geo Coded Datasets
## Step-1: Read geo-coded PSU data from DHS
### Load India shape file
df_dhs_geo_raw <- read_sf("./data/gdrive_temp_files_input/dhs_shp_files_india/IAGE7AFL.shp")

### Select relevant variables and filter out absurd geocodes
df_dhs_psu_geo <- df_dhs_geo_raw %>% 
  dplyr::select(psu = DHSCLUST, dist_name = DHSREGNA,
                lat = LATNUM, long = LONGNUM) %>% 
  dplyr::filter(!is.na(lat)) %>% 
  dplyr::filter(!is.na(long)) %>% 
  dplyr::filter(lat!=0 | long!=0) %>%                  #LAT=0 and LONG=0 are missing coordinates  
  dplyr::filter(lat <= -0.00005 | long >= 0.00005)      #missing obs. - remove


## Step-2: Get India Administrative Boundaries
### Load adm-1 for India
if (!dir.exists("./data/geo-spatial-data/")) {
  # Create the directory if it does not exist
  dir.create("./data/geo-spatial-data/", showWarnings = TRUE, recursive = TRUE)
}

india_boundary <- ne_countries(scale = "medium", returnclass = "sf") %>% 
              filter(admin == "India")
# plot(st_geometry(india_boundary))

#### Add Buffer
india_boundary_buf <- st_buffer(india_boundary, dist = 50000)
# plot(st_geometry(india_boundary_buf))

# Save output ----
## Check if the directory exists and create if not
if (!dir.exists("./data/processed-data/")) {
  # Create the directory if it does not exist
  dir.create("./data/processed-data/", showWarnings = TRUE, recursive = TRUE)
}

## Save the file
saveRDS(df_dhs_psu_geo, file = "./data/processed-data/2.1-a-df-dhs-psu-geo.rds")
saveRDS(india_boundary_buf, file = "./data/processed-data/2.1-b-ind-boundary-0-buf.rds")

