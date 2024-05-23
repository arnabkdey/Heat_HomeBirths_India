# title: "Read spatial datasets"

# Load Packageslibrary(tidyverse)
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# Read Geo Coded Datasets
## Step-1: Read geo-coded PSU data from DHS
### Load India shape file
df_dhs_geo_raw <- read_sf(here("2-data", "2.1-raw-data", "dhs_shp_files_india", "IAGE7AFL.shp"))

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
if (!dir.exists(here("2-data", "geo-spatial-data"))) {
  # Create the directory if it does not exist
  dir.create(here("2-data", "geo-spatial-data"), showWarnings = TRUE, recursive = TRUE)
}

india_boundary <- ne_countries(scale = "medium", returnclass = "sf") %>% 
              filter(admin == "India")
# plot(st_geometry(india_boundary))

#### Add Buffer
india_boundary_buf <- st_buffer(india_boundary, dist = 50000)
# plot(st_geometry(india_boundary_buf))

# Save output ----
## Check if the directory exists and create if not
path_processed_data <- here("2-data", "2.2-processed-data")
if (!dir.exists(path_processed_data)) {
  # Create the directory if it does not exist
  dir.create(path_processed_data, showWarnings = TRUE, recursive = TRUE)
}
## Save the file
saveRDS(df_dhs_psu_geo, file = here(path_processed_data, "1.2-a-df-dhs-psu-geo.rds"))
saveRDS(india_boundary_buf, file = here(path_processed_data, "1.2-b-ind-boundary-0-buf.rds"))
