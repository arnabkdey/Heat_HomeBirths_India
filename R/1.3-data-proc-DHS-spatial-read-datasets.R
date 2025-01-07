# title: "Read spatial datasets"

# load packages --------------------------------------------------------------
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# set paths ------------------------------------------------------------------
source(here("paths-mac.R"))

# Read Geo Coded Datasets ---------------------------------------------------
## Step-1: Read geo-coded PSU data from DHS
### Load India shape file
df_dhs_geo_raw <- read_sf(here(path_dhs_india_shp, "IAGE7AFL.shp"))

### Rename variables 
df_dhs_geo_raw <- df_dhs_geo_raw %>% 
  dplyr::select(psu = DHSCLUST, dist_name = DHSREGNA,
                lat = LATNUM, long = LONGNUM)
nrow(df_dhs_geo_raw)

### Filter missing values
df_dhs_psu_geo <- df_dhs_geo_raw %>%
  dplyr::filter(!is.na(lat)) %>% 
  dplyr::filter(!is.na(long)) %>% 
  dplyr::filter(lat!=0 | long!=0) %>%                  #LAT=0 and LONG=0 are missing coordinates  
  dplyr::filter(lat <= -0.00005 | long >= 0.00005)      #missing obs. - remove

nrow(df_dhs_psu_geo) #118 psus dropped

## Step-2: Get India Administrative Boundaries
india_boundary <- ne_countries(scale = "medium", returnclass = "sf") %>% 
              filter(admin == "India")
# plot(st_geometry(india_boundary))

#### Add Buffer
india_boundary_buf <- st_buffer(india_boundary, dist = 50000)
# plot(st_geometry(india_boundary_buf))

# Save output ----
saveRDS(df_dhs_psu_geo, file = here(path_project, "data", "processed-data", "1.3-a-df-dhs-psu-geo.rds"))
saveRDS(india_boundary_buf, file = here(path_project, "data", "processed-data", "1.3-b-ind-boundary-0-buf.rds"))
