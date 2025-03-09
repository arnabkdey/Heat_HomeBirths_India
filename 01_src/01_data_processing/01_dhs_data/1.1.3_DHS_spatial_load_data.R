# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script loads and processes geo-coded PSU data from the DHS-India shape file
# @date: March 2025

# load packages ----
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)

# set paths ----
source(here("paths_mac.R"))

# load data ----
## read geo-coded PSU data from DHS
### Load India shape file
df_dhs_geo_raw <- read_sf(here(path_dhs_india_shp, "IAGE7AFL.shp"))

### Rename variables 
df_dhs_geo_raw <- df_dhs_geo_raw |> 
  dplyr::select(psu = DHSCLUST, dist_name = DHSREGNA,
                lat = LATNUM, long = LONGNUM)
nrow(df_dhs_geo_raw)

### Filter missing values
df_dhs_psu_geo <- df_dhs_geo_raw |>
  dplyr::filter(!is.na(lat)) |> 
  dplyr::filter(!is.na(long)) |> 
  dplyr::filter(lat!=0 | long!=0) |>                  #LAT=0 and LONG=0 are missing coordinates  
  dplyr::filter(lat <= -0.00005 | long >= 0.00005)      #missing obs. - remove

nrow(df_dhs_psu_geo) #118 psus dropped

## Get India Administrative Boundaries
india_boundary <- ne_countries(scale = "medium", returnclass = "sf") |> 
              filter(admin == "India")

### Add Buffer
india_boundary_buf <- st_buffer(india_boundary, dist = 50000)

# save outputs ----
saveRDS(df_dhs_psu_geo, file = here(path_project, "data", "processed_data", 
  "1.1.3.a_df_dhs_psu_geo.rds"))
saveRDS(india_boundary_buf, file = here(path_project, "data", "processed_data", 
  "1.1.3.b_ind_boundary_0_buf.rds"))
