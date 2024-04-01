# Title: "Linking climate regions in india with districts"

# Data source (Beck et al)
#https://www.nature.com/articles/sdata2018214/tables/3
#https://www.nature.com/articles/sdata2018214#data-records

# Load Libraries ---- 
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
pacman::p_load(sf, sp, raster, terra, tidyterra, ncdf4, rnaturalearth)
library(smoothr)
library(tiff)
library(rasterVis)
library(tmap)
library(rdhs)

# Step-0: Download the Beck_KG climate zone files from Google Drive to local files ----
## Create a local folder to download files

if (!dir.exists(here("data", "gdrive_temp_files_input", "Beck_KG_files"))) {
  # Create the directory if it does not exist
  dir.create(here("data", "gdrive_temp_files_input", "Beck_KG_files"), showWarnings = TRUE, recursive = TRUE)
}

## List all files in the Google Drive folder
folder_id_clim_zones <- "1hTtzU4LtzE4mHBKcaOuZlgeFmDSgewe5"
files_clim_zone <- googledrive::drive_ls(as_id(folder_id_clim_zones)) |> filter(str_detect(name, "Beck_KG_V1_present_0p0083"))

## Download the relevant file to the local folder
drive_download(as_id(files_clim_zone$id), 
  path = paste0(here("data", "gdrive_temp_files_input", "Beck_KG_files"), files_clim_zone$name))

# Step-1: Read and rasterize the downloaded file and extract the climate zones ----
file <- here("data", "gdrive_temp_files_input", "Beck_KG_files", "Beck_KG_V1_present_0p0083.tif")
KG.rd <- terra::rast(file)
plot(KG.rd)


# Step-2: Retrieve the spatial boundaries for India (DHS spatial repository)
## Create a df with districts and climate zones
#https://spatialdata.dhsprogram.com/boundaries/
bord <- download_boundaries(surveyId = "IA2020DHS", method = "sf")
class(bord)
### Unlist the list
list2env(bord ,.GlobalEnv)

## Check the data
### Check plots
# plot(sdr_subnational_boundaries[1]) # State boundaries
# plot(sdr_subnational_boundaries2[1]) # District boundaries

### Compare the CRS projections
crs_KG <- terra::crs(KG.rd)
crs_dhs <- terra::crs(sdr_subnational_boundaries2)


# Step-3: Crop the KG climate map to the state/district boundary
## NOTE: use " sdr_subnational_boundaries" everywhere if you want to generate data for states

## Check the class of the two objects
class(KG.rd)
# class(sdr_subnational_boundaries2)

sdr_raster <- rasterize(sdr_subnational_boundaries2, KG.rd)
# plot(sdr_raster)

# Mask KG.rd with sdr_raster using a logical approach 
sdr_logical <- sdr_raster > 0
masked_KG <- KG.rd * sdr_logical
# plot(masked_KG)

# Step-4: Extract the gridcell values from the spatialbrick
extracted_values <- extract(x = masked_KG, y = sdr_subnational_boundaries2, method='simple')
# class(extracted_values)
# dim(extracted_values)
# length(unique(extracted_values$ID))
# head(sdr_subnational_boundaries2)
# class(sdr_subnational_boundaries2)
# length(unique(sdr_subnational_boundaries2$REGCODE))

# Step-5: Merge the two datasets
## Pick up from here
df_zones <- sdr_subnational_boundaries2 |> 
                dplyr::select(state.name = OTHREGNA, 
                              district.name = REGNAME, 
                              district.code = REGCODE) 
                

rd0 <- raster::extract(x = cd2, y = sdr_subnational_boundaries2, method='simple')
# summary(rd0)
# class(rd0)
# head(rd0)
# length(rd0)
# nrow(sdr_subnational_boundaries2)

rd1 <- mapply(cbind, extracted_values, state.name=sdr_subnational_boundaries2$OTHREGNA, district.name = sdr_subnational_boundaries2$REGNAME, district.code = sdr_subnational_boundaries2$REGCODE)


# Step-5: Convert List to wide data frame, making all colums (except CNTRY_NAME, REG and DIST) numeric
df0 <- rd1 %>% 
  lapply(as.data.frame, stringsAsFactors = FALSE) %>% 
  bind_rows() %>% 
  tbl_df()  %>%
  dplyr::mutate_each(funs(as.numeric), -state.name, -district.name, -district.code) %>% 
  drop_na()
# View(df0)

# Step-6: Aggregate the grid data to district level data by taking the most common value to define climatic zone
df_zones <- ddply(df0, .(state.name, district.name, district.code),summarize,V1={
  tt <- table(V1)
  names(tt)[which.max(tt)]
})

# Step-7: Assign labels to climate zones -- Detailed
sort(unique(as.numeric(df_zones$V1)))  
df_zones$clim_zone[df_zones$V1 == 1] <- "Af: Tropical, rainforest"
df_zones$clim_zone[df_zones$V1 == 2] <- "Am: Tropical, monsoon "
df_zones$clim_zone[df_zones$V1 == 3] <- "Aw: Tropical, savannah"
df_zones$clim_zone[df_zones$V1 == 4] <- "BWh: Arid, desert, hot"
df_zones$clim_zone[df_zones$V1 == 5] <- "BWk: Arid, desert, cold"
df_zones$clim_zone[df_zones$V1 == 6] <- "BSh: Arid, steppe, hot"
df_zones$clim_zone[df_zones$V1 == 7] <- "BSk: Arid, steppe, cold"
df_zones$clim_zone[df_zones$V1 == 8] <- "Csa: Temperate, dry summer, hot summer"
df_zones$clim_zone[df_zones$V1 == 9] <- "Csb: Temperate, dry summer, warm summer"
df_zones$clim_zone[df_zones$V1 == 11] <- "Cwa: Temperate, dry winter, hot summer"
df_zones$clim_zone[df_zones$V1 == 12] <- "Cwb: Temperate, dry winter, warm summer"
df_zones$clim_zone[df_zones$V1 == 14] <- "Cfa: Temperate, no dry season, hot summer"
df_zones$clim_zone[df_zones$V1 == 15] <- "Cfb: Temperate, no dry season, warm summer"
df_zones$clim_zone[df_zones$V1 == 17] <- "Dsa: Cold, dry summer, hot summer"
df_zones$clim_zone[df_zones$V1 == 18] <- "Dsb: Cold, dry summer, warm summer"
df_zones$clim_zone[df_zones$V1 == 19] <- "Dsc: Cold, dry summer, cold summer"
df_zones$clim_zone[df_zones$V1 == 22] <- "Dwb: Cold, dry winter, warm summer"
df_zones$clim_zone[df_zones$V1 == 26] <- "Dfb: Cold, no dry season, warm summer"
df_zones$clim_zone[df_zones$V1 == 29] <- "ET: Polar, tundra"

# Step-9: Save the dataset with districts and climate zones for India
sum <- df_zones |>
        dplyr::group_by(state.name, district.name, district.code, clim_zone) %>% 
        dplyr::summarize(count=n())

df_zones <- df_zones |> 
  dplyr::rename(clim_code = V1)

# Step-10: Assign shorter climate code (from Anna's paper)
df_zones <- df_zones |>
  mutate(clim_zone_short = case_when(
    clim_code < 3 ~ "trop_wet",
    clim_code == 3 ~ "trop_wet_dry",
    clim_code == 4 ~ "arid",
    clim_code == 6 ~ "semi-Arid",
    clim_code == 8 | clim_code == 11 | clim_code == 12 | clim_code ==14 ~ "hum_sub-trop",
    clim_code > 18 ~ "mountain")) |> 
  mutate(monsoon_zone = case_when(
    clim_zone_short == "Tropical wet" ~ "monsoon",
    clim_zone_short == "Tropical wet and dry" ~ "monsoon",
    clim_zone_short == "Humid sub-tropical" ~ "monsoon",
    TRUE ~ "non-monsson"))

# Step-11: Save dataset with districts and climate zones for India
## Check if the directory exists and create if not
if (!dir.exists(here("data", "processed-data"))) {
  # Create the directory if it does not exist
  dir.create(here("data", "processed-data"), showWarnings = TRUE, recursive = TRUE)
}

## Save the file
write_fst(df_zones, path = here("data", "processed-data", "1.2-india-dist-climate-zones.fst"))