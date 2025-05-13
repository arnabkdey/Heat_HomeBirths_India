library(here)

# Paths for the project on the shared drive
path_project <- "path/to/your/project/directory" # Replace with your project path
path_processed <- here(path_project, "data/processed_data/")
path_outputs <- here(path_project, "outputs/")

# Paths for datasets from the common-datasets folder
root_dir <- "path/to/your/common-datasets/directory" # Replace with your common datasets path

## DHS datasets
path_dhs_india_IR <- here(root_dir, "dhs-raw-datasets/India/2019-21/individual-recode/IAIR7EDT/")
path_dhs_india_shp <- here(root_dir, "dhs-raw-datasets/India/2019-21/geographic-data/IAGE7AFL/")

## KG Beck Climate zones
path_beck <- here(root_dir, "climate-datasets/global/climate-region-maps/Beck_KG")

## Temperature and precipitation
path_wbgt_max_raw <- here(root_dir, "climate-datasets/global/wbgt-brimicombe/world-temp-wetbulb-max/")
path_db_max_noaa <- here(root_dir, "climate-datasets/global/air-temperature-noaa/world-temp-drybulb-max/1979-2023/")
path_db_max_era5 <- here(root_dir, "climate-datasets/global/air-temperature-era5/tmax/")