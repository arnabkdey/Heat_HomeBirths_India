# ---------------------------------------------------------------------------------
# @project: Heat and Home Births in India
# @author: Arnab K. Dey, Anna Dimitrova
# @organization: Scripps Institution of Oceanography, UC San Diego
# @description: This script merges the DHS dataset with the climate datasets that contain the heatwave definitions for each PSU.
# @date: March 2025

# load-packages ----- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)

# set paths ----
source(here("paths_mac.R"))

## load-datasets -----
### IR data ------ 
path_processed <- here(path_project, "data", "processed_data")
df_IR_long <- read_fst(here(path_processed, "1.1.2_dhs_IR_vars_created_imp.fst"), 
  as.data.table = TRUE)

### Remove week_of_year as it was already created in 2.2
df_IR_long$dob_week_of_year <- NULL

### Climate data ------
df_climate_final <- read_fst(here(path_processed, "1.2.3_clim_data_vars.fst"), 
  as.data.table = TRUE)

# Merge IR and Temperature data ---- 
df_IR_long$psu <- as.factor(df_IR_long$psu)

df_paper_final <- merge(df_IR_long, df_climate_final,
                           by.x = c("psu", "dob"),
                           by.y = c("psu", "date"))

## Check for missing values
nrow(df_IR_long) # 210,735
nrow(df_paper_final) # 209,924 - 811 missing values dropped due to missing values in climate data

# Save file ---- 
df_paper_final |> saveRDS(here(path_processed, "1.3.1_final_data_for_paper.rds"))
print("finished processing 1.3.1")