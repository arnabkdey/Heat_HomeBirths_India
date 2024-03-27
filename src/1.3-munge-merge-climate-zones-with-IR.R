# title: "Merge climate zones into IR dataset"

# Preparatory ----
## Load packages ----- 
library(tidyverse)
library(data.table)
library(fst)
## Read datasets ----- 
### IR vars created datasets -----
rm(list = ls())
file_path_IR <- "./data/processed-data/1.1-dhs-IR-vars-created.fst"
df_IR_long <- read_fst(file_path_IR, as.data.table = TRUE)

## Climate zones of India with districts dataset
file_path_zones <- "./data/processed-data/1.2-india-dist-climate-zones.fst"
df_zones <- read_fst(file_path_zones, as.data.table = TRUE)

# Merge climate zones into IR dataset  ----
## Convert all variable labels to lower case in df_zones and match state/dist names -----
df_zones <- df_zones |>
  mutate(state_name = tolower(state.name),
         dist_name = tolower(district.name)) |>
  mutate(state_name = ifelse(state_name == "dadra & nagar haveli & daman & diu", 
                                "dadra & nagar haveli and daman & diu", state_name)) |>
  mutate(dist_name = case_when(
                        dist_name == "maharajganj" ~ "mahrajganj",
                        dist_name == "buxer" ~ "buxar",
                        dist_name == "north & middle andaman" ~ "north  & middle andaman",
                        dist_name == "janjgir-champa" ~ "janjgir - champa",
                        dist_name == "leh" ~ "leh(ladakh)",
                        dist_name == "north district" ~ "north  district",
                        dist_name == "sant ravidas nagar" ~ "sant ravidas nagar (bhadohi)",
                        TRUE ~ dist_name)) |>
  dplyr::select(-state.name, -district.name, -district.code)
colnames(df_zones)

## Merge df_zones with df_IR_long -----
df_IR_long_w_zones <- merge(df_IR_long, df_zones, 
                    by.x = c("state_name", "dist_name"), 
                    by.y = c("state_name", "dist_name"), 
                    all.x = TRUE)
# colnames(df_IR_long_w_zones)
# sum(is.na(df_IR_long_w_zones$climate_zone))

# Save output ----
## Check if the directory exists and create if not
if (!dir.exists("./data/processed-data/")) {
  # Create the directory if it does not exist
  dir.create("./data/processed-data/", showWarnings = TRUE, recursive = TRUE)
}

## Save the file
write_fst(df_IR_long_w_zones, path = "./data/processed-data/1.3-dhs-IR-vars-climate-zones.fst")
