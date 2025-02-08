# Title: "Merge the dataset with temp vars with IR data"

# load-packages ----- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, here)

# set paths ----
source(here("paths-mac.R"))

## load-datasets -----
### IR data ------ 
path_processed <- here(path_project, "data", "processed-data")
df_IR_long <- read_fst(here(path_processed, "1.2-dhs-IR-vars-created.fst"), as.data.table = TRUE)
colnames(df_IR_long)

### Remove week_of_year as it was already created in 2.2
df_IR_long$dob_week_of_year <- NULL

### Climate data ------
df_climate_final <- read_fst(here(path_processed, "1.6-dhs-psu-paper-tmax-added.fst"), as.data.table = TRUE)

# Merge IR and Temperature data ---- 
df_IR_long$psu <- as.factor(df_IR_long$psu)
df_paper_final <- merge(df_IR_long, df_climate_final,
                           by.x = c("psu", "dob"),
                           by.y = c("psu", "date"))

## Check for missing values
nrow(df_IR_long) # 198,889
nrow(df_paper_final) # 196,396 - 2,493 missing values dropped due to missing values in climate data

# Assign Variable Labels ----
library(Hmisc)
# Label variables
## Absolute values
label(df_paper_final$hotday_wb_30) <- "Hotday(30C)-WB"
label(df_paper_final$hw_wb_30_2d) <- "HW(30C)-WB(2 days)"
label(df_paper_final$hw_wb_30_3d) <- "HW(30C)-WB(3 days)"
label(df_paper_final$hw_wb_30_5d) <- "HW(30C)-WB(5 days)"
label(df_paper_final$hotday_wb_31) <- "Hotday(31C)-WB"
label(df_paper_final$hw_wb_31_2d) <- "HW(31C)-WB(2 days)"
label(df_paper_final$hw_wb_31_3d) <- "HW(31C)-WB(3 days)"
label(df_paper_final$hw_wb_31_5d) <- "HW(31C)-WB(5 days)"
label(df_paper_final$hotday_wb_32) <- "Hotday(32C)-WB"
label(df_paper_final$hw_wb_32_2d) <- "HW(32C)-WB(2 days)"
label(df_paper_final$hw_wb_32_3d) <- "HW(32C)-WB(3 days)"
label(df_paper_final$hw_wb_32_5d) <- "HW(32C)-WB(5 days)"

## for wet bulb (wb) ntile
label(df_paper_final$hotday_wb_90) <- "Hotday(90th percentile)-WB"
label(df_paper_final$hw_wb_90_2d) <- "HW(90th percentile)-WB(2 days)"
label(df_paper_final$hw_wb_90_3d) <- "HW(90th percentile)-WB(3 days)"
label(df_paper_final$hw_wb_90_5d) <- "HW(90th percentile)-WB(5 days)"
label(df_paper_final$hotday_wb_95) <- "Hotday(95th percentile)-WB"
label(df_paper_final$hw_wb_95_2d) <- "HW(95th percentile)-WB(2 days)"
label(df_paper_final$hw_wb_95_3d) <- "HW(95th percentile)-WB(3 days)"
label(df_paper_final$hw_wb_95_5d) <- "HW(95th percentile)-WB(5 days)"
label(df_paper_final$hotday_wb_97) <- "Hotday(97th percentile)-WB"
label(df_paper_final$hw_wb_97_2d) <- "HW(97th percentile)-WB(2 days)"
label(df_paper_final$hw_wb_97_3d) <- "HW(97th percentile)-WB(3 days)"
label(df_paper_final$hw_wb_97_5d) <- "HW(97th percentile)-WB(5 days)"

## for dry bulb (db) ntile
label(df_paper_final$hotday_db_90) <- "Hotday(90th percentile)-DB"
label(df_paper_final$hw_db_90_2d) <- "HW(90th percentile)-DB(2 days)"
label(df_paper_final$hw_db_90_3d) <- "HW(90th percentile)-DB(3 days)"
label(df_paper_final$hw_db_90_5d) <- "HW(90th percentile)-DB(5 days)"
label(df_paper_final$hotday_db_95) <- "Hotday(95th percentile)-DB"
label(df_paper_final$hw_db_95_2d) <- "HW(95th percentile)-DB(2 days)"
label(df_paper_final$hw_db_95_3d) <- "HW(95th percentile)-DB(3 days)"
label(df_paper_final$hw_db_95_5d) <- "HW(95th percentile)-DB(5 days)"
label(df_paper_final$hotday_db_97) <- "Hotday(97th percentile)-DB"
label(df_paper_final$hw_db_97_2d) <- "HW(97th percentile)-DB(2 days)"
label(df_paper_final$hw_db_97_3d) <- "HW(97th percentile)-DB(3 days)"
label(df_paper_final$hw_db_97_5d) <- "HW(97th percentile)-DB(5 days)"

print("Variable labels assigned")
nrow(df_paper_final)
# Center the mean precipitation dataset ----
df_paper_final$mean_precip_center <- scale(df_paper_final$mean_precip, center = TRUE, scale = FALSE)

# Save file ---- 
## Saving in RDS because saving in fst looses some of the variable labels
df_paper_final |> saveRDS(here(path_processed, "1.7-final-data-for-paper.rds"))
print("finished processing 1.7")