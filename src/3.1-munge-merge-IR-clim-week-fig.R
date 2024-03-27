# Title: "Merge the dataset with temp vars with IR data"

# Preparatory ----
## load-packages ----- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive)

## Load-datasets -----
### IR data ------ 
file_path_IR <- "./data/processed-data/1.3-dhs-IR-vars-climate-zones.fst"
df_IR_long_w_zones_dt <- read_fst(file_path_IR, as.data.table = TRUE)
colnames(df_IR_long_w_zones_dt)
### Remove week_of_year as it was already created in 2.2
df_IR_long_w_zones_dt$week_of_year <- NULL

### Climate data ------
file_path_climate <- "./data/processed-data/2.4-dhs-psu-paper.fst" 
df_climate_final <- read_fst(file_path_climate, as.data.table = TRUE)
colnames(df_climate_final)
# Merge IR and Temperature data ---- 
df_paper_final <- merge(df_IR_long_w_zones_dt, df_climate_final,
                           by.x = c("psu", "dob"),
                           by.y = c("psu", "date"))

# Assign Variable Labels ----
library(Hmisc)
label(df_paper_final$hotday_85_wb) <- "Hotday(85th percentile)-WB"
label(df_paper_final$hw_85_wb_2d) <- "HW(85th percentile)-WB(2 days)"
label(df_paper_final$hw_85_wb_3d) <- "HW(85th percentile)-WB(3 days)"
label(df_paper_final$hw_85_wb_5d) <- "HW(85th percentile)-WB(5 days)"
label(df_paper_final$hotday_90_wb) <- "Hotday(90th percentile)-WB"
label(df_paper_final$hw_90_wb_2d) <- "HW(90th percentile)-WB(2 days)"
label(df_paper_final$hw_90_wb_3d) <- "HW(90th percentile)-WB(3 days)"
label(df_paper_final$hw_90_wb_5d) <- "HW(90th percentile)-WB(5 days)"
label(df_paper_final$hotday_95_wb) <- "Hotday(95th percentile)-WB"
label(df_paper_final$hw_95_wb_2d) <- "HW(95th percentile)-WB(2 days)"
label(df_paper_final$hw_95_wb_3d) <- "HW(95th percentile)-WB(3 days)"
label(df_paper_final$hw_95_wb_5d) <- "HW(95th percentile)-WB(5 days)"

label(df_paper_final$hotday_26_wb) <- "Hotday(26C)-WB"
label(df_paper_final$hw_26_wb_2d) <- "HW(26C)-WB(2 days)"
label(df_paper_final$hw_26_wb_3d) <- "HW(26C)-WB(3 days)"
label(df_paper_final$hw_26_wb_5d) <- "HW(26C)-WB(5 days)"
label(df_paper_final$hotday_28_wb) <- "Hotday(28C)-WB"
label(df_paper_final$hw_28_wb_2d) <- "HW(28C)-WB(2 days)"
label(df_paper_final$hw_28_wb_3d) <- "HW(28C)-WB(3 days)"
label(df_paper_final$hw_28_wb_5d) <- "HW(28C)-WB(5 days)"
label(df_paper_final$hotday_30_wb) <- "Hotday(30C)-WB"
label(df_paper_final$hw_30_wb_2d) <- "HW(30C)-WB(2 days)"
label(df_paper_final$hw_30_wb_3d) <- "HW(30C)-WB(3 days)"
label(df_paper_final$hw_30_wb_5d) <- "HW(30C)-WB(5 days)"
label(df_paper_final$hotday_32_wb) <- "Hotday(32C)-WB"
label(df_paper_final$hw_32_wb_2d) <- "HW(32C)-WB(2 days)"
label(df_paper_final$hw_32_wb_3d) <- "HW(32C)-WB(3 days)"
label(df_paper_final$hw_32_wb_5d) <- "HW(32C)-WB(5 days)"

print("variable labels assigned")

# Save file ---- 
## Saving in RDS because saving in fst looses some of the variable labels
saveRDS(df_paper_final, "./data/processed-data/3.1-final-data-for-paper.rds")
print("finished processing 3.1")


