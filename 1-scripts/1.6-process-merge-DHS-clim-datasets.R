# Title: "Merge the dataset with temp vars with IR data"

# Preparatory ----
## load-packages ----- 
rm(list = ls())
pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)

## Load-datasets -----
### IR data ------ 
path_processed <- here("2-data", "2.2-processed-data")
df_IR_long <- read_fst(here(path_processed, "1.1-dhs-IR-vars-created.fst"), as.data.table = TRUE)
colnames(df_IR_long)
### Remove week_of_year as it was already created in 2.2
df_IR_long$week_of_year <- NULL

### Climate data ------
df_climate_final <- read_fst(here(path_processed, "1.5-dhs-psu-paper.fst"), as.data.table = TRUE)
colnames(df_climate_final)
# Merge IR and Temperature data ---- 
df_paper_final <- merge(df_IR_long, df_climate_final,
                           by.x = c("psu", "dob"),
                           by.y = c("psu", "date"))

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

## Ntile doy
label(df_paper_final$hotday_wb_90_doy) <- "Hotday(90th percentile)-WB-doy"
label(df_paper_final$hw_wb_90_doy_2d) <- "HW(90th percentile)-WB(2 days)-doy"
label(df_paper_final$hw_wb_90_doy_3d) <- "HW(90th percentile)-WB(3 days)-doy"
label(df_paper_final$hw_wb_90_doy_5d) <- "HW(90th percentile)-WB(5 days)-doy"
label(df_paper_final$hotday_wb_95_doy) <- "Hotday(95th percentile)-WB-doy"
label(df_paper_final$hw_wb_95_doy_2d) <- "HW(95th percentile)-WB(2 days)-doy"
label(df_paper_final$hw_wb_95_doy_3d) <- "HW(95th percentile)-WB(3 days)-doy"
label(df_paper_final$hw_wb_95_doy_5d) <- "HW(95th percentile)-WB(5 days)-doy"
label(df_paper_final$hotday_wb_97_doy) <- "Hotday(97th percentile)-WB-doy"
label(df_paper_final$hw_wb_97_doy_2d) <- "HW(97th percentile)-WB(2 days)-doy"
label(df_paper_final$hw_wb_97_doy_3d) <- "HW(97th percentile)-WB(3 days)-doy"
label(df_paper_final$hw_wb_97_doy_5d) <- "HW(97th percentile)-WB(5 days)-doy"

## Ntile harmo
label(df_paper_final$hotday_wb_90_harmo) <- "Hotday(90th percentile)-WB-harmo"
label(df_paper_final$hw_wb_90_harmo_2d) <- "HW(90th percentile)-WB(2 days)-harmo"
label(df_paper_final$hw_wb_90_harmo_3d) <- "HW(90th percentile)-WB(3 days)-harmo"
label(df_paper_final$hw_wb_90_harmo_5d) <- "HW(90th percentile)-WB(5 days)-harmo"
label(df_paper_final$hotday_wb_95_harmo) <- "Hotday(95th percentile)-WB-harmo"
label(df_paper_final$hw_wb_95_harmo_2d) <- "HW(95th percentile)-WB(2 days)-harmo"
label(df_paper_final$hw_wb_95_harmo_3d) <- "HW(95th percentile)-WB(3 days)-harmo"
label(df_paper_final$hw_wb_95_harmo_5d) <- "HW(95th percentile)-WB(5 days)-harmo"
label(df_paper_final$hotday_wb_97_harmo) <- "Hotday(97th percentile)-WB-harmo"
label(df_paper_final$hw_wb_97_harmo_2d) <- "HW(97th percentile)-WB(2 days)-harmo"
label(df_paper_final$hw_wb_97_harmo_3d) <- "HW(97th percentile)-WB(3 days)-harmo"
label(df_paper_final$hw_wb_97_harmo_5d) <- "HW(97th percentile)-WB(5 days)-harmo"

print("Variable labels assigned")

# Center the mean precipitation dataset ----
df_paper_final$mean_precip_center <- scale(df_paper_final$mean_precip, center = TRUE, scale = FALSE)

# Save file ---- 
## Saving in RDS because saving in fst looses some of the variable labels
saveRDS(df_paper_final, here(path_processed, "1.6-final-data-for-paper.rds"))
print("finished processing 1.6")