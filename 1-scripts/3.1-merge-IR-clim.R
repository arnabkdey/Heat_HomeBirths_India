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
df_climate_final <- read_fst(here(path_processed, "2.3-dhs-psu-paper.fst"), as.data.table = TRUE)
colnames(df_climate_final)
# Merge IR and Temperature data ---- 
df_paper_final <- merge(df_IR_long, df_climate_final,
                           by.x = c("psu", "dob"),
                           by.y = c("psu", "date"))

# Assign Variable Labels ----
library(Hmisc)
label(df_paper_final$hotday_90_wb) <- "Hotday(90th percentile)-WB"
label(df_paper_final$hw_90_wb_2d) <- "HW(90th percentile)-WB(2 days)"
label(df_paper_final$hw_90_wb_3d) <- "HW(90th percentile)-WB(3 days)"
label(df_paper_final$hw_90_wb_5d) <- "HW(90th percentile)-WB(5 days)"
label(df_paper_final$hotday_95_wb) <- "Hotday(95th percentile)-WB"
label(df_paper_final$hw_95_wb_2d) <- "HW(95th percentile)-WB(2 days)"
label(df_paper_final$hw_95_wb_3d) <- "HW(95th percentile)-WB(3 days)"
label(df_paper_final$hw_95_wb_5d) <- "HW(95th percentile)-WB(5 days)"
label(df_paper_final$hotday_97_wb) <- "Hotday(97th percentile)-WB"
label(df_paper_final$hw_97_wb_2d) <- "HW(97th percentile)-WB(2 days)"
label(df_paper_final$hw_97_wb_3d) <- "HW(97th percentile)-WB(3 days)"
label(df_paper_final$hw_97_wb_5d) <- "HW(97th percentile)-WB(5 days)"

label(df_paper_final$hotday_30_wb) <- "Hotday(30C)-WB"
label(df_paper_final$hw_30_wb_2d) <- "HW(30C)-WB(2 days)"
label(df_paper_final$hw_30_wb_3d) <- "HW(30C)-WB(3 days)"
label(df_paper_final$hw_30_wb_5d) <- "HW(30C)-WB(5 days)"
label(df_paper_final$hotday_31_wb) <- "Hotday(31C)-WB"
label(df_paper_final$hw_31_wb_2d) <- "HW(31C)-WB(2 days)"
label(df_paper_final$hw_31_wb_3d) <- "HW(31C)-WB(3 days)"
label(df_paper_final$hw_31_wb_5d) <- "HW(31C)-WB(5 days)"
label(df_paper_final$hotday_32_wb) <- "Hotday(32C)-WB"
label(df_paper_final$hw_32_wb_2d) <- "HW(32C)-WB(2 days)"
label(df_paper_final$hw_32_wb_3d) <- "HW(32C)-WB(3 days)"
label(df_paper_final$hw_32_wb_5d) <- "HW(32C)-WB(5 days)"

print("variable labels assigned")

# Save file ---- 
## Saving in RDS because saving in fst looses some of the variable labels
saveRDS(df_paper_final, here(path_processed, "3.1-final-data-for-paper.rds"))
print("finished processing 3.1")


