pacman::p_load(tidyverse, data.table, janitor, fst, beepr, openxlsx, lme4, broom, broom.mixed, googledrive, here)
library(multcomp)

rm(list = ls())

# Create a folder for the outputs ----
path_out <- here("3-outputs", "models", "models-with-interaction")
if (!dir.exists(path_out)) {
  # Create the directory if it does not exist
  dir.create(path_out, showWarnings = TRUE, recursive = TRUE)
}

# Load models ----
path_processed <- here("2-data", "2.2-processed-data")
model_int_all <- readRDS(here(path_processed, "2.2-models-interactions-all-exp.rds"))
print("finished loading models")
model_names <- names(model_int_all)

# Varlist of interaction terms -----
varlist_interaction <- c("rural", "hh_caste_club", "hh_religion_bi", 
                          "hh_wealth_quintile_ru_og", "lt_tmax_mean_cat_tert_wb", 
                          "access_issue_distance")

# Get the index of the models that contain the interaction tems ----
index_rural <- model_names[1:24]
index_caste <- model_names[25:48]
index_religion <- model_names[49:72]
index_wealth <- model_names[73:96]
index_lt_tmax_mean <- model_names[97:120]
index_access_issue_distance <- model_names[121:144]

# Assess the number of times zero has to be repeated ----
## This is worked using the example of Rural/Urban but works for all interaction terms

## First, get the number of coefficients in the model ----
sum_model_cur <- summary(model_int_all[[1]])
nrow <- nrow(sum_model_cur$coefficients)
print(nrow) # 33

# a = num of coefficients = 35
# b = number of levels of the effect modifier = 2 (Rural/Urban)
# c = number of trailing elements after rep = b - 1 = 1
# Total number of times zero has to be repeated = a - c - 2 = 33-1-2 = 30
rep_zero <- 30


# Extract multcomp objects for each Effect Modifier -----
## For Rural ----
### Run loop to extract multcomp objects ----
list_rural <- list()
for (i in index_rural) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Rural, Urban
    contrast_cur <- rbind("Rural" = c(0, 1, rep(0, rep_zero), 1),
                    "Urban" = c(0, 1, rep(0, rep_zero), 0),    
                    "Rural vs Urban" = c(0, 0, rep(0, rep_zero), 1))
    # names(model_cur) <- names(model_int_all[[i]])
    # print(names(model_cur))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_rural[[name_cur]] <- tab_cur
}

### Save to excel ----
sheet_names_rural <- names(model_int_all[index_rural])
write.xlsx(list_rural, here(path_out, "multcomp-cis-rural.xlsx"), sheetName = sheet_names_rural)

## For Caste ----
### Run loop to extract multcomp objects ----
list_caste <- list()
for (i in index_caste) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- OBC, Other, SC, ST
    contrast_cur <- rbind("OBC" = c(0, 1, rep(0, rep_zero), 0, 0, 0),
                    "Other" = c(0, 1, rep(0, rep_zero), 1, 0, 0), 
                    "SC" = c(0, 1, rep(0, rep_zero), 0, 1, 0), 
                    "ST" = c(0, 1, rep(0, rep_zero), 0, 0, 1),
                    "OBC vs Other" = c(0, 0, rep(0, rep_zero), 1, 0, 0),
                    "OBC vs SC" = c(0, 0, rep(0, rep_zero), 0, 1, 0),
                    "OBC vs ST" = c(0, 0, rep(0, rep_zero), 0, 0, 1),
                    "Other vs SC" = c(0, 0, rep(0, rep_zero), -1, 1, 0),
                    "Other vs ST" = c(0, 0, rep(0, rep_zero), -1, 0, 1),
                    "SC vs ST" = c(0, 0, rep(0, rep_zero), 0, -1, 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_caste[[name_cur]] <- tab_cur
}    

### Save to excel ----
sheet_names_caste <- names(model_int_all[index_caste])
write.xlsx(list_caste, here(path_out, "multcomp-cis-caste.xlsx"), sheetName = sheet_names_caste)

## For Religion ----
### Run loop to extract multcomp objects ----
list_rel <- list()
for (i in index_religion) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Hindu, Not-Hindu
    contrast_cur <- rbind("Hindu" = c(0, 1, rep(0, rep_zero), 0),
                    "Not-Hindu" = c(0, 1, rep(0, rep_zero), 1), 
                    "Hindu vs Not-Hindu" = c(0, 0, rep(0, rep_zero), 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_rel[[name_cur]] <- tab_cur
}

### Save to excel ----
sheet_names_rel <- names(model_int_all[index_religion])
write.xlsx(list_rel, here(path_out, "multcomp-cis-religion.xlsx"), sheetName = sheet_names_rel)

## For Wealth ----
### Run loop to extract multcomp objects ----
list_wealth <- list()
# model_int_all[[37]]
for (i in index_wealth) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Poorest, Poorer, Middle, Richer, Richest
    contrast_cur <- rbind("Poorest" = c(0, 1, rep(0, rep_zero), 0, 0, 0, 0),
                    "Poorer" = c(0, 1, rep(0, rep_zero), 1, 0, 0, 0), 
                    "Middle" = c(0, 1, rep(0, rep_zero), 0, 1, 0, 0), 
                    "Richer" = c(0, 1, rep(0, rep_zero), 0, 0, 1, 0),
                    "Richest" = c(0, 1, rep(0, rep_zero), 0, 0, 0, 1),
                    "Poorest vs Poorer" = c(0, 0, rep(0, rep_zero), 1, 0, 0, 0),
                    "Poorest vs Middle" = c(0, 0, rep(0, rep_zero), 0, 1, 0, 0),
                    "Poorest vs Richer" = c(0, 0, rep(0, rep_zero), 0, 0, 1, 0),
                    "Poorest vs Richest" = c(0, 0, rep(0, rep_zero), 0, 0, 0, 1),
                    "Poorer vs Middle" = c(0, 0, rep(0, rep_zero), -1, 1, 0, 0),
                    "Poorer vs Richer" = c(0, 0, rep(0, rep_zero), -1, 0, 1, 0),
                    "Poorer vs Richest" = c(0, 0, rep(0, rep_zero), -1, 0, 0, 1),
                    "Middle vs Richer" = c(0, 0, rep(0, rep_zero), 0, -1, 1, 0),
                    "Middle vs Richest" = c(0, 0, rep(0, rep_zero), 0, -1, 0, 1),
                    "Richer vs Richest" = c(0, 0, rep(0, rep_zero), 0, 0, -1, 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_wealth[[name_cur]] <- tab_cur
}

### Save to excel ----
sheet_names_wealth <- names(model_int_all[index_wealth])
write.xlsx(list_wealth, here(path_out, "multcomp-cis-wealth.xlsx"), sheetName = sheet_names_wealth)

## For lt_tmax_mean ----
### Run loop to extract multcomp objects ----
list_lt_tmax_mean <- list()
for (i in index_lt_tmax_mean) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- Lowest_Tertile, Medium_Tertile, High_Tertile
    contrast_cur <- rbind("Lowest_Tertile" = c(0, 1, rep(0, rep_zero), 0, 0),
                    "Medium_Tertile" = c(0, 1, rep(0, rep_zero), 1, 0), 
                    "High_Tertile" = c(0, 1, rep(0, rep_zero), 0, 1),
                    "Lowest_Tertile vs Medium_Tertile" = c(0, 0, rep(0, rep_zero), 1, 0),
                    "Lowest_Tertile vs High_Tertile" = c(0, 0, rep(0, rep_zero), 0, 1),
                    "Medium_Tertile vs High_Tertile" = c(0, 0, rep(0, rep_zero), -1, 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_lt_tmax_mean[[name_cur]] <- tab_cur                    
}    

### Save to excel ----
sheet_names_lt_tmax_mean <- names(model_int_all[index_lt_tmax_mean])
write.xlsx(list_lt_tmax_mean, here(path_out, "multcomp-cis-lt_tmax_mean.xlsx"), sheetName = sheet_names_lt_tmax_mean)

## For Access Issue Distance ----
### Run loop to extract multcomp objects ----
list_access_issue_distance <- list()
for (i in index_access_issue_distance) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Levels- 'big-problem', 'not-a-big-problem'
    contrast_cur <- rbind("big-problem" = c(0, 1, rep(0, rep_zero), 0),
                    "not-a-big-problem" = c(0, 1, rep(0, rep_zero), 1), 
                    "big-problem vs not-a-big-problem" = c(0, 0, rep(0, rep_zero), 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_access_issue_distance[[name_cur]] <- tab_cur
}

### Save to excel ----
sheet_names_access_issue_distance <- names(model_int_all[index_access_issue_distance])
write.xlsx(list_access_issue_distance, here(path_out, "multcomp-cis-access_issue_distance.xlsx"), sheetName = sheet_names_access_issue_distance)

# Save models arranged by exposure definition ----
## For DOY - 90 ----
list_doy_90 <- c(list_caste[1:4], list_rel[1:4], list_rural[1:4], list_wealth[1:4], list_access_issue_distance[1:4], list_lt_tmax_mean[1:4])
sheet_names_doy_90 <- c(names(list_caste[1:4]), names(list_rel[1:4]), names(list_rural[1:4]), names(list_wealth[1:4]), names(list_access_issue_distance[1:4]), names(list_lt_tmax_mean[1:4]))
write.xlsx(list_doy_90, here(path_out, "multcomp-doy-90.xlsx"), sheetName = sheet_names_doy_90)

## For DOY - 95 ----
list_doy_95 <- c(list_caste[5:8], list_rel[5:8], list_rural[5:8], list_wealth[5:8], list_access_issue_distance[5:8], list_lt_tmax_mean[5:8])
sheet_names_doy_95 <- c(names(list_caste[5:8]), names(list_rel[5:8]), names(list_rural[5:8]), names(list_wealth[5:8]), names(list_access_issue_distance[5:8]), names(list_lt_tmax_mean[5:8]))
write.xlsx(list_doy_95, here(path_out, "multcomp-doy-95.xlsx"), sheetName = sheet_names_doy_95)

## For DOY - 97 ----
list_doy_97 <- c(list_caste[9:12], list_rel[9:12], list_rural[9:12], list_wealth[9:12], list_access_issue_distance[9:12], list_lt_tmax_mean[9:12])
sheet_names_doy_97 <- c(names(list_caste[9:12]), names(list_rel[9:12]), names(list_rural[9:12]), names(list_wealth[9:12]), names(list_access_issue_distance[9:12]), names(list_lt_tmax_mean[9:12]))
write.xlsx(list_doy_97, here(path_out, "multcomp-doy-97.xlsx"), sheetName = sheet_names_doy_97)

## For absolute value - 30 ----
list_abs_30 <- c(list_caste[13:16], list_rel[13:16], list_rural[13:16], list_wealth[13:16], list_access_issue_distance[13:16], list_lt_tmax_mean[13:16])
sheet_names_abs_30 <- c(names(list_caste[13:16]), names(list_rel[13:16]), names(list_rural[13:16]), names(list_wealth[13:16]), names(list_access_issue_distance[13:16]), names(list_lt_tmax_mean[13:16]))
write.xlsx(list_abs_30, here(path_out, "multcomp-abs-30.xlsx"), sheetName = sheet_names_abs_30)

## For absolute value - 31 ----
list_abs_31 <- c(list_caste[17:20], list_rel[17:20], list_rural[17:20], list_wealth[17:20], list_access_issue_distance[17:20], list_lt_tmax_mean[17:20])
sheet_names_abs_31 <- c(names(list_caste[17:20]), names(list_rel[17:20]), names(list_rural[17:20]), names(list_wealth[17:20]), names(list_access_issue_distance[17:20]), names(list_lt_tmax_mean[17:20]))
write.xlsx(list_abs_31, here(path_out, "multcomp-abs-31.xlsx"), sheetName = sheet_names_abs_31)

## For absolute value - 32 ----
list_abs_32 <- c(list_caste[21:24], list_rel[21:24], list_rural[21:24], list_wealth[21:24], list_access_issue_distance[21:24], list_lt_tmax_mean[21:24])
sheet_names_abs_32 <- c(names(list_caste[21:24]), names(list_rel[21:24]), names(list_rural[21:24]), names(list_wealth[21:24]), names(list_access_issue_distance[21:24]), names(list_lt_tmax_mean[21:24]))
write.xlsx(list_abs_32, here(path_out, "multcomp-abs-32.xlsx"), sheetName = sheet_names_abs_32)