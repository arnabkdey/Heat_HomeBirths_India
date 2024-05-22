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
model_int_all <- readRDS(here(path_processed, "4.4-models-interactions-90-30.rds"))
print("finished loading models")
model_names <- names(model_int_all)

# Varlist of interaction terms -----
varlist_interaction <- c("rural", "hh_caste_club", "hh_religion_bi", 
                          "hh_wealth_quintile_ru_og", "lt_tmax_mean_cat_tert_wb", 
                          "lt_tmax_median_cat_tert_wb")

# Get the index of the models that contain the interaction tems ----
index_rural <- model_names[1:12]
index_caste <- model_names[13:24]
index_religion <- model_names[25:36]
index_wealth <- model_names[37:48]
index_lt_tmax_mean <- model_names[49:60]
index_lt_tmax_median <- model_names[61:72]

# Extract multcomp objects for each Effect Modifier -----
## For Rural ----
### Run loop to extract multcomp objects ----
list_rural <- list()
model_int_all[[1]]
# model_int_all[[1]]
for (i in index_rural) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Craete Contrasts
    sum_model_cur <- summary(model_cur)
    nrow <- nrow(sum_model_cur$coefficients)
    print(nrow)
    # a = num of coefficients- 35
    # b = number of levels of the effect modifier- 2
    # c = number of trailing elements after rep- b - 1 = 1
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- Rural, Urban
    contrast_cur <- rbind("Rural" = c(0, 1, rep(0, 32), 1),
                    "Urban" = c(0, 1, rep(0, 32), 0),    
                    "Rural vs Urban" = c(0, 0, rep(0, 32), 1))
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
    # Create Contrasts
    # sum_model_cur <- summary(model_cur)
    # nrow <- nrow(sum_model_cur$coefficients)
    # print(nrow)
    # a = num of coefficients- 37
    # b = number of levels of the effect modifier- 4
    # c = number of trailing elements after rep- b - 1 = 3
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- OBC, Other, SC, ST
    contrast_cur <- rbind("OBC" = c(0, 1, rep(0, 32), 0, 0, 0),
                    "Other" = c(0, 1, rep(0, 32), 1, 0, 0), 
                    "SC" = c(0, 1, rep(0, 32), 0, 1, 0), 
                    "ST" = c(0, 1, rep(0, 32), 0, 0, 1),
                    "OBC vs Other" = c(0, 0, rep(0, 32), 1, 0, 0),
                    "OBC vs SC" = c(0, 0, rep(0, 32), 0, 1, 0),
                    "OBC vs ST" = c(0, 0, rep(0, 32), 0, 0, 1),
                    "Other vs SC" = c(0, 0, rep(0, 32), -1, 1, 0),
                    "Other vs ST" = c(0, 0, rep(0, 32), -1, 0, 1),
                    "SC vs ST" = c(0, 0, rep(0, 32), 0, -1, 1))
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
# model_int_all[[25]]
for (i in index_religion) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Create Contrasts
    # sum_model_cur <- summary(model_cur)
    # nrow <- nrow(sum_model_cur$coefficients)
    # print(nrow)
    # a. num of coefficients- 35
    # b. number of levels of the effect modifier- 2
    # c. number of trailing elements after rep- b - 1 = 1
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- Hindu, Not-Hindu
    contrast_cur <- rbind("Hindu" = c(0, 1, rep(0, 32), 0),
                    "Not-Hindu" = c(0, 1, rep(0, 32), 1), 
                    "Hindu vs Not-Hindu" = c(0, 0, rep(0, 32), 1))
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
    # Create Contrasts
    # sum_model_cur <- summary(model_cur)
    # nrow <- nrow(sum_model_cur$coefficients)
    # print(nrow)
    # a. num of coefficients- 38
    # b. number of levels of the effect modifier- 5
    # c. number of trailing elements after rep- b - 1 = 4
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- Poorest, Poorer, Middle, Richer, Richest
    contrast_cur <- rbind("Poorest" = c(0, 1, rep(0, 32), 0, 0, 0, 0),
                    "Poorer" = c(0, 1, rep(0, 32), 1, 0, 0, 0), 
                    "Middle" = c(0, 1, rep(0, 32), 0, 1, 0, 0), 
                    "Richer" = c(0, 1, rep(0, 32), 0, 0, 1, 0),
                    "Richest" = c(0, 1, rep(0, 32), 0, 0, 0, 1),
                    "Poorest vs Poorer" = c(0, 0, rep(0, 32), 1, 0, 0, 0),
                    "Poorest vs Middle" = c(0, 0, rep(0, 32), 0, 1, 0, 0),
                    "Poorest vs Richer" = c(0, 0, rep(0, 32), 0, 0, 1, 0),
                    "Poorest vs Richest" = c(0, 0, rep(0, 32), 0, 0, 0, 1),
                    "Poorer vs Middle" = c(0, 0, rep(0, 32), -1, 1, 0, 0),
                    "Poorer vs Richer" = c(0, 0, rep(0, 32), -1, 0, 1, 0),
                    "Poorer vs Richest" = c(0, 0, rep(0, 32), -1, 0, 0, 1),
                    "Middle vs Richer" = c(0, 0, rep(0, 32), 0, -1, 1, 0),
                    "Middle vs Richest" = c(0, 0, rep(0, 32), 0, -1, 0, 1),
                    "Richer vs Richest" = c(0, 0, rep(0, 32), 0, 0, -1, 1))
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
# model_int_all[[49]]
for (i in index_lt_tmax_mean) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Create Contrasts
    # sum_model_cur <- summary(model_cur)
    # nrow <- nrow(sum_model_cur$coefficients)
    # print(nrow)
    # a. num of coefficients- 36
    # b. number of levels of the effect modifier- 3
    # c. number of trailing elements after rep- b - 1 = 2
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- Lowest_Tertile, Medium_Tertile, High_Tertile
    contrast_cur <- rbind("Lowest_Tertile" = c(0, 1, rep(0, 32), 0, 0),
                    "Medium_Tertile" = c(0, 1, rep(0, 32), 1, 0), 
                    "High_Tertile" = c(0, 1, rep(0, 32), 0, 1),
                    "Lowest_Tertile vs Medium_Tertile" = c(0, 0, rep(0, 32), 1, 0),
                    "Lowest_Tertile vs High_Tertile" = c(0, 0, rep(0, 32), 0, 1),
                    "Medium_Tertile vs High_Tertile" = c(0, 0, rep(0, 32), -1, 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_lt_tmax_mean[[name_cur]] <- tab_cur                    
}    

### Save to excel ----
sheet_names_lt_tmax_mean <- names(model_int_all[index_lt_tmax_mean])
write.xlsx(list_lt_tmax_mean, here(path_out, "multcomp-cis-lt_tmax_mean.xlsx"), sheetName = sheet_names_lt_tmax_mean)

## For lt_tmax_median ----
### Run loop to extract multcomp objects ----
list_lt_tmax_median <- list()
# model_int_all[[49]]
for (i in index_lt_tmax_median) {
    # Assign Model 
    model_cur <- model_int_all[[i]]
    name_cur <- names(model_int_all[i])
    # Create Contrasts
    # sum_model_cur <- summary(model_cur)
    # nrow <- nrow(sum_model_cur$coefficients)
    # print(nrow)
    # a. num of coefficients- 36
    # b. number of levels of the effect modifier- 3
    # c. number of trailing elements after rep- b - 1 = 2
    # Total number of times zero has to be repeated- a - c - 2 = 32
    # Levels- Lowest_Tertile, Medium_Tertile, High_Tertile
    contrast_cur <- rbind("Lowest_Tertile" = c(0, 1, rep(0, 32), 0, 0),
                    "Medium_Tertile" = c(0, 1, rep(0, 32), 1, 0), 
                    "High_Tertile" = c(0, 1, rep(0, 32), 0, 1),
                    "Lowest_Tertile vs Medium_Tertile" = c(0, 0, rep(0, 32), 1, 0),
                    "Lowest_Tertile vs High_Tertile" = c(0, 0, rep(0, 32), 0, 1),
                    "Medium_Tertile vs High_Tertile" = c(0, 0, rep(0, 32), -1, 1))
    glht_cur <- glht(model_cur, linfct = contrast_cur)
    tab_cur <- tidy(glht_cur, conf.int = TRUE, exponentiate = FALSE)
    list_lt_tmax_median[[name_cur]] <- tab_cur
}    

### Save to excel ----
sheet_names_lt_tmax_median <- names(model_int_all[index_lt_tmax_median])
write.xlsx(list_lt_tmax_median, here(path_out, "multcomp-cis-lt_tmax_median.xlsx"), sheetName = sheet_names_lt_tmax_median)
