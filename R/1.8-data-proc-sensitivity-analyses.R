
# load libraries ----
rm(list = ls())
pacman::p_load(dplyr, janitor, data.table, fst, openxlsx, here)

# set paths ----
source(here("paths-mac.R"))
path_processed <- here(path_project, "data", "processed-data")

# load datasets ------
df_paper_final <- readRDS(here(path_processed, "1.7-final-data-for-paper.rds"))
nrow(df_paper_final) # 198,126

tabyl(df_paper_final$midx)

# Create dataset for sensitivity analysis -------------------------------------------
## First identify cases to be dropped for sensitivity analysis
tabyl(df_paper_final$mat_emp_healthcare)
tabyl(df_paper_final, m65a) # 51,109 cases missing
tabyl(df_paper_final, m14) # 51,109 cases missing
tabyl(df_paper_final, midx) |> adorn_totals() # 198,126 total cases and 147,017 cases where midx == 1; accounting for 51,109 cases where midx > 1 (missing in m65a and m14)

# Note: we see that variables related to anc and decision to not deliver at healthcare are missing for cases where midx > 1. 
# These questions were asked only for the last child born to the mother before the survey.
# We will drop these cases for sensitivity analysis.

## create a data subset for sensitivity analysis
df_paper_final_sens <- df_paper_final |> filter(midx == 1)
tabyl(df_paper_final_sens, midx) |> adorn_totals()
tabyl(df_paper_final_sens, m65a) |> adorn_totals()
tabyl(df_paper_final_sens, m14) |> adorn_totals()
tabyl(df_paper_final_sens, mat_emp_healthcare) |> adorn_totals() # 125,021 cases still missing

## Create variable for ANC visits
df_paper_final_sens <- df_paper_final_sens |>
  ### Create variable for ANC visits
  dplyr::mutate(four_anc_dk = case_when(
    str_detect(m14, "no antenatal visits") ~ "no_anc",
    str_detect(m14, "don't know") ~ "don't know",
    str_detect(m14, "1") ~ "less_than_four",
    str_detect(m14, "2") ~ "less_than_four",
    str_detect(m14, "3") ~ "less_than_four",
    TRUE ~ "four_or_more")) |> 
dplyr::mutate(mat_four_anc = ifelse(four_anc_dk == "four_or_more", 1, 0)) 

tabyl(df_paper_final_sens, mat_four_anc)

## Reasons for not delivering in institution -----------------------
tabyl(df_paper_final_sens, m65h) |> adorn_totals()

df_paper_final_sens <- df_paper_final_sens |>
  rename(
    mat_reason_hb_cost = m65a,
    mat_reason_hb_fac_not_open = m65b,
    mat_reason_hb_fac_far = m65c,
    mat_reason_hb_poor_service = m65d,
    mat_reason_hb_no_female_prov = m65e,
    mat_reason_hb_husb_didnt_allow = m65f,
    mat_reason_hb_not_necessary = m65g,
    mat_reason_hb_not_customary = m65h
  )

# Save the dataset for sensitivity analysis
df_paper_final_sens |> write_fst(here(path_processed, "1.8-final-data-for-paper-sens.fst"))