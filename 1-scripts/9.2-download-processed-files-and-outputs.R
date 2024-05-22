library(googledrive)
library(here)

# Function to upload files ----------------------------------------------------
func_download <- function(folder_id, path_dest_folder) {
    ## List all files in the folder
    file_list <- googledrive::drive_ls(as_id(folder_id))
    ## Download all files to the local folder
    for (i in seq_len(nrow(file_list))) {
        cur_file <- file_list[i, ]
    googledrive::drive_download(as_id(cur_file$id), path = here(path_dest_folder, cur_file$name))}
}

# Upload all processed data to Google Drive -----------------------------------
## Get ID of the google drive folder to download from
folder_id_proc <- "1nVfhyjylSpuWUf6ZiUyilHMK8cGA8-a3"

## Specify the destination folder
path_dest_proc <- here("2-data", "2.2-processed-data")

## Run the function to upload all files
func_download(folder_id_proc, path_dest_proc)

# Upload all outputs to Google Drive -------------------------------------------
## Get ID of the folder to upload to
### Main Outputs
folder_id_outputs_desc <- "1jkTEvAvAJ3MW93NNFcfZvkxaTeuHbSLl"
folder_id_outputs_fig <- "1umMP3JSNm3zySfAHUlAnfBX3XGqqKj5y"
folder_id_outputs_model_no_int <- "14Sh39ZaZVEiYAmMy8RtbgCmtIp0vNyEj"
folder_id_outputs_model_int <- "1Zn7pkas7TnZexwqB3RmszMHdybJkcmOm"
### Supplementary Outputs
folder_id_supp_fig <- "15v1Bzut5hVZbPRxrZ8cfnhA9P8PaFJr_"
folder_id_supp_model_no_int <- "1-pQO3hfLkyO3268YV8kXvwQh6nRLOcOe"
folder_id_supp_model_with_int <- "1JLIkzpCyJ8ckvec7QO0x-3f2qKP9q99H"

## Specify the destination folders
### Main Outputs
path_dest_fig <- here("3-outputs", "figures")
path_dest_model_no_int <- here("3-outputs", "models", "models-no-interaction")
path_dest_model_int <- here("3-outputs", "models", "models-with-interaction")
### Supplementary Outputs
path_dest_supp_fig <- here("3-outputs", "supplements", "figures")
path_dest_supp_model_no_int <- here("3-outputs", "supplements", "models", "models-no-interaction")
path_dest_supp_model_int <- here("3-outputs", "supplements", "models", "models-with-interaction")

## Run the function to upload all files
### Main Outputs
func_download(folder_id_outputs_desc, path_dest_fig)
func_download(folder_id_outputs_fig, path_dest_fig)
func_download(folder_id_outputs_model_no_int, path_dest_model_no_int)
func_download(folder_id_outputs_model_int, path_dest_model_int)
### Supplementary Outputs
func_download(folder_id_supp_fig, path_dest_supp_fig)
func_download(folder_id_supp_model_no_int, path_dest_supp_model_no_int)
func_download(folder_id_supp_model_with_int, path_dest_supp_model_int)

