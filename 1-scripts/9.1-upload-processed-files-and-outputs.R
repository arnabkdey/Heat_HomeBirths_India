library(googledrive)
library(here)

# Function to upload files ----------------------------------------------------
func_upload <- function(file_list, folder_id) {
  for (file in file_list) {
    ## Get the file name
    file_name <- basename(file)
    
    ## Upload the file
    drive_upload(media = file, 
                 name = file_name, 
                 path = as_id(folder_id),
                 overwrite = TRUE)
  }
}

# Upload all processed data to Google Drive -----------------------------------
## Get ID of the folder to upload to
folder_id_proc <- "1nVfhyjylSpuWUf6ZiUyilHMK8cGA8-a3"

## List all processed files
file_list_proc <- list.files(here("2-data", "2.2-processed-data"), full.names = TRUE)

## Run the function to upload all files
func_upload(file_list_proc, folder_id_proc)

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

## List all outputs
### Main Outputs
file_list_out_desc <- list.files(here("3-outputs", "descriptives"), full.names = TRUE)
file_list_out_fig <- list.files(here("3-outputs", "figures"), full.names = TRUE)
file_list_out_model_no_int <- list.files(here("3-outputs", "models", "models-no-interaction"), full.names = TRUE)
file_list_out_model_int <- list.files(here("3-outputs", "models", "models-with-interaction"), full.names = TRUE)

### Supplementary Outputs
file_list_supp_fig <- list.files(here("3-outputs", "supplements", "figures"), full.names = TRUE)
file_list_supp_model_no_int <- list.files(here("3-outputs", "supplements", "models", "models-no-interaction"), full.names = TRUE)
file_list_supp_model_int <- list.files(here("3-outputs", "supplements", "models", "models-with-interaction"), full.names = TRUE)

## Run the function to upload all files
### Main Outputs
func_upload(file_list_out_desc, folder_id_outputs_desc)
func_upload(file_list_out_fig, folder_id_outputs_fig)
func_upload(file_list_out_model_no_int, folder_id_outputs_model_no_int)
func_upload(file_list_out_model_int, folder_id_outputs_model_int)

### Supplementary Outputs
func_upload(file_list_supp_fig, folder_id_supp_fig)
func_upload(file_list_supp_model_no_int, folder_id_supp_model_no_int)
func_upload(file_list_supp_model_int, folder_id_supp_model_with_int)

