library(googledrive)

# Upload all processed data to Google Drive
## Get ID of the folder to upload to
folder_id_upload <- "1GYSlxfAmLWQzzu3vLCkW2gcPwUU6A9xD"

## List all processed files
file_list <- list.files("./2-data/2.2-processed-data/", full.names = TRUE)

## Write a loop to upload all files 
for (file in file_list) {
  ## Get the file name
  file_name <- basename(file)
  
  ## Upload the file
  drive_upload(media = file, 
               name = file_name, 
               path = as_id(folder_id_upload),
               overwrite = TRUE)
}

