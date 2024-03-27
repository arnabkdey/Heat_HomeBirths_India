# Delete the directory ./data/gdrive_temp_files_input/
if (dir.exists("./data/gdrive_temp_files_input/")) {
  # Delete the directory if it exists
  unlink("./data/gdrive_temp_files_input/", recursive = TRUE)
}
