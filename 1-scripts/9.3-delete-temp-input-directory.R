# Delete the directory ./data/2.1-raw-data/
if (dir.exists("./2-data/2.1-raw-data/")) {
  # Delete the directory if it exists
  unlink("./2-data/2.1-raw-data/", recursive = TRUE)
}
