rdata_files <- list.files(pattern = "\\.RData$")

remaining_files <- rdata_files[7:length(rdata_files)]

for (f in remaining_files) {
  put_object(
    file = f, 
    object = paste0("temp/", f), 
    bucket = "nfl-pff-data-lucas",
    multipart = TRUE
  )
  cat("Uploaded:", f, "\n")
}


put_object(
  file = "~/receiving_stats_build_workspace_AWS.RData",
  object = "temp/receiving_stats_build_workspace_AWS.RData",
  bucket = "nfl-pff-data-lucas"
)
