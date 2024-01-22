inputFLs <- function(filenames) {
  test <- list()
  
  # Extract unique COIs from filenames
  coi_ids <- unique(sub(".*(/COI \\d+)_Operation \\d+$", "\\1", filenames))
  
  for (coi_id in coi_ids) {
    operation_files <- filenames[grepl(coi_id, filenames)]
    
    if (length(operation_files) > 0) {
      # Extract just the COI name ("COI x") from the coi_id
      coi_name <- sub(".*/(COI \\d+)$", "\\1", coi_id)
      
      test[[coi_name]] <- list()
      for(file in operation_files) {
        dt <- read_csv(file)
        if (nrow(dt) > 0) {
          dt <- as.data.frame(unclass(dt), stringsAsFactors=TRUE)
          # Extract Operationx from filenames
          operation_id <- sub("^.*_(Operation \\d+)$", "\\1", file)  
          test[[coi_name]][[operation_id]] <- drop_na(expand.grid(dt))
        }
      }
      test[[coi_name]] <- Filter(Negate(is.null), test[[coi_name]])  # remove NULLs
    }
  }
  
  test
}






