import_other_factors <- function(filenames) {
  other_factors <- list()
  
  # Extract unique COIs from filenames
  coi_ids <- unique(sub(".*(/COI \\d+)_Operation \\d+$", "\\1", filenames))
  
  for (coi_id in coi_ids) {
    operation_files <- filenames[grepl(coi_id, filenames)]
      # Extract just the COI name ("COI x") from the coi_id
      coi_name <- sub(".*/(COI \\d+)$", "\\1", coi_id)
      
      other_factors[[coi_name]] <- list()
      for(file in operation_files) {
        dt <- as.data.frame(read.csv(file))
          operation_id <- sub("^.*_(Operation \\d+)$", "\\1", file)  
          other_factors[[coi_name]][[operation_id]] <- dt
        
      }
      
    
  }
  
  other_factors
}


