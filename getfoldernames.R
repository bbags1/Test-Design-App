get_folder_names <- function(bucket, folder){
  contents = aws.s3::get_bucket_df(bucket = bucket, prefix = folder, delimeter = "/")
  folders = gsub(folder, "", contents$Key)
  names = unique(gsub("/.*$", "", folders))
  return(names)
}




 