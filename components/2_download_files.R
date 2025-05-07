# One Download files from S3
# ---
# NaaVRE:
#  cell:
#   outputs:
#    - is_file_download_succesful: Integer
#   params:
#    - param_s3_region:
#       type: String
#       default_value: "nl-uvalight"
#    - param_s3_endpoint:
#       type: String
#       default_value: "scruffy.lab.uvalight.net:9000"
#    - param_s3_user_prefix:
#       type: String
#       default_value: "sandro.boelsz@student.uva.nl"
#    - param_s3_bucket:
#       type: String
#       default_value: "naa-vre-user-data"
#    - param_input_dir:
#       type: String
#       default_value: "/tmp/data/input/"
#    - param_output_dir:
#       type: String
#       default_value: "/tmp/data/output/"
#    - param_buffer:
#       type: Integer
#       default_value: 3000
#    - param_map:
#       type: String
#       default_value: "map.tif"
#    - param_map_aux:
#       type: String
#       default_value: "map.tif.aux.xml"
#    - param_lookup_table:
#       type: String
#       default_value: "lookup_table.csv"
#    - param_locations:
#       type: String
#       default_value: "locations.csv"
#    - param_parameters:
#       type: String
#       default_value: "parameters.csv"
#    - param_simulation:
#       type: String
#       default_value: "simulation.csv"
#    - param_model:
#       type: String
#       default_value: "Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"
#   secrets:
#    - secret_s3_access_id:
#       type: String
#    - secret_s3_secret_key:
#       type: String
#   dependencies:
#    - name: aws.s3
# ...

library(aws.s3)
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = secret_s3_access_id,
  "AWS_SECRET_ACCESS_KEY" = secret_s3_secret_key,
  "AWS_DEFAULT_REGION" = param_s3_region,
  "AWS_S3_ENDPOINT" = param_s3_endpoint,
  "AWS_S3_VERIFY" = "FALSE"
)

# Define filenames and their S3 paths
filenames <- c(param_map, param_map_aux, param_lookup_table, param_locations, param_parameters, param_simulation, param_model)
s3_locations <- paste0(param_s3_user_prefix, "/input/", filenames)
download_locations <- paste0(param_input_dir, filenames)

# Function to download files from MinIO
download_file <- function(s3_path, local_path, bucket) {
  tryCatch({
    save_object(object = s3_path, 
                bucket = bucket, 
                file = local_path)
    
    if (file.exists(local_path)) {
      message(paste("Downloaded:", s3_path, "to", local_path))
      return(TRUE)
    } else {
      warning(paste("File not found after download attempt:", s3_path))
      return(FALSE)
    }
  }, error = function(e) {
    warning(paste("Failed to download:", s3_path, "Error:", e$message))
      return(FALSE)
  })
}

# Loop through files and download each one
results <- mapply(download_file, s3_locations, download_locations, param_s3_bucket)

is_file_download_succesful <- as.integer(all(results))
if (is_file_download_succesful == 1) {
  message("All files downloaded successfully!")
} else {
  warning("Some files failed to download.")
  stop("Some files failed to download.")
}