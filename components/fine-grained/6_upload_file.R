# Component 6 upload files to S3
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - output_file: String
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

# Extract the filename from output_file path
filename <- basename(output_file)

# Construct the S3 location
s3_location <- paste0(param_s3_user_prefix, "/output/", filename)

# Function to upload file to S3
upload_file <- function(local_path, s3_path, bucket) {
  tryCatch({
    put_object(file = local_path, 
               object = s3_path, 
               bucket = bucket)
    
    message(paste("Uploaded:", local_path, "to S3:", s3_path))
    return(TRUE)
  }, error = function(e) {
    warning(paste("Failed to upload:", local_path, "Error:", e$message))
    return(FALSE)
  })
}

# Upload the file
upload_successful <- upload_file(output_file, s3_location, param_s3_bucket)

if (upload_successful) {
  message("File successfully uploaded to S3!")
} else {
  stop("File upload failed. Exiting with error.")
  exit 
}