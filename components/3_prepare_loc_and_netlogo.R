# Two Prepare locations and netlogo input
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - is_file_download_succesful: Integer
#   outputs:
#    - locations_output: List
#    - netlogo_output: List
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
#    - name: readr
#    - name: purrr
#    - name: dplyr
# ...

library(readr)   # For reading CSV and delimited files
library(purrr)   # For functional programming tools (e.g., map)
library(dplyr)   # For data manipulation

# Check if file download was successful, stop workflow if not
if (!is_file_download_succesful) {
  stop("File download failed! Stopping workflow.")
} else {
  message("All files downloaded successfully. Proceeding with input preparation.")
}

# Construct input file paths based on parameters
input_tif_path <- file.path(param_input_dir, param_map)
input_lookup_path <- file.path(param_input_dir, param_lookup_table)
input_locations_path <- file.path(param_input_dir, param_locations)
input_parameters_path <- file.path(param_input_dir, param_parameters)
input_simulation_path <- file.path(param_input_dir, param_simulation)

# Ensure output directory exists
if (!dir.exists(param_output_dir)) {
  dir.create(param_output_dir)
}

# Load the first location from the locations file
location <- 
  read_delim(input_locations_path,
             delim = ",",
             col_types = list(
               id = "i",
               lat = "d",
               lon = "d"
  )) |>
  slice(1)  # Only use the first row

# Load parameters from the parameters file and append NetLogo-specific parameters
parameters <- 
  read_csv(
    file = input_parameters_path,
    col_types = list(
      Parameter = col_character(),
      Value = col_double(),
      `Default.Value` = col_skip()
    )
  ) |>
  rbind(
    data.frame(
      Parameter = c("INPUT_FILE", "WeatherFile", "random-seed"),
      Value = c(paste0("\"", param_input_dir, "locations", "/input.txt\""),
                paste0("\"", param_input_dir, "locations", "/weather.txt\""),
                sample(1:100000, 1))
    )
)

# Convert parameters to a named list for NetLogo input
parameters_list <- parameters$Value |>
  map(~list(.x))
names(parameters_list) <- parameters$Parameter

# Load simulation data (simulation days and start day)
simulation_df <- read_csv(
  file = input_simulation_path,
  col_types = list(
    sim_days = col_integer(),
    start_day = col_character()
  )
)

# Prepare output list for locations
locations_output <- list(
  id = location$id,
  lat = location$lat,
  lon = location$lon,
  buffer_size = param_buffer,
  sim_days = simulation_df$sim_days[1],
  start_day = simulation_df$start_day[1],
  location_path = file.path(param_input_dir, "locations"),
  input_tif_path = input_tif_path,
  nectar_pollen_lookup_path = input_lookup_path
)

# Prepare output list for NetLogo simulation
netlogo_output <- list(
  outpath = file.path(
    param_output_dir,
    paste0("output.csv")
  ),
  metrics = c(
    "TotalIHbees + TotalForagers",
    "(honeyEnergyStore / ( ENERGY_HONEY_per_g * 1000 ))",
    "PollenStore_g"
  ),
  variables = parameters_list,
  sim_days = simulation_df$sim_days[1],
  start_day = simulation_df$start_day[1]
)