# Prepare input
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - isFileDownloadSuccessful: Integer
#   outputs:
#    - locations_output: List
#    - netlogo_output: List
#   dependencies:
#    - name: readr
#    - name: purrr
#    - name: dplyr
# ...

library(readr)
library(purrr)
library(dplyr)

if (!isFileDownloadSuccessful) {
  stop("File download failed! Stopping workflow.")
} else {
  message("All files downloaded successfully. Proceeding with input preparation.")
}

input_tif_path <- file.path(param_input_dir, param_map)
input_lookup_path <- file.path(param_input_dir, param_lookup_table)
input_locations_path <- file.path(param_input_dir, param_locations)
input_parameters_path <- file.path(param_input_dir, param_parameters)
input_simulation_path <- file.path(param_input_dir, param_simulation)

if (!dir.exists(param_output_dir)) {
  dir.create(param_output_dir)
}

# Load locations ----
location <- 
  read_delim(input_locations_path,
             delim = ",",
             col_types = list(
               id = "i",
               lat = "d",
               lon = "d"
  )) |>
  slice(1)

# Load parameters ----
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
      Value = c(paste0("\"", param_input_dir, "/locations", "/input.txt\""),
                paste0("\"", param_input_dir, "/locations", "/weather.txt\""),
                sample(1:100000, 1))
    )
)

parameters_list <- parameters$Value |>
  map(~list(.x))
names(parameters_list) <- parameters$Parameter
  
# Load simulation data ----
simulation_df <- read_csv(
  file = input_simulation_path,
  col_types = list(
    sim_days = col_integer(),
    start_day = col_character()
  )
)
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
    
netlogo_output <- list(
  outpath = file.path(
    param_output_dir,
    paste0("output_id", location$id, ".csv")
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

print(locations_output)
print(netlogo_output)
