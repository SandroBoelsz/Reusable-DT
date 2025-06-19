# Three Create weather input
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - locations_output: List
#   outputs:
#    - weather_file: String
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
#    - name: terra
#    - name: dplyr
#    - name: purrr
#    - name: lubridate
#    - name: raster
#    - name: Rcpp
#    - name: stringr
# ...

library(terra)
library(dplyr)
library(purrr)
library(stringr)
library(rdwd)
library(lubridate)

###### R - script to create input (resources and weather) files to run the BEEHAVE model
# main contributor Anna Wendt, University of Freiburg
# contributor of an earlier version of the WeatherDataInput() function Okan Özsoy
# modifications have been done by Jürgen Groeneveld, Tomas Martinovic, Tuomas Rossi
# the honeybee pDT has benefited from the work of the honeybee pDT team

# Function to create a weatherinput file for the beehave model
weather_data_input <- function(bee_location,
                               from_date = "2016-01-01",
                               to_date = "2016-12-31") {
  # transform input coordinates to degrees
  TrachtnetConv <- project(bee_location, "epsg:4326")
  Coordinates <- as.data.frame(crds(TrachtnetConv))
  
  # Read the station data
  WeatherStations <- nearbyStations(
    Coordinates$y,
    Coordinates$x,
    radius = 50,
    res = "daily", var = "kl", per = "historical", mindate = to_date
  ) |>
    # select only stations that started measuring before 2016
    filter(von_datum < from_date) |>
    # change urls to https
    mutate(
      url = map_chr(url,
      function(x){str_replace_all(x, "ftp://", "https://")})) 
  
  # check through the stations for NA values in data
  for (i in 1:nrow(WeatherStations)) {
    weather_data <- dataDWD(WeatherStations$url[i], varnames = TRUE, quiet = TRUE) |>
      select(MESS_DATUM,
             SDK.Sonnenscheindauer,
             TXK.Lufttemperatur_Max) |>
      # mutate(MESS_DATUM = as_date(MESS_DATUM)) |>
      filter(MESS_DATUM >= as.POSIXct(from_date, tz = "GMT"),
             MESS_DATUM <= as.POSIXct(to_date, tz = "GMT")) 

    # breaks when file with no NAs in SDK found
    if (anyNA(weather_data$SDK.Sonnenscheindauer) == FALSE & length(weather_data$SDK.Sonnenscheindauer) > 0) break
    
    # if all stations contain NA values give warning
    if (i == length(WeatherStations$Stations_id)) {
      warning(paste("Final selected weather station includes NA values. No stations found without any NA within 50km distance. Station ID:", WeatherStations$Stations_id[i]))
    }
  }
  
  # Add station id and day number
  weather_data <- weather_data |>
    rename(Date = MESS_DATUM,
           T_max = TXK.Lufttemperatur_Max,
           Sun_hours = SDK.Sonnenscheindauer) |>
    mutate(Station_id = WeatherStations$Stations_id[i],
           Day = 1:n(),
           .before = Date) |>
    # Use only sun hours where max temperature is above 15 degrees celsium
    mutate(Sun_hours = ifelse(T_max < 15, 0, Sun_hours))
  
  # create vector for BEEHAVE weather input with sunshine hours
  weather_file <- paste("[", paste(weather_data$Sun_hours, collapse = " "), "]")
  
  return(list(weather_data, weather_file))
}

input_map <-
  rast(locations_output$input_tif_path)

bee_location <- vect(
  data.frame(
    id = locations_output$id,
    lon = locations_output$lon,
    lat = locations_output$lat
  ),
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
) |>
  project(input_map)

# Create weather input for beehave and write file ----
to_date <- locations_output$start_day |>
  as.Date() + locations_output$sim_days

WeatherOutput <- weather_data_input(bee_location,
                                    from_date = locations_output$start_day,
                                    to_date = to_date)

write.table(
  WeatherOutput[2],
  paste0(
    locations_output$location_path,
    "/weather",
    ".txt"
  ),
  quote = F,
  row.names = F,
  col.names = F
)

weather_file <- paste0(locations_output$location_path, "/weather.txt")