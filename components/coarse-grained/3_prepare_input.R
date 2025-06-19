# Two Prepare input
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - is_file_download_succesful: Integer
#   outputs:
#    - input_files: List
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
#    - name: terra
#    - name: sf
#    - name: lubridate
#    - name: raster
#    - name: Rcpp
#    - name: stringr
# ...

library(readr)   # For reading CSV and delimited files
library(purrr)   # For functional programming tools (e.g., map)
library(dplyr)   # For data manipulation
library(stringr)
library(rdwd)
library(terra)
library(sf)
library(lubridate)

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


if (!dir.exists(locations_output$location_path)) {
  dir.create(locations_output$location_path)
}

# Landscape Classification Map ----
stopifnot(file.exists(locations_output$input_tif_path))
stopifnot(file.exists(paste0(locations_output$input_tif_path, ".aux.xml")))

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

lookup_table <- read.csv(locations_output$nectar_pollen_lookup_path)

extract_list <- function(x) {
  x[[1]]
}
# Function to split polygons using regular point clusters and voronoi-polygons
SplitPolygonsEqual <- function(polygon, polygon_size, density = 1000, RefCRS) {
  # split polygon in smaller polygons with +/- equal size
  # using regular points and voronoi polygons
  # with size indicating the target size of the polygons
  # and density altering the point density generated per polygon

  # transform spatvector to sf-polygon
  polygon <- st_as_sf(polygon, crs = RefCRS)

  # calculate number of points
  nPoints <- floor(polygon$size_sqm/density)
  # and create regular points covering polygon
  points <- st_sample(polygon, size = nPoints, type = "regular") |>
    st_sf()

  # calculate amounts of polygons to be split into
  nPolys <- ceiling(polygon$size_sqm/polygon_size)
  # create clusters using kmeans
  pointClusters <- st_coordinates(points) |>
    kmeans(nPolys)

  # extract centroids for voronoi polygons
  centroids <- data.frame(pointClusters$centers) |>
    vect(geom = c("X", "Y"), crs = RefCRS)

  # create voronoi polygons
  voronoiPoly <- voronoi(centroids, polygon) |>
    st_as_sf(crs = RefCRS)

  # check validity of polygon and correct it if needed
  if (st_is_valid(polygon) == FALSE) {
    polygon <- st_make_valid(polygon)
  }

  # clip voronoi polygons to input polygon
  polyFinal <- st_intersection(polygon, voronoiPoly) |>
    vect()

  return(polyFinal)
}

#### BeeHave Input-File Generator #####
# Function to generate Input Files for BEEHAVE-Model
# based on the landscape classification map by Preidl et al. (2020)
#
# Input of function: Raster Image as SpatRaster and
#                    Location of Beehave as SpatVector
#                    Nectar and Pollen Data
# (using package terra, sf, dplyr, lubridate and rdwd)
beehave_input <- function(input_map,
                          bee_location,
                          lookup_table,
                          polygon_size = 200000,
                          buffer_size = 3000,
                          beehave_levels = c(
                            "Maize",
                            "Legumes",
                            "Rapeseed",
                            "Strawberries",
                            "Stone Fruits",
                            "Vines",
                            "Asparagus",
                            "Grassland"
                          )) {
  ## 01 clip map to location and relevant crop types ##
  # extract Coordinate Reference System
  RefCRS <- crs(input_map, parse = FALSE)

  # create buffer around Beehave Locations...
  clip_buffer <- buffer(bee_location, width = buffer_size)

  # ... and clip raster to buffer
  location_area <- crop(input_map, clip_buffer)

  # to select only bee-relevant landscape types, remaining values are set to NA
  # first extract Values of bee-relevant landscapes
  bee_landscapes <- location_area |>
    cats() |>
    extract_list() |>
    filter(category %in% beehave_levels) |>
    pull(value)

  # Change values that are not beehave_levels to NA, this keeps levels unlike reclassify used previously
  set.values(location_area, cells(location_area, setdiff(0:24, bee_landscapes)) |> unlist(), NA)

  ## 02 create polygons and add first attributes ##
  # transform raster to polygons and disaggregate into multipolygon object
  location_area_poly <- as.polygons(location_area) |>
    disagg()

  # add attributes (id, area) to polygons
  values(location_area_poly) <- values(location_area_poly) |>
    mutate(id = 1:n(), .before = category) |>
    mutate(size_sqm = expanse(location_area_poly)) |>
    rename(PatchType = category)

  ## 03 split big polygons into multiple smaller ones ##
  # select polygons bigger than threshold (default: > 20ha)
  PolySelection <- subset(
    location_area_poly,
    location_area_poly$size_sqm > polygon_size
  )

  # exclude polygons from original vector
  # to be able to combine it later again with split up polygons
  location_area_poly_sub <- subset(
    location_area_poly,
    location_area_poly$size_sqm < polygon_size
  )

  # loop over polygons to split them up separately
  for (i in PolySelection$id) {
    # first select polygon
    split_polygon <- PolySelection |>
      subset(PolySelection$id == i)

    # use helper function to split polygons
    split_polygon <- SplitPolygonsEqual(split_polygon,
      polygon_size = polygon_size,
      RefCRS = RefCRS
    )

    # rejoin split up polygons to the rest
    location_area_poly_sub <- rbind(location_area_poly_sub, split_polygon)
  }

  # overwrite polygons with new splitted polygons
  location_area_poly <- location_area_poly_sub

  ## 04 update geographical attributes
  coordsPolys <- crds(centroids(location_area_poly))
  coordsBees <- crds(bee_location)

  # add running id, oldPatchID, calculate polygon size and distance to beehave
  # calculate centroid location with beehave as reference = (0,0)
  LocationAttributes <- values(location_area_poly) |>
    mutate(
      id = 1:n() - 1,
      oldPatchID = id, .before = PatchType,
      size_sqm = round(expanse(location_area_poly)),
      distance_m = round(as.vector(distance(bee_location, location_area_poly))),
      xcor = round(coordsPolys[, 1] - coordsBees[, 1]),
      ycor = round(coordsPolys[, 2] - coordsBees[, 2])
    ) |>
    dplyr::select(c("id", "oldPatchID", "PatchType", "distance_m", "xcor", "ycor", "size_sqm"))

  values(location_area_poly) <- LocationAttributes

  ## 05 add nectar and pollen information and create input file
  # transform geographic information to daily format
  # and add nectar and pollen information
  InputTable <- data.frame(LocationAttributes) |>
    slice(rep(1:n(), each = 365)) |>
    mutate(day = rep(1:365, nrow(LocationAttributes)), .before = id) |>
    left_join(lookup_table[, -c(7:8)], by = "PatchType") |>
    # calculate detection probability and set modelled detection prob. to 0
    mutate(
      calculatedDetectionProb_per_trip = exp(-0.00073 * distance_m),
      modelledDetectionProb_per_trip = 0, .before = nectarGathering_s
    ) |>
    # calculate nectar and pollen quantity according to patch size
    # set distance of polygon containing beehave to 0.1 meter
    mutate(
      quantityNectar_l = quantityNectar_l * size_sqm,
      quantityPollen_g = quantityPollen_g * size_sqm,
      distance_m = ifelse(distance_m == 0, 0.1, distance_m)
    )

  # reduce nectar and pollen availability to flowering days
  # with flowering information provided in lookup_table
  for (i in 1:nrow(lookup_table)) {
    flowerStart <- lookup_table$flowerStart[i]
    flowerEnd <- lookup_table$flowerEnd[i]
    Patch <- lookup_table$PatchType[i]

    InputTable[which(InputTable$PatchType == Patch), ] <-
      InputTable[which(InputTable$PatchType == Patch), ] |>
      mutate(
        quantityNectar_l = ifelse(day >= flowerStart & day <= flowerEnd,
          quantityNectar_l, 0
        ),
        quantityPollen_g = ifelse(day >= flowerStart & day <= flowerEnd,
          quantityPollen_g, 0
        )
      )
  }
  
  # InputTable <- InputTable |>
  #   filter(distance_m < buffer_size )
  # return both Input File and Polygons
  return(list(InputTable, location_area_poly))
}

modify_input_file <- function(input, lookup_table) {
  # Convert input to a data frame if it is a list
  if (is.list(input)) {
    input <- as.data.frame(input)
  }
  
  temp_start <- lookup_table[which(lookup_table$PatchType == "GrasslandSeason"), 7]
  temp_end <- lookup_table[which(lookup_table$PatchType == "GrasslandSeason"), 8]
  
  if (length(temp_start) == 0 | length(temp_end) == 0) {
    return(input)
  }
  
  temp_old_pollen <- lookup_table[which(lookup_table$PatchType == "Grassland"), 2]
  temp_season_pollen <- lookup_table[which(lookup_table$PatchType == "GrasslandSeason"), 2]

  index <- which(input$PatchType == "Grassland" & input$day > temp_start & input$day < temp_end)

  if (length(index) > 0) {
    input[index, ]$quantityPollen_g <- input[index, ]$quantityPollen_g/temp_old_pollen * temp_season_pollen
    temp_old_nectar <- lookup_table[which(lookup_table$PatchType == "Grassland"), 4]
    temp_season_nectar <- lookup_table[which(lookup_table$PatchType == "GrasslandSeason"), 4]
    input[index, ]$quantityNectar_l <- input[index, ]$quantityNectar_l/temp_old_nectar * temp_season_nectar
  }
  
  return(input)
}

input_patches <-
  beehave_input(input_map = input_map,
               bee_location = bee_location,
               lookup_table = lookup_table,
               polygon_size = 200000,
               buffer_size = locations_output$buffer_size)[[1]]

# allows to discriminate nectar and pollen resources from grassland during summer (patchtype = "Season") and the rest of the year (patchtype = "GrasslandRest")
input_patches_modified <- modify_input_file(input_patches, lookup_table)

# Write files ----
write.table(
  input_patches_modified,
  paste0(locations_output$location_path, "/input.txt"),
  sep = " ",
  row.names = FALSE
)

input_file <- paste0(locations_output$location_path, "/input.txt")

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

# Prepare output list for locations
input_files <- c(locations_output, list(
    input_file = input_file,
    weather_file = weather_file
))