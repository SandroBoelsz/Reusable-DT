box::use(
  readr[read_delim, read_csv, col_character, col_double, col_skip, col_integer],
  purrr[map]
)

box::use(
  terra[
    rast, vect, project, crs, buffer, crop, classify, cats, set.values, cells, voronoi, crds
  ],
  sf[
    st_as_sf, st_sample, st_sf, st_coordinates, st_is_valid, st_make_valid, st_intersection
  ],
  dplyr[
    mutate, filter, select, left_join, pull, rename, slice, n
  ],
  purrr[
    map_chr
  ],
  stringr[
    str_replace_all
  ],
  rdwd[
    nearbyStations, dataDWD
  ],
  lubridate[
    as_date
  ]
)

box::use(
  terra[crs, buffer, crop, classify, cats, set.values, cells, as.polygons, disagg, expanse, crds, distance, centroids, values, `values<-`],
  dplyr[mutate, filter, pull, left_join, rename, select, slice, n],
)
###### R - helper functions to create input (resources) files to run the BEEHAVE model
# main contributor Anna Wendt, University of Freiburg
# modifications have been done by Jürgen Groeneveld, Tomas Martinovic, Tuomas Rossi 
# the honeybee pDT has benefited from the work of the honeybee pDT team

# Import functions ----
box::use(
  terra[crs, buffer, crop, classify, cats, set.values, cells, vect, voronoi],
  sf[st_as_sf, st_sample, st_sf, st_coordinates, st_is_valid, st_make_valid, st_intersection],
  dplyr[mutate, filter, select, left_join],
  stats[kmeans],
)

box::use(
  optparse[OptionParser, add_option, parse_args],
  jsonlite[parse_json],
  purrr[map],
  stringr[str_split],
  stats[na.omit],
  dplyr[mutate],
  readr[read_file],
)

box::use(
  XML[newXMLDoc, newXMLNode, addChildren, saveXML],
  readr[read_csv, cols],
)

input_dir <- "../../../data/input" 
output_dir <- "../../../data/output" 
buffer <- 3000L
map <- "map.tif"
lookup_table <- "lookup_table.csv"
locations <- "locations.csv"
parameters <- "parameters.csv"
simulation <- "simulation.csv"

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

# # Function to create spatial variability in flowering of grassland polygons
# SpatialVaryingInput <- function(input, monthlyProb) {
#   # alter flowering time of grassland in the BeeHave input-file
#   # so that each grassland Patch is only flowering one month
#   # "monthlyProb" indicates the probability of flowering per month
#   # with numbers between 0 and 1 giving the probability of flowering
#   # of each month (length = 12)

#   # extract Patch IDs of Grassland
#   grasslandIDs <- input |>
#     filter(PatchType == "Grassland") |>
#     select("id") |>
#     unique()

#   # generate random numbers indicating at which months Patch is flowering
#   # with given probabilities per month
#   FloweringLookup <-
#     data.frame(grasslandIDs,
#       flowerMonth = sample(1:12, length(grasslandIDs),
#         prob = monthlyProb,
#         replace = TRUE
#       )
#     )

#   # join month of flowering to input file and calculate month from date
#   newInput <- left_join(input, FloweringLookup,
#     by = "id"
#   ) |>
#     mutate(month = month(as.Date(day - 1, origin = "2016-01-01")))

#   #
#   newInput <- newInput |>
#     mutate(flowering = ifelse(flowerMonth == month, TRUE, FALSE)) |>
#     mutate(
#       quantityNectar_l = ifelse(flowering == TRUE, quantityNectar_l, 0),
#       quantityPollen_g = ifelse(flowering == TRUE, quantityPollen_g, 0)
#     ) |>
#     select(-c(month, flowerMonth, flowering))

#   return(newInput)
# }

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


  # # create a dataframe for reclassification which sets all other landscapes to NA
  # lookUpValues <- data.frame(from = 0:24) %>%
  #   mutate(to = ifelse(from %in% bee_landscapes, from, NA))
  #
  # # reclassify values in clipped raster image
  # location_area <- classify(location_area, lookUpValues)
  #
  # # change values to correct categories as displayed in input map
  # # lookUpCategories <- data.frame(
  # #   value = 0:24,
  # #   category = levels(input_map)[[1]])
  #
  # lookUpCategories <- data.frame(levels(input_map)[[1]])
  #
  # # extract values present at location (in case not all values are present)
  # valuesLocation <- unique(values(location_area))
  #
  # # overwrite levels in clipped raster image to PatchType Name
  # levels(location_area) <- lookUpCategories[which(lookUpCategories$value %in%
  #                                                  valuesLocation),]

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
    select(c("id", "oldPatchID", "PatchType", "distance_m", "xcor", "ycor", "size_sqm"))

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

##### modification of the input file allowing to discriminate the background resources of grassland and resources during summer
# therefore there is GrasslandSeason type in the look up table now

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

prepare_input <- function() {  
  # Prepare file paths ----
  input_tif_path <- file.path(input_dir, map)
  input_lookup_path <- file.path(input_dir, lookup_table)
  input_locations_path <- file.path(input_dir, locations)
  input_parameters_path <- file.path(input_dir, parameters)
  input_simulation_path <- file.path(input_dir, simulation)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
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
        Value = c(paste0("\"", "/mnt/d/Repositories/School/Thesis/Reusable-DT/data/input", "/locations", "/input1.txt\""),
                  paste0("\"", "/mnt/d/Repositories/School/Thesis/Reusable-DT/data/input", "/locations", "/weather1.txt\""),
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
    buffer_size = buffer,
    sim_days = simulation_df$sim_days[1],
    start_day = simulation_df$start_day[1],
    location_path = file.path(input_dir, "locations"),
    input_tif_path = input_tif_path,
    nectar_pollen_lookup_path = input_lookup_path
  )
      
  netlogo_output <- list(
    outpath = file.path(
      output_dir,
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

  return(list(locations_output = locations_output, netlogo_output = netlogo_output))
}

#Step 3
make_nl_XML <- function(input_list){
  
 experimentXML <- newXMLDoc()

  experiment <- newXMLNode(
    "experiment",
    attrs = list(
      name = "Exp1",
      repetitions = "1",
      runMetricsEveryStep = "true"
    )
  ) 
  experiment <- experiment |>
    addChildren(
      newXMLNode(
        "setup",
        "setup",
        parent = experiment
      )
    ) |>
    addChildren(
      newXMLNode(
        "go",
        "go",
        parent = experiment
      )
    ) |>
    addChildren(
      newXMLNode(
        "timeLimit",
        attrs = c(steps = input_list$sim_days),
        parent = experiment
      )
    )
  
  for (i in seq_along(input_list$metric)) {
    experiment <- experiment |>
      addChildren(
        newXMLNode(
          "metric",
          input_list$metrics[[i]],
          parent = experiment
        )
      )
  }
  
  variables_names <- input_list$variables |>
    names()
  
  for (i in seq_along(input_list$variables)) {
    experiment <- experiment |>
      addChildren(
        newXMLNode(
          "enumeratedValueSet",
          newXMLNode(
            "value",
            attrs = c(value = input_list$variables[[i]])
          ),
          attrs = c(variable = variables_names[i]),
          parent = experiment
        )
      )
  }
  
  experiments <- newXMLNode(
    "experiments",
    experiment,
    doc = experimentXML
  )
  
  return(experiments)
}

run_simulation <- function(
    netlogo_jar_path,
    model_path,
    output_path,
    input_list,
    xml_path = NULL,
    memory = 2048,
    threads = 1
) {
  if (is.null(xml_path)) {
    xml_path <- paste0(tempfile(pattern = "netlogo_xml_"), ".xml")
  }
  
  simulation_xml <- make_nl_XML(input_list)

  saveXML(
    simulation_xml,
    file = xml_path
  )

  netlogo_jar_path <- gsub('^"|"$', '', netlogo_jar_path)
  model_path <- gsub('^"|"$', '', model_path)
  xml_path <- gsub('^"|"$', '', xml_path)
  output_path <- gsub('^"|"$', '', output_path)

  # Print file paths to verify
  print(paste("NetLogo JAR Path:", netlogo_jar_path))
  print(paste("Model Path:", model_path))
  print(paste("XML Path:", xml_path))
  print(paste("Output Path:", output_path))

  system_cmd <- paste(
    'java',
    paste0('-Xmx', memory, 'm -Dfile.encoding=UTF-8'),
    '-classpath', shQuote(netlogo_jar_path),
    'org.nlogo.headless.Main',
    '--model', shQuote(model_path),
    '--setup-file', shQuote(xml_path),
    '--experiment Exp1',
    '--table', shQuote(output_path),
    '--threads', threads
  )

  print(system_cmd)
  system(system_cmd)
  
  results <- read_csv(
    output_path,
    skip = 6,
    col_types = cols()
  )
  
  return(results)
}

# Step 2
# Define rdwd download location to reduce network load
RDWD_CACHEDIR = Sys.getenv("RDWD_CACHEDIR")
if (RDWD_CACHEDIR != "") {
  options(rdwdlocdir = RDWD_CACHEDIR)
}

# Prepare input parameters ----
args <- prepare_input()
locations_output <- args$locations_output

if (!dir.exists(locations_output$location_path)) {
  dir.create(locations_output$location_path)
}

# args should contain
# id - location ID, e.g 412
# x - latitude in EPSG:4326 CRS
# y - longitude in EPSG:4326 CRS
# buffer_size - size of the buffer around points (area size in map units, typically meters)
# location_path - path to temp directory where to store inputs for computation
# input_tif_path - path to input tif file
# nectar_pollen_lookup_path - path to lookup_table.csv

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

# Call Input generator with different patch sizes ----
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
  paste0(locations_output$location_path, "/input", locations_output$id, ".txt"),
  sep = " ",
  row.names = FALSE
)

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
    locations_output$id,
    ".txt"
  ),
  quote = F,
  row.names = F,
  col.names = F
)

inputs <- list()
inputs$netlogo_jar_path <- "/mnt/d/Repositories/School/Thesis/Reusable-DT/NetLogo6.3.0/lib/app/netlogo-6.3.0.jar"
Sys.setenv(JAVA_HOME="/usr/lib/jvm/java-17-openjdk-amd64")
inputs$model_path <- "/mnt/d/Repositories/School/Thesis/Reusable-DT/data/input/Beehave_BeeMapp2015_Netlogo6version_PolygonAggregation.nlogo"
user_params <- args$netlogo_output

#user_params$constants$WeatherFile[[1]] <- gsub('^"|"$', '', paste0(locations_output$location_path, "/weather", locations_output$id, ".txt"))
#user_params$constants$INPUT_FILE[[1]] <- gsub('^"|"$', '', paste0(locations_output$location_path, "/input", locations_output$id, ".txt"))

if (!is.null(user_params$variables$HoneyHarvesting)) {
  user_params$variables$HoneyHarvesting <- ifelse(user_params$variables$HoneyHarvesting == 0, "false", "true")
}
if (!is.null(user_params$variables$VarroaTreatment)) {
  user_params$variables$VarroaTreatment <- ifelse(user_params$variables$VarroaTreatment == 0, "false", "true")
}
if (!is.null(user_params$variables$DroneBroodRemoval)) {
  user_params$variables$DroneBroodRemoval <- ifelse(user_params$variables$DroneBroodRemoval == 0, "false", "true")
}

# Rewrite default parameters by user defined ----
# user_params$variables <- map(user_params$variables, ~list(values = .x |> unlist() |> unname()))
input_file <- gsub('^.|.$', '', user_params$variables$INPUT_FILE)
weather_file <- gsub(
  '^.|.$', '', user_params$variables$WeatherFile
)

stopifnot(file.exists(input_file))
stopifnot(file.exists(weather_file))
# print("passed_file_check")

# Load weather data ----
# Read the content of the weather file
weather_content <- read_file(weather_file)

# Remove the square brackets and split the content into a vector of substrings based on spaces
weather_values <- str_split(gsub("\\[|\\]", "", weather_content), " ", simplify = TRUE)

# Convert the substrings to numeric, suppressing warnings about NAs
weather <- suppressWarnings(as.numeric(weather_values))

# Remove any NA values
weather <- na.omit(weather)

weather <- rep(weather, 10)
# Run experiment ----
results <- run_simulation(
  inputs$netlogo_jar_path,
  inputs$model_path,
  user_params$outpath,
  user_params
)

start_date <- user_params$start_day[[1]] |>
  as.Date()

results <- results |>
  mutate(weather = weather[1:nrow(results)],
         date = seq(from = start_date,
                    to = start_date + user_params$sim_days[[1]],
                    by = "day"))

# Store results ----
write.table(results,
            file = user_params$outpath,
            sep = ",",
            row.names = FALSE)
