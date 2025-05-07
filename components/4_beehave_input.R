# Three Prepare beehave model input
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - locations_output: List
#   outputs:
#    - input_file: String
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
#    - name: sf
#    - name: dplyr
#    - name: lubridate
#    - name: raster
#    - name: Rcpp
# ...

library(terra)
library(sf)
library(dplyr)
library(lubridate)

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