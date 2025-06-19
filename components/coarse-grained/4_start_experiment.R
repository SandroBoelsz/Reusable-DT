# Three Start beehave experiments
# ---
# NaaVRE:
#  cell:
#   inputs:
#    - netlogo_output: List
#    - input_files: List
#
#   outputs:
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
#    - name: readr
#    - name: purrr
#    - name: dplyr
#    - name: stringr
#    - name: XML
# ...

library(readr)
library(purrr)
library(stringr)
library(stats)
library(dplyr)
library(XML)

user_params <- netlogo_output

if (!is.null(user_params$variables$HoneyHarvesting)) {
  user_params$variables$HoneyHarvesting <- ifelse(user_params$variables$HoneyHarvesting == 0, "false", "true")
}
if (!is.null(user_params$variables$VarroaTreatment)) {
  user_params$variables$VarroaTreatment <- ifelse(user_params$variables$VarroaTreatment == 0, "false", "true")
}
if (!is.null(user_params$variables$DroneBroodRemoval)) {
  user_params$variables$DroneBroodRemoval <- ifelse(user_params$variables$DroneBroodRemoval == 0, "false", "true")
}

# extract input file and weather file paths from input_files
input_file <- user_params$input_file
weather_file <- user_params$weather_file

stopifnot(file.exists(input_file))
stopifnot(file.exists(weather_file))

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

  model_path <- gsub('^"|"$', '', model_path)
  xml_path <- gsub('^"|"$', '', xml_path)
  output_path <- gsub('^"|"$', '', output_path)

  # Print file paths to verify
  print(paste("Model Path:", model_path))
  print(paste("XML Path:", xml_path))
  print(paste("Output Path:", output_path))


  # Retrieve the NetLogo JAR path from the environment variable
  netlogo_jar_path <- Sys.getenv("NETLOGO_JAR")

  # Ensure the variable is not empty
  if (netlogo_jar_path == "") {
    stop("Environment variable NETLOGO_JAR is not set.")
  }

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

model_path <- paste0(param_input_dir, param_model)

results <- run_simulation(
  model_path,
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

output_file <- user_params$outpath