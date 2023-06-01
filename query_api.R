library(httr)

## APPEL DE l'API POUR UNE SEULE LIASSE ####

# Function to query the API
query_api <- function(username, password, text_feature, type_liasse=NULL, nature=NULL, surface=NULL, event=NULL, nb_echos_max=5, prob_min=0.01) {
  base_url <- "https://codification-ape.lab.sspcloud.fr/predict"
  
  # Construct the parameters for the API request
  params <- list(
    text_feature = text_feature,
    type_liasse = type_liasse,
    nature = nature,
    surface = surface,
    event = event,
    nb_echos_max = nb_echos_max,
    prob_min = prob_min
  )
  
  # Remove parameters with NULL values
  params <- params[!sapply(params, is.null)]
  
  # Construct the URL with the parameters
  url <- modify_url(base_url, query = params)
  
  # Send GET request to the API with authentication
  response <- GET(url, authenticate(username, password))
  
  # Check the response status code
  if (status_code(response) == 200) {
    # Return the parsed JSON content of the response
    return(content(response, "parsed"))
  } else if (status_code(response) == 400) {
    # If the status code is 400, print the error detail
    error_detail <- content(response, "parsed")$detail
    print(error_detail)
  } else {
    # If there is an error, print a generic error message and return NULL
    print("Error occurred while querying the API.")
    return(NULL)
  }
}

# Set the authentication credentials and query parameters
username <- "username"
password <- "password"
text_feature <- "LOUEUR MEUBLE NON PROFESSIONNEL EN RESIDENCE DE SERVICES (CODE APE 6820A Location de logements)"
type_liasse <- "I"
nature <- NULL
surface <- NULL
event <- "01P"
nb_echos_max <- 5
prob_min <- 0.01

# Call the query_api function with the provided parameters
result <- query_api(username, password, text_feature, type_liasse, nature, surface, event, nb_echos_max, prob_min)

# Print the result
result


## APPEL DE l'API POUR UN BATCH DE LIASSES ####

# Function to query the API in batch mode
query_batch_api <- function(username, password, data, nb_echos_max=5, prob_min=0.01) {
  base_url <- "https://codification-ape.lab.sspcloud.fr/predict-batch"
  
  # Construct the parameters for the API request
  params <- list(
    nb_echos_max = nb_echos_max,
    prob_min = prob_min
  )
  
  # Construct the URL with the parameters
  url <- modify_url(base_url, query = params)
  
  # Create the request body as a list from the DataFrame
  request_body <- as.list(data)
  
  # Send POST request to the API with authentication and JSON-encoded body
  response <- POST(url, body = jsonlite::toJSON(request_body), encode = "json", authenticate(username, password))
  
  # Check the response status code
  if (status_code(response) == 200) {
    # Return the parsed JSON content of the response
    return(content(response, "parsed"))
  } else if (status_code(response) == 400) {
    # If the status code is 400, print the error detail
    error_detail <- content(response, "parsed")$detail
    print(error_detail)
  } else {
    # If there is an error, print a generic error message and return NULL
    print("Error occurred while querying the API.")
    return(NULL)
  }
}

# Set the authentication credentials
username <- "username"
password <- "password"

# Create vectors for the data columns
libs <- c("LOUEUR MEUBLE NON PROFESSIONNEL EN RESIDENCE DE SERVICES (CODE APE 6820A Location de logements)", "CONSULTANT EN INNOVATION", "INTERMEDIAIRE DE COMMERCE MISE EN RELATION")
types <- c("I", "X", "Y")
natures <- c(NA, NA, 10)
surfaces <- c(NA, NA, 2)
events <- c("01P", "01P", "01P")

# Create the dataframe
df <- data.frame(
  text_description = libs,
  type_ = types,
  nature = natures,
  surface = surfaces,
  event = events,
  stringsAsFactors = FALSE
)

# Call the query_batch_api function with the provided parameters
result <- query_batch_api(username, password, df)

# Print the result
result

