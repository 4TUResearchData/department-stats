library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)

api_url <- "https://data.4tu.nl/v2/articles"

# Make the API request
all_datasets <- request(api_url) |>
  req_url_query("limit"=1000,
                "group"= 28589, # eindhoven
                "published_since"= "2024-01-01") |>
  req_perform() |>
  resp_body_string() |>
  fromJSON()

# make a loop for each dataset from eindhoven

datasets<- list()


for (dataset in all_datasets$uuid) {

  response <- request(api_url) |>
    req_url_path_append(dataset) |>
    req_perform() |>
    resp_body_string() |>
    fromJSON()
  
  organizations <-response$custom_fields|>
    filter(name == "Organizations")|>
    select(value) 

  datasets[[dataset]]<- tibble(uuid = dataset, 
                               doi = response$doi, 
                               organization = organizations$value)
}

org <- sapply(datasets, function(x) unlist(x$organization) ) 

datasets <- datasets |> 
  lapply(function(x) select(x, -organization))|>
  bind_rows() |>
  mutate(organization = org)

pattern <- "\\b(Eindhoven University of Technology,?|TU Eindhoven,?)\\b"

datasets<- datasets|>
  mutate(organization_clean = organization |> str_remove_all( pattern)|>
                              str_replace_all("Faculty", "Department")|> 
                              str_replace_all("&", "and")|> 
                              str_remove_all("[[:punct:]]")|>
                              tolower())

datasets|>
  count(organization_clean) |>
  print(n = 50)

departments <- c( "Applied Physics",
"Biomedical Engineering", 
"Built Environment", 
"Chemical Engineering",
"Electrical Engineering", 
"Industrial Design", 
"Industrial Engineering", 
"Mathematics and Computer Science",
"Mechanical Engineering", 
"eindhoven school of education"
 ) |> tolower()

 # Extract matching values from organizational lists based on TU/e departments list

datasets$org_clean <- datasets$organization_clean |>
  map(~ {
    matches <- str_extract_all(.x, paste(departments, collapse = "|"))
    if (length(matches[[1]]) == 0) {
      .x  # Keep the original value if no match
    } else {
      matches[[1]]  # Return the matched values if found
    }
  })
  
datasets_clean <-datasets |>
  unnest(org_clean) |>
  mutate(org_clean= str_trim(org_clean)) |>
  mutate(org_clean= org_clean |>
                      str_replace_all("department of mathemathics and computer science", "mathematics and computer science"))|>
  distinct()

datasets_clean|>
  count(org_clean)|>
  mutate(org_clean = forcats::fct_reorder(org_clean, n)) |>
  ggplot(aes(x= org_clean, y= n)) +
  geom_col(fill = "orange") +
  coord_flip()+
  labs(title = "TU/e dataset and software publications per department",
       subtitle = "since 01-01-2024", 
       x = "Department", 
       y = "Number of publications") +
  geom_text(aes(label = n), hjust = -0.5)+
  theme_minimal()
  

