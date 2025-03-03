# Libraries used
library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)

# Make the API request to find articles from a certain group

get_all_datasets_group <- function(api_url, 
                                   limit = 1000,
                                   group, 
                                   published_since) {

  datasets <- request(api_url) |>
    req_url_query("limit"=limit,
                  "group"= group,
                  "published_since"= published_since) |>
                    req_perform() |>
                    resp_body_string() |>
                    fromJSON()
  return(datasets)

}

api_url <- "https://data.4tu.nl/v2/articles"
limit <- 1000
group <- c('eindhoven' = 28589, # eindhoven
            'delft' = 28586, 
            'twente' = 28592,
            'wur' = 28595
          )

start_date <- "2024-01-01"

all_datasets <- get_all_datasets_group(api_url, limit, group['delft'], start_date)

# make a loop for each dataset to retrieve the details from the API

datasets<- list()

for (dataset in all_datasets$uuid) {

  response <- request(api_url) |>
    req_url_path_append(dataset) |>
    req_perform() |>
    resp_body_string() |>
    fromJSON()
  
  if( !is.null(response$custom_fields)){
    organizations <-response$custom_fields|>
      filter(name == "Organizations")|>
      pull(value) 
  }
  else {
    organizations <- ""
  }

  datasets[[dataset]]<- tibble(uuid = dataset
                               ,doi = response$doi 
                               ,title = response$title
                               ,created_date = response$created_date
                               ,modified_date = response$modified_date
                               ,organization = organizations)
}

#  Convert to a data.frame
org <- sapply(datasets, function(x) unlist(x$organization) ) 

datasets <- datasets |> 
  lapply(function(x) select(x, -organization))|>
  bind_rows() |>
  mutate(organization = org)

# Organisation information text cleaning
uni_pattern <- c(eindhoven = "(Eindhoven University of Technology|TU Eindhoven)",
                 delft = "(Delft University of Technology|TU Delft)")

datasets<- datasets|>
  mutate(organization_clean = organization |> 
                                str_remove_all(unname(uni_pattern['delft']))|>
                                str_replace_all("&", "and")|> 
                                str_remove_all("[[:punct:]]")|>
                                tolower())

datasets|>
  count(organization_clean) |>
  print(n = 50)

# Departments  + other units at Tu/e
departments <- list(
  eindhoven =  c( "Applied Physics",
                  "Biomedical Engineering", 
                  "Built Environment", 
                  "Chemical Engineering",
                  "Electrical Engineering", 
                  "Industrial Design", 
                  "Industrial Engineering", 
                  "Mathematics and Computer Science",
                  "Mechanical Engineering", 
                  "Eindhoven School of Education"
                ), 
                delft = c("Aerospace Engineering"
                          ,"Industrial Design Engineering"
                          ,"Technology Policy and Management"
                          ,"Electrical Engineering Mathematics and Computer Science"
                          ,"Civil Engineering and Geoscience"
                          ,"Mechanical Engineering"
                          ,"Architecture and the Built Environment"
                          ,"Applied Sciences"
                          ,"Mechanical Maritime and Materials Engineering"
                          ,"QuTech"
                          ,"Kavli"
)) 

departments <- lapply(departments, tolower)


# Extract matching values from organizational lists based on TU/e departments list

datasets$org_clean <- datasets$organization_clean |>
  map(~ {
    matches <- str_extract_all(.x, paste(departments[['delft']], collapse = "|"))
    if (length(matches[[1]]) == 0) {
      .x  # Keep the original value if no match
    } else {
      matches[[1]]  # Return the matched values if found
    }
  })
  
datasets_clean <-datasets |>
  unnest(org_clean) |>
  mutate(org_clean= str_trim(org_clean)) |>
  # mutate(org_clean= org_clean |> 
  #  str_replace_all("department of mathemathics and computer science", "mathematics and computer science"))|>
  mutate(org_clean= org_clean |> 
   str_replace_all("mechanical maritime and materials engineering", "mechanical engineering"))|>
  distinct()

datasets_clean|>
  count(org_clean) |>
  arrange(n) |>
  print(n = 60)

# show the resultd on a plot
datasets_clean|>
  count(org_clean)|>
  arrange(desc(n)) |>
  head(10) |>
  mutate(org_clean = forcats::fct_reorder(org_clean, n)) |>
  ggplot(aes(x= org_clean, y= n)) +
  geom_col(fill = "orange") +
  coord_flip()+
  labs(title = "Dataset and software publications per department",
       subtitle = "since 01-01-2024", 
       x = "Department", 
       y = "Number of publications") +
  geom_text(aes(label = n), hjust = -0.5)+
  theme_minimal()

# extract civil engineering datasets 
datasets_ceg <- datasets_clean |>
  filter(org_clean == "civil engineering and geoscience") |>
  select(uuid, doi, title, 
         created_date, modified_date, organization)


write_csv(datasets_ceg, 
  'data/extracts/data-ceg.csv')
