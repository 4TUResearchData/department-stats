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

get_all_datasets_group <- function(api_url= "https://data.4tu.nl/v2/articles" , 
                                   group_id = NULL, 
                                   published_since = NULL) {
  page_number = 1
  page_size = 1000
  dataset_list <- list()

  repeat{
    req <- request(api_url) |>
      req_url_query( "page" = page_number
                    ,"page_size" = page_size)
    
    if (!is.null(group_id)){ req <- req |> req_url_query("group"= group_id, .multi = "explode") }
    if (!is.null(published_since)){ req <- req |> 
                                            req_url_query("published_since" = published_since) }
    
    response <- req |>
      req_perform() |>
      resp_body_string() |>
      fromJSON()

  # Extract events data
  if (!is.null(response) && nrow(response) > 0) {
     dataset_list[[page_number]] <- response
     if (nrow(response) == 1000) {
      page_number = page_number + 1
     } else break
    } else break  # Stop if no more data
   }

  datasets <- bind_rows(dataset_list ) 
  return(datasets)
}

clean_basic <- function(text_vector){
  output_vector <- text_vector |> 
                   str_replace_all("&", "and")|>
                   str_remove_all("[[:punct:]]")|>
                   tolower()
                }

api_url <- "https://data.4tu.nl/v2/articles"
group <- list( 'eindhoven' = c(28589,28631),
               'delft'    = c(28586,28628), 
               'twente'   = c(28592,28634),
               'wur'      = 28595)

uni <- "delft"

start_date <- "2024-01-01"


all_datasets <- get_all_datasets_group(group = group[[uni]]
                                      ,published_since = NULL)

str(all_datasets)

all_datasets <- all_datasets|>
  select(-timeline)

# Save all datasets in a data file 
# write extracted files list
write_csv(all_datasets, 
  paste0('data/extracts/data-all-overview','-',uni,'-',
  Sys.Date(),
  '.csv' ))

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

  datasets[[dataset]]<- tibble( group_id = response$group_id
                               ,uuid = dataset
                               ,doi = response$doi 
                               ,title = response$title
                               ,created_date = response$created_date
                               ,modified_date = response$modified_date
                               ,organization = organizations
                               ,is_embargoed = response$is_embargoed
                               ,embargo_type = response$embargo_title
                               ,dataset_type = response$defined_type_name
                              )
}

#  Convert to a data.frame
org <- sapply(datasets, function(x) unlist(x$organization) ) 
if(is.list(org)) {org <- unlist(org)}

datasets <- datasets |> 
  lapply(function(x) select(x, -organization))|>
  bind_rows() |>
  mutate(organization = org)


write_csv(datasets, 
  paste0('data/extracts/data-all-details','-',uni,'-',
  Sys.Date(),
  '.csv' ))


# Organisation information text cleaning
uni_pattern <- c(eindhoven = "(Eindhoven University of Technology|TU Eindhoven)"
                ,delft     = "(Delft University of Technology|TU Delft)"
                ,wur       = "(WUR|Wageningen University and Research)"
                ,twente    = "(University of Twente|UT)")

datasets<- datasets|>
  mutate(organization_clean = organization |> 
                               str_remove_all(unname(uni_pattern['twente']))|>
                                clean_basic())

datasets|>
  count(organization_clean) |>
  arrange(desc(n)) |>
  print(n = 50)

# Departments  + other units at Tu/e
faculties <- list(
  eindhoven =  c("Applied Physics"
                ,"Biomedical Engineering"
                ,"Built Environment"
                ,"Chemical Engineering"
                ,"Electrical Engineering"
                ,"Industrial Design"
                ,"Industrial Engineering"
                ,"Mathematics and Computer Science"
                ,"Mechanical Engineering"
                ,"Eindhoven School of Education"
                ),
  delft =      c("Aerospace Engineering"
                ,"Industrial Design Engineering"
                ,"Technology Policy and Management"
                ,"Electrical Engineering Mathematics and Computer Science"
                ,"Civil Engineering and Geoscience"
                ,"Mechanical Engineering"
                ,"Architecture and the Built Environment"
                ,"Applied Sciences"
                ,"Mechanical Maritime and Materials Engineering"
                ,"QuTech"
                ,"Kavli"), 
  twente =     c("Behavioural, Management and Social sciences"
                ,"Engineering Technology"
                ,"Electrical Engineering, Mathematics and Computer Science"
                ,"Science and Technology"
                ,"GeoInformation Science and Earth Observation"
                ,"MESA+"
                , "Techmed Centre"
                , "Digital Society"),
  wur =         c("Agrotechnology and Food Sciences"
                ,"Food sciences"
                ,"Human Nutrition & Health"
                ,"Biomolecular sciences"
                ,"Biobased sciences"
                ,"Animal Sciences"
                ,"Environmental Sciences"
                ,"Plant Sciences"
                ,"Social Sciences"
                ,"Space, Place and Society"
                ,"Sustainable Governance"
                ,"Gender Studies"
                ,"Wageningen Bioveterinary Research"
                ,"Wageningen Environmental Research"
                ,"Wageningen Food and Biobased Research"
                ,"Wageningen Food Safety Research"
                ,"Wageningen Livestock Research"
                ,"Wageningen Marine Research"
                ,"Wageningen Plant Research"
                ,"Wageningen Social & Economic Research"))

departments <- list(wur = c("Food Chemistry"
                            ,"Food Microbiology"
                            ,"Food Process Engineering"
                            ,"Food Quality and Design"
                            ,"Physics and Physical Chemistry of Foods"
                            ,"Global Nutrition"
                            ,"Nutrition and Disease"
                            ,"Nutritional Biology"
                            ,"Nutrition, Metabolism and Genomics"
                            ,"Sensory Science and Eating Behaviour"
                            ,"Biochemistry"
                            ,"BioNanoTechnology"
                            ,"Biophysics"
                            ,"Microbiology"
                            ,"Organic Chemistry"
                            ,"Physical Chemistry and Soft Matter"
                            ,"Systems and Synthetic Biology"
                            ,"Toxicology"
                            ,"Bioprocess Engineering"
                            ,"Environmental Technology"
                            ,"Biobased Chemistry and Technology"
                            ,"Animal Breeding and Genomics Group"
                            ,"Adaptation Physiology Group"
                            ,"Aquaculture and Fisheries"
                            ,"Animal Nutrition Group"
                            ,"Animal Production Systems Group"
                            ,"Behavioural Ecology Group"
                            ,"Cell Biology and Immunology Group"
                            ,"Experimental Zoology Group"
                            ,"Host-Microbe Interactions"
                            ,"Human and Animal Physiology"
                            ,"Infectious Disease Epidemiology"
                            ,"Marine Animal Ecology"
                            ,"Soil Biology Group"
                            ,"Soil Chemistry and Chemical Soil Quality"
                            ,"Soil Physics and Land Management Group"
                            ,"Soil Geography and Landscape Group"
                            ,"Aquatic Ecology and Water Quality Management"
                            ,"Cultural Geography Group"
                            ,"Forest and Nature Conservation Policy Group"
                            ,"Forest Ecology and Forest Management Group"
                            ,"Hydrology and Environmental Hydraulics"
                            ,"Laboratory of Geo-information Science and Remote Sensing"
                            ,"Landscape Architecture and Spatial Planning"
                            ,"Meteorology and Air Quality Group"
                            ,"Plant Ecology and Nature Conservation Group"
                            ,"Water Resources Management Group"
                            ,"Wildlife Ecology and Conservation Group"
                            ,"Earth Systems and Global Change Group"
                            ,"Bioinformatics Group"
                            ,"Biosystematics Group"
                            ,"Centre for Crop Systems Analysis"
                            ,"Agricultural Biosystems Engineering"
                            ,"Farming Systems Ecology Group"
                            ,"Horticulture & Product Physiology Group"
                            ,"Laboratory of Entomology"
                            ,"Laboratory of Genetics"
                            ,"Laboratory of Nematology"
                            ,"Laboratory of Phytopathology"
                            ,"Laboratory of Plant Physiology"
                            ,"Laboratory of Plant Breeding"
                            ,"Laboratory of Virology"
                            ,"Mathematical and Statistical Methods"
                            ,"Plant Production Systems Group"
                            ,"Cluster Plant Developmental Biology"
                            ,"Laboratory of Molecular Biology"
                            ,"Laboratory of Cell Biology"
                            ,"Artificial Intelligence Group"
                            ,"Business Economics Group"
                            ,"Business Management and Organisation"
                            ,"Information Technology Group"
                            ,"Marketing and Consumer Behaviour Group"
                            ,"Operations Research and Logistics Group"
                            ,"Education and Learning Sciences"
                            ,"Knowledge, Technology and Innovation Group"
                            ,"Philosophy Group"
                            ,"Strategic Communication Group"
                            ,"Agricultural Economics and Rural Policy Group"
                            ,"Development Economics Group"
                            ,"Environmental Economics and Natural Resource Group"
                            ,"Economic and Environmental History Group"
                            ,"Urban Economics"
                            ,"Health and Society Group"
                            ,"Rural Sociology Group"
                            ,"Sociology of Development and Change Group"
                            ,"Consumption and Healthy Lifestyles"
                            ,"Environmental Policy Group"
                            ,"Law Group"
                            ,"Public Administration and Policy Group"
                            ,"Gender Studies"
                            ,"Wageningen Bioveterinary Research"
                            ,"Wageningen Environmental Research"
                            ,"Wageningen Food and Biobased Research"
                            ,"Wageningen Food Safety Research"
                            ,"Wageningen Livestock Research"
                            ,"Wageningen Marine Research"
                            ,"Wageningen Plant Research"
                            ,"Wageningen Social & Economic Research"))


departments_clean <- lapply(departments,function(x) clean_basic(x)|>
                                              str_remove_all(" group"))

faculties_clean <- lapply(faculties,function(x) clean_basic(x))

# Extract matching values from organizational lists based on TU/e departments list

datasets$org_clean <- datasets$organization_clean |>
  map(~ {
    matches <- str_extract_all(.x, paste(faculties_clean[[uni]], collapse = "|"))
    if (length(matches[[1]]) == 0) {
      "other"  # Keep the original value if no match
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

# look at the results
datasets_clean|>
  filter(created_date >= start_date) |>
  count(org_clean) |>
  arrange(desc(n)) |>
  print(n = 60)

# Have a look at the "Other" category
datasets_clean|>
  filter(created_date >= start_date & org_clean=="other") |>
  count(organization_clean) |>
  arrange(desc(n)) |>
  print(n = 60) |>
  View()

# Have a look at the "Other" category
datasets_clean|>
  filter(org_clean=="other") |>
  print(n = 60) |>
  View()

# show the results on a plot
datasets_clean|>
  filter(created_date >= start_date) |>
  count(org_clean)|>
  arrange(desc(n)) |>
  head(15) |>
  mutate(org_clean = forcats::fct_reorder(org_clean, n)) |>
  ggplot(aes(x= org_clean, y= n)) +
  geom_col(fill = "orange") +
  coord_flip()+
  labs(title = "Dataset and software publications per department",
       subtitle = "since 01-01-2024", 
       x = "Faculty/Institute", 
       y = "Number of publications") +
  geom_text(aes(label = n), hjust = -0.5, size = 2)+
  theme_minimal() 
  
ggsave(filename = "tud-faculty.png", path = "outputs", 
width =595, height = 329, units = "mm")

# results per year
datasets_clean|>
  mutate(created_year = lubridate::year(created_date)) |>
  count(created_year,org_clean)|>
  arrange(desc(n)) |>
  mutate(org_clean = forcats::fct_reorder(org_clean, n)) |>
  ggplot(aes(x= org_clean, y= n)) +
  geom_col(fill = "orange") +
  facet_wrap(vars(created_year))+
  coord_flip()+
  labs(title = "Dataset and software publications per department",
       #subtitle = "since 01-01-2024", 
       x = "Department", 
       y = "Number of publications") +
  geom_text(aes(label = n), hjust = -0.5, size = 3)+
  theme_minimal()


# Extract for specific faculty" 

# extract civil engineering datasets 
datasets_me <- datasets_clean |>
  filter(org_clean == "mechanical engineering") |>
  select(uuid, doi, title, 
         created_date, modified_date, organization)

# write extracted files list
write_csv(datasets_me, 
  'data/extracts/data-me.csv')
