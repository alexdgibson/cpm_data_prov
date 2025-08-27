# 02_research_outputs.R
# created 20/08/2025
# take the results of Google Scholar and identify which research outputs to screen and not

# load libraries
library(tidyverse)

# load in data
ro <- read.csv("01_data/kaggle_research_outputs.csv")

# check structure
str(ro)


# Gather the title, doi from articles that use either data set that have an available PDF in English
# TRIPOD+AI statement will only be conducted on research articles that use the dataset

stroke_tripod <- ro %>% 
  filter(type == "article" &
           uses_data == "yes" &
           search == "stroke" &
           pdf == "yes" &
           english == "yes")

# check for any duplicate results
# identify the DOIs that are duplicated
stroke_remove_doi <- stroke_tripod %>% 
  filter(duplicated(stroke_tripod$doi) & doi != "na")

stroke_remove_title <- stroke_tripod %>% 
  filter(duplicated(stroke_tripod$title) & doi != "na")

# remove all duplicates then add back in only one of the duplicate rows
stroke_tripod_screen <- stroke_tripod[!(stroke_tripod$doi %in% stroke_remove_doi$doi),] %>% bind_rows(stroke_remove_doi)
stroke_tripod_screen <- stroke_tripod[!(stroke_tripod$title %in% stroke_remove_title$title),] %>% bind_rows(stroke_remove_title)

# form the list into a random order with the date the data was collected
# date collected was 15/08/2025 and seed is 15082025
set.seed(15082025)
stroke_tripod_screen = mutate(stroke_tripod_screen, random = runif(n=n())) %>%
  arrange(desc(random)) %>% 
  select(!random)

# save the csv output for screening
write.csv(stroke_tripod_screen, file = "01_data/stroke_tripod_screen.csv")

# complete for the diabetes dataset to be screened as well
# load the diabetes dataset
diabetes_tripod <- ro %>% 
  filter(type == "article" &
           uses_data == "yes" &
           search == "diabetes" &
           pdf == "yes" &
           english == "yes")

# check for any duplicate results
# identify the DOIs that are duplicated
diabetes_remove_doi <- diabetes_tripod %>% 
  filter(duplicated(diabetes_tripod$doi) & doi != "na")

diabetes_remove_title <- diabetes_tripod %>% 
  filter(duplicated(diabetes_tripod$title) & doi != "na")

# no duplicates in the diabetes outputs
# convert the list randomly for screening
set.seed(15082025)
diabetes_tripod = mutate(diabetes_tripod, random = runif(n=n())) %>%
  arrange(desc(random)) %>% 
  select(!random)

# save the output to be screened in random order
write.csv(diabetes_tripod, file = "01_data/diabetes_tripod_screen.csv")


# gather summary data stroke and diabetes data

ro %>% 
  filter(search == "stroke") %>% 
  nrow()

ro %>% 
  filter(search == "stroke",
         uses_data == "yes") %>% 
  nrow()

ro %>% 
  filter(search == "stroke",
         uses_data == "no") %>% 
  nrow()

ro %>% 
  filter(search == "stroke",
         uses_data == "na") %>% 
  nrow()

ro %>% 
  filter(search == "stroke",
         uses_data == "yes",
         pdf == "yes",
         english == "yes") %>%
  nrow()
  
ro %>% 
  filter(search == "stroke",
         uses_data == "yes",
         pdf == "yes",
         english == "yes",
         type == "book") %>%
  nrow()




ro %>% 
  filter(search == "diabetes") %>% 
  nrow()

ro %>% 
  filter(search == "diabetes",
         uses_data == "yes") %>% 
  nrow()

ro %>% 
  filter(search == "diabetes",
         uses_data == "no") %>% 
  nrow()

ro %>% 
  filter(search == "diabetes",
         uses_data == "na") %>% 
  nrow()

ro %>% 
  filter(search == "diabetes",
         uses_data == "yes",
         pdf == "yes",
         english == "yes") %>%
  nrow()

ro %>% 
  filter(search == "diabetes",
         uses_data == "yes",
         pdf == "yes",
         english == "yes",
         type == "na") %>%
  nrow()
