# 03_tripod_assessments.R
# created 08/09/2025
# examine and summarise the results from the TRIPOD assessments

# load libraries
library(tidyverse)
library(janitor)
library(openalexR)
library(treemapify)
library(maps)

# load in data
tripod_df <- read.csv("01_data/tripod_assessment.csv") %>% 
  clean_names()

# split into kaggle and datasets
kaggle_tripod <- tripod_df %>% filter(type == "data")
diabetes_tripod <- tripod_df %>% filter(type == "diabetes")
stroke_tripod <- tripod_df %>% filter(type == "stroke")


# remove the outputs that did not meet the criteria for a tripod assessment
diabetes_assessed <- diabetes_tripod %>% 
  filter(cpm_type != "na")

# save this as a csv file for later use
write.csv(diabetes_assessed, file = "01_data/diabetes_assessed.csv")

# create a list of the DOIS for the diabetes research articles to be searched in OA
oa_diabetes_doi <- as.list(diabetes_assessed$doi)


# collecting the stroke articles from OpenAlex
oa_diabetes <- oa_fetch(
  entity = "works",
  doi = oa_diabetes_doi,
  verbose = TRUE,
  per_page = 200, # more than enough
  abstract = FALSE
)


test <- oa_query(entiy = "works",
         cites = "W4396754438")


oa_request(query_url = test,
           count_only = FALSE,
           verbose = TRUE)





query <- oa_query( entity = "works", cites = "W4396754438")


res <- oa_request( query_url = query, count_only = FALSE, verbose = TRUE)


df <- oa2df(res, entity = "works") 












# check if review articles have cited any of these articles
diabetes_cited_web <- oa_diabetes %>% filter(cited_by_count > 0) %>% 
  select(cited_by_api_url) %>% 
  split(., seq(nrow(.)))


# Create an empty list to store results
diabetes_cited <- list()

# Loop through each element of stroke_cited_web
for (i in seq_along(diabetes_cited_web)) {
  
  # Call oa_request using the appropriate query (possibly using stroke_cited_web[i])
  result <- oa_request(query_url = diabetes_cited_web[[i]], verbose = TRUE, parse = TRUE)
  
  # Store the result in the list
  diabetes_cited[[i]] <- result
}


# Create an empty data frame to store the values
diabetes_cited_info <- data.frame()

# Loop through each element in stroke_cited (1 to 55)
for (j in 1:length(diabetes_cited)) {
  
  # Check if the current element has any articles (not NULL or empty)
  if (length(diabetes_cited[[j]]) > 0) {
    
    # Loop through each article in the current stroke_cited element
    for (i in 1:length(diabetes_cited[[j]])) {
      
      # Extract values safely
      id <- diabetes_cited[[j]][[i]]$id
      doi <- diabetes_cited[[j]][[i]]$doi
      title <- diabetes_cited[[j]][[i]]$title
      type <- diabetes_cited[[j]][[i]]$type
      type_crossref <- diabetes_cited[[j]][[i]]$type_crossref
      cited_by_count <- diabetes_cited[[j]][[i]]$cited_by_count
      
      # Create a temporary data frame for this entry
      temp <- data.frame(id, doi, title, type, type_crossref, cited_by_count, stringsAsFactors = FALSE)
      
      # Append to the main data frame
      diabetes_cited_info <- rbind(diabetes_cited_info, temp)
    }
  }
}


# summary data from the stroke_cited_info
table(diabetes_cited_info$type)






# complete the same now for the stroke articles
# stroke
stroke_assessed <- stroke_tripod %>% 
  filter(cpm_type != "na")

# save this as a csv file for later use
#write.csv(stroke_assessed, file = "01_data/stroke_assessed.csv")

# create a list of the DOIS for the stroke research articles to be searched in OA
oa_stroke_doi <- as.list(stroke_assessed$doi)

# collecting the stroke articles from OpenAlex
oa_stroke <- oa_fetch(
  entity = "works",
  doi = oa_stroke_doi,
  verbose = TRUE,
  per_page = 200 # more than enough
)

# check if review articles have cited any of these articles
stroke_cited_web <- oa_stroke %>% filter(cited_by_count > 0) %>% 
  select(cited_by_api_url) %>% 
  split(., seq(nrow(.)))


# Create an empty list to store results
stroke_cited <- list()

# Loop through each element of stroke_cited_web
for (i in seq_along(stroke_cited_web)) {
  
  # Call oa_request using the appropriate query (possibly using stroke_cited_web[i])
  result <- oa_request(query_url = stroke_cited_web[[i]], verbose = TRUE, parse = TRUE)
  
  # Store the result in the list
  stroke_cited[[i]] <- result
}


# Create an empty data frame to store the values
stroke_cited_info <- data.frame()

# Loop through each element in stroke_cited (1 to 55)
for (j in 1:length(stroke_cited)) {
  
  # Check if the current element has any articles (not NULL or empty)
  if (length(stroke_cited[[j]]) > 0) {
    
    # Loop through each article in the current stroke_cited element
    for (i in 1:length(stroke_cited[[j]])) {
      
      # Extract values safely
      id <- stroke_cited[[j]][[i]]$id
      doi <- stroke_cited[[j]][[i]]$doi
      title <- stroke_cited[[j]][[i]]$title
      type <- stroke_cited[[j]][[i]]$type
      type_crossref <- stroke_cited[[j]][[i]]$type_crossref
      cited_by_count <- stroke_cited[[j]][[i]]$cited_by_count
      
      # Create a temporary data frame for this entry
      temp <- data.frame(id, doi, title, type, type_crossref, cited_by_count, stringsAsFactors = FALSE)
      
      # Append to the main data frame
      stroke_cited_info <- rbind(stroke_cited_info, temp)
    }
  }
}


# summary data from the stroke_cited_info
table(stroke_cited_info$type)







# plotting the research interest in stroke research over time
rbind(oa_stroke, oa_diabetes) %>% 
  select(title, publication_date) %>%
  arrange(publication_date) %>% 
  mutate(total = row_number()) %>%
  ggplot(aes(x = publication_date, y = total))+
  geom_line()+
  theme_classic()+
  labs(x = "Year",
       y = "Total Clinical Prediction Model Studies")+
  scale_y_continuous(breaks = c(0,15,30,45,60,75,90), )


ggsave(filename = "03_figures/pubs_over_time.jpg",
       width = 6,
       height = 4,
       dpi = 300)
















# Getting the publishers of all the articles

diabetes_tripod <- tripod_df %>% filter(type == "diabetes")
stroke_tripod <- tripod_df %>% filter(type == "stroke")


articles <- rbind(diabetes_tripod, stroke_tripod) %>% 
  filter(cpm_type != "na")


all_oa <- oa_fetch(entity = "works",
         doi = articles$doi,
         verbose = TRUE)






all_oa %>% 
  group_by(host_organization_name) %>% 
  summarise(n = n()) %>%
  na.omit() %>% 
  mutate(label_wrapped = str_wrap(host_organization_name, width = 15)) %>%   # << wrap text
  ggplot(aes(area = n,
             label = label_wrapped,
             alpha = n)) +
  geom_treemap(fill = "#DD3333") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = FALSE,
                    alpha = 1) +
  theme(legend.position = "none")


ggsave(filename = "03_figures/treemap.jpg",
       width = 6,
       height = 4,
       dpi = 300)







all_oa %>% 
  group_by(host_organization_name) %>% 
  summarise(n = n()) %>%
  na.omit() %>% mutate(total = sum(n))



world_map <- map_data("world")


countries <- data.frame(
  region = unique(articles$first_auth_country),
  highlight = TRUE) %>%
  mutate(
    region = case_when(
      region == "United States of America" ~ "USA",
      region == "United Kingdom" ~ "UK",
      region == "Libyra" ~ "Libya",
      region == "Republic of Korea" ~ "South Korea",
      TRUE ~ region))


# Merge with world_map

world_map_highlighted <- left_join(world_map, countries, by = "region")
world_map_highlighted$highlight[is.na(world_map_highlighted$highlight)] <- FALSE

ggplot(world_map_highlighted, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = highlight), color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("FALSE" = "grey85", "TRUE" = "#163E64"),
                    name = "Highlighted",
                    labels = c("Other", "Highlighted")) +
  theme_void()+
  theme(legend.position = "none")


ggsave(filename = "03_figures/worldmap.jpg",
       width = 12,
       height = 8,
       dpi = 300)


