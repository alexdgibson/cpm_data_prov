# 03_tripod_assessments.R
# created 08/09/2025
# examine and summarise citations and research interest over time

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
#write.csv(diabetes_assessed, file = "01_data/diabetes_assessed.csv")

# create a list of the DOIS for the stroke research articles to be searched in OA
oa_diabetes_doi <- as.list(diabetes_assessed$doi)

# complete for the diabetes articles first

# Search OA for all of the dois available and get the OA ID to search for article type
oa_diabetes <- oa_fetch(
  entity = "works",
  doi = oa_diabetes_doi,
  per_page = 200,
  abstract = FALSE,
  verbose = TRUE
)






# select all of the diabetes articles that have been cited once or more
# these will be searched to check article type (i.e. review)
diabetes_ids <- oa_diabetes %>%
  filter(cited_by_count > 0) %>%
  pull(id)

# search for the articles which have cited the included articles to get their article type
diabetes_cited_info <- map_dfr(diabetes_ids,
                                ~ oa_fetch(entity = "works",
                                           cites = .x,
                                           per_page = 200,
                                           abstract = FALSE))

# group the article types together and check "review" for the project
diabetes_review <- diabetes_cited_info %>% 
  filter(id != "https://openalex.org/W4409194057") %>% 
  group_by(type) %>% 
  summarise(n = n()) %>% 
  filter(type == "review")


# get the number of articles which had citations
nrow(oa_diabetes %>% filter(id != "https://openalex.org/W4409194057")) # number of diabetes articles indexed in OA
length(diabetes_ids)-1 # number of diabetes articles which had more than 0 citations
oa_diabetes %>% 
  filter(id != "https://openalex.org/W4409194057") %>% 
  summarise(total = sum(cited_by_count)) # number of total citations all of the diabetes articles have received
diabetes_review # number of citations which were review articles



# complete for the stroke articles second

# filter to the cpm studies
stroke_assessed <- stroke_tripod %>% 
  filter(cpm_type != "na")

# save this as a csv file for later use
#write.csv(stroke_assessed, file = "01_data/stroke_assessed.csv")


# create a list of the DOIS for the stroke research articles to be searched in OA
oa_stroke_doi <- as.list(stroke_assessed$doi)

# Search OA for all of the dois available and get the OA ID to search for article type
oa_stroke <- oa_fetch(
  entity = "works",
  doi = oa_stroke_doi,
  per_page = 200,
  abstract = FALSE,
  verbose = TRUE
)


# select all of the diabetes articles that have been cited once or more
# these will be searched to check article type (i.e. review)
stroke_ids <- oa_stroke %>%
  filter(cited_by_count > 0) %>%
  pull(id)

# search for the articles which have cited the included articles to get their article type
stroke_cited_info <- map_dfr(stroke_ids,
                              ~ oa_fetch(entity = "works",
                                        cites = .x,
                                        per_page = 200,
                                        abstract = FALSE))

# group the article types together and check "review" for the project
stroke_review <- stroke_cited_info %>% 
  filter(id != "https://openalex.org/W4409194057") %>% 
  group_by(type) %>% 
  summarise(n = n()) %>% 
  filter(type == "review")



# get the number of articles which had citations
nrow(oa_stroke %>% filter(id != "https://openalex.org/W4409194057")) # number of stroke articles indexed in OA
length(stroke_ids)-1 # number of stroke articles which had more than 0 citations # remove the extra article using both datasets
oa_stroke %>% 
  filter(id != "https://openalex.org/W4409194057") %>% 
  summarise(total = sum(cited_by_count)) # number of total citations all of the stroke articles have received
stroke_review # number of citations which were review articles


# plotting the research interest in stroke research over time

# join stroke and diabetes together and plot the cumulative articles by publication date by OA over time
rbind(oa_stroke, oa_diabetes) %>% 
  distinct() %>% # remove one of the articles that uses both datasets (was one in each stroke and diabetes, just remove one)
  select(title, publication_date) %>%
  arrange(publication_date) %>% 
  mutate(total = row_number()) %>%
  ggplot(aes(x = publication_date, y = total))+
  geom_line()+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  labs(x = "Year",
       y = "Total Clinical Prediction Model Studies")+
  scale_y_continuous(breaks = c(0,20,40,60,80,100,120))

# save the figure
ggsave(filename = "03_figures/pubs_over_time.jpg",
       width = 6,
       height = 4,
       dpi = 500)

# get the total number of articles from both stroke and diabetes which had OA publication date available
rbind(oa_stroke, oa_diabetes) %>% 
  distinct() %>%
  nrow()

# get the last publication date
rbind(oa_stroke, oa_diabetes) %>% 
  distinct() %>% view()


# Getting the publishers of all the articles to plot the number of different publishers

# get the articles together to search OA
all_articles <- rbind(diabetes_tripod, stroke_tripod) %>% 
  filter(cpm_type != "na") %>% 
  distinct(doi)

# search OA
all_oa <- oa_fetch(entity = "works",
         doi = all_articles$doi,
         verbose = TRUE)

# plot into ha treemap
all_oa %>% 
  group_by(host_organization_name) %>% 
  summarise(n = n()) %>%
  na.omit() %>% 
  mutate(
    host_organization_name = case_when(
      host_organization_name %in% c("Nature Portfolio", 
                                    "Springer Nature", 
                                    "Springer Science+Business Media", 
                                    "BioMed Central") ~ "Springer Nature Group",
      TRUE ~ host_organization_name
    )
  ) %>% 
  group_by(host_organization_name) %>% 
  summarise(n = sum(n)) %>% 
  mutate(label_wrapped = str_wrap(host_organization_name, width = 15)) %>%
  ggplot(aes(area = n,
             label = label_wrapped,
             alpha = n)) +
  geom_treemap(fill = "#DD3333", colour = "black") +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    size = 10,
                    grow = FALSE,
                    alpha = 1) +
  theme(legend.position = "none")

# save the figure
ggsave(filename = "03_figures/treemap.jpg",
       width = 6,
       height = 4,
       dpi = 500)


# get summary stats
all_oa %>% 
  distinct() %>%  # remove one of the articles that uses both datasets (was one in each stroke and diabetes, just remove one)
  group_by(host_organization_name) %>% 
  summarise(n = n()) %>%
  na.omit() %>% mutate(total = sum(n))



# create a world map of where the articles have been published assessed by the first author affiliation

# load in the world map
world_map <- map_data("world")

# change spelling of some countries
countries <- data.frame(
  region = unique(all_articles$first_auth_country),
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


# plot the world map
ggplot(world_map_highlighted, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = highlight), color = "black", linewidth = 0.2) +
  scale_fill_manual(values = c("FALSE" = "grey85", "TRUE" = "#163E64"),
                    name = "Highlighted",
                    labels = c("Other", "Highlighted")) +
  theme_void()+
  theme(legend.position = "none")

# save the figure
ggsave(filename = "03_figures/worldmap.jpg",
       width = 12,
       height = 8,
       dpi = 300)


