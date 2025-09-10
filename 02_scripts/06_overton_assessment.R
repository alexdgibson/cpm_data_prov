# 06_overton_assessment.R
# created 08/09/2025
# examine and summarise the results from the TRIPOD assessments

# load libraries
library(tidyverse)
library(janitor)

# load in data
tripod_df <- read.csv("01_data/tripod_assessment.csv") %>% 
  clean_names()

# select all the articles to be searched in overton
assessed_articles <- tripod_df %>% 
  filter(cpm_type != "na") %>%
  select(title, doi)

# save the csv output for screening
write.csv(assessed_articles, file = "01_data/articles_overton.csv")
