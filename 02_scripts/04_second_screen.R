# 04_second_screen.R
# check screening with gwets statistic

# produce 10 random article for second screener to reivew
# seed will be the inverse order of the first seed
# first seed was 20250815, this seed will be 51805202

# load in libraries
library(tidyverse)
library(irrCAC)

# randomly select 10 articles from all for the second reviewer to screen against tripod
# set seed
set.seed(51805202)

# join the articles
second_screen <- rbind(stroke_assessed, diabetes_assessed) %>%
  mutate(random = runif(n=n())) %>%
  arrange(desc(random)) %>% 
  select(!random) %>% 
  head(10) %>% 
  select(type, title, doi)
  
# save the output to be screened by second screener
write.csv(second_screen, file = "01_data/02_second_screen/tripod_second_screen.csv")

# load in the second screen
second_complete <- read.csv("01_data/02_second_screen/tripod_assessment_second.csv")

# load in the stroke and diabetes assessed articles
stroke_assessed <- read.csv("01_data/stroke_assessed.csv")
diabetes_assessed <- read.csv("01_data/diabetes_assessed.csv")

# bind the datasets together to select for the second screened articles and select the tripod assessments
combined_tripod_assessed <- rbind(stroke_assessed, diabetes_assessed) %>% 
  select(type, title, doi, x5a, x5b, x6a, x7, x8a, x9a, x9b, x10, x11)
  

ten_tripod_assessed <- combined_tripod_assessed[combined_tripod_assessed$title %in% second_complete$title, ]


