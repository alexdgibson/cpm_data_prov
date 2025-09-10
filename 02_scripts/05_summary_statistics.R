# 05_summary_statistics.R
# getting summary results

# load in libraries
library(tidyverse)

# load in data
diabetes_assessed <- read.csv("01_data/diabetes_assessed.csv")
stroke_assessed <- read.csv("01_data/stroke_assessed.csv")


nrow(diabetes_assessed)
nrow(stroke_assessed)


# check practical applications for each of the articles
diabetes_practical <- diabetes_assessed %>% 
  filter(practical_use != "na" &
           practical_use != "There was no clear talk of any practical use of this model.") %>% 
  select(type, title, doi, practical_use, other_comments) 

# check practical applications for each of the articles
stroke_practical <- stroke_assessed %>% 
  filter(practical_use != "na") %>%
  select(type, title, doi, practical_use, other_comments) 

# read the assessments to ensure they are practical

practical_use_assessment <- rbind(stroke_practical, diabetes_practical)

# write to csv for assessing the practical uses
write.csv(practical_use_assessment, file = "01_data/practical_assessment.csv")



# countries of the first author for diabetes
diabetes_assessed %>% 
  group_by(first_auth_country) %>% 
  summarise(total = n()) %>% 
  arrange(-total)

# examining ethics for diabetes
diabetes_assessed %>% 
  group_by(ethics) %>% 
  summarise(n())

# examining declaration of helsinki for diabetes
diabetes_assessed %>% 
  group_by(helsinki) %>% 
  summarise(n())













# countries of the first author for stroke
stroke_assessed %>% 
  group_by(first_auth_country) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% view()


# examining ethics for stroke
stroke_assessed %>% 
  group_by(ethics) %>% 
  summarise(n())

# examining declaration of helsinki for stroke
stroke_assessed %>% 
  group_by(helsinki) %>% 
  summarise(n())









# total country for both stroke and diabetes
rbind(stroke_assessed, diabetes_assessed) %>% 
  group_by(first_auth_country) %>% 
  summarise(total = n()) %>% 
  arrange(-total) %>% view()
