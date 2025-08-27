# 01_examine_kaggle_data.R
# created 18/08/2025
# examine the two kaggle data sets to determine an outcome if they are real, fake, simulated or verifiable

# load in libraries
library(tidyverse)


# load in each of the two kaggle data sets for exploratory analysis

stroke_df <- read.csv("01_data/02_kaggle/healthcare-dataset-stroke-data.csv")
diabetes_df <- read.csv("01_data/02_kaggle/diabetes_prediction_dataset.csv")


# check the structure of each data set
str(stroke_df)
str(diabetes_df)


# change variables to numeric
# categorical variables, e.g. male and female changed to factors
stroke_df <- stroke_df %>% 
  mutate(ever_married = as.factor(ever_married),
         gender = as.factor(gender),
         work_type = as.factor(work_type),
         Residence_type = as.factor(Residence_type),
         smoking_status = as.factor(smoking_status),
         id = as.numeric(id),
         age = as.numeric(age),
         hypertension = as.numeric(hypertension),
         heart_disease = as.numeric(heart_disease),
         avg_glucose_level = as.numeric(avg_glucose_level),
         bmi = as.numeric(bmi),
         stroke = as.numeric(stroke))

# diabetes dataset
diabetes_df <- diabetes_df %>% 
  mutate(gender = as.factor(gender),
         age = as.numeric(age),
         hypertension = as.numeric(hypertension),
         heart_disease = as.numeric(heart_disease),
         smoking_history = as.factor(smoking_history),
         bmi = as.numeric(bmi),
         HbA1c_level = as.numeric(HbA1c_level),
         blood_glucose_level = as.numeric(blood_glucose_level),
         diabetes = as.numeric(diabetes))


# plot the distributions for the continuous variables for stroke
# plotting the distribution of the numeric variable ID for stroke
stroke_df %>% 
  ggplot(aes(x = id))+
  geom_histogram()+
  theme_classic()

# plotting the distribution of the numeric variable age for stroke
stroke_df %>% 
  ggplot(aes(x = age))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# plotting the distribution of the numeric variable avg glucose level for stroke
stroke_df %>% 
  ggplot(aes(x = avg_glucose_level))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# plotting the distribution of the numeric variable bmi for stroke
stroke_df %>% 
  ggplot(aes(x = bmi))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# plot the totals for categorical variables
# bar charts  for stroke outcome
stroke_df %>% 
  ggplot(aes(x = stroke))+
  geom_bar()+
  theme_classic()

# bar charts  for gender
stroke_df %>% 
  ggplot(aes(x = gender))+
  geom_bar()+
  theme_classic()

stroke_df %>% filter(gender == "Female") %>% nrow()
stroke_df %>% filter(gender == "Male") %>% nrow()
stroke_df %>% filter(gender == "Other") %>% nrow()

# bar charts  for hypertension
stroke_df %>% 
  ggplot(aes(x = hypertension))+
  geom_bar()+
  theme_classic()

# bar charts  for heart disease
stroke_df %>% 
  ggplot(aes(x = heart_disease))+
  geom_bar()+
  theme_classic()

# bar charts  for ever married
stroke_df %>% 
  ggplot(aes(x = ever_married))+
  geom_bar()+
  theme_classic()

# bar charts  for work type
stroke_df %>% 
  ggplot(aes(x = work_type))+
  geom_bar()+
  theme_classic()

# bar charts  for residence
stroke_df %>% 
  ggplot(aes(x = Residence_type))+
  geom_bar()+
  theme_classic()

# bar charts  for smoking status
stroke_df %>% 
  ggplot(aes(x = smoking_status))+
  geom_bar()+
  theme_classic()

# plot a point graph of each continuous variable with each other for stroke
# ID and age
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = id, y = age))+
  theme_classic()

# id and glucose
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = id, y = avg_glucose_level))+
  theme_classic()

# id and glucose
# with vline at half the id variable
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = id, y = avg_glucose_level))+
  geom_vline(xintercept = max(stroke_df$id)/2)+
  theme_classic()

# id and bmi
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = id, y = bmi))+
  theme_classic()

# age and glucose
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = age, y = avg_glucose_level))+
  theme_classic()

# age and bmi
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = age, y = bmi))+
  theme_classic()

# glucose and bmi
stroke_df %>% 
  ggplot()+
  geom_point(aes(x = avg_glucose_level, y = bmi))+
  theme_classic()

# bmi vs avg glucose correlation between ID when ID is less than half max ID
stroke_df %>% 
  filter(id < (max(id)/2)) %>% 
  ggplot()+
  geom_point(aes(x = avg_glucose_level, y = bmi))+
  theme_classic()

# bmi vs avg glucose correlation between ID when ID is greater than half max ID
stroke_df %>% 
  filter(id > (max(id)/2)) %>% 
  ggplot()+
  geom_point(aes(x = avg_glucose_level, y = bmi))+
  theme_classic()

# examine missing data in the stroke data set
# select all the rows of data that have some missing data
stroke_df[!complete.cases(stroke_df), ]

# how many missing cases
nrow(stroke_df[!complete.cases(stroke_df), ])

# is missing BMI data random across ID
# number of missing data in the first half of the ID
stroke_df[!complete.cases(stroke_df), ] %>% 
  arrange(id) %>% 
  filter(id < max(stroke_df$id/2)) %>% nrow()

# missing data in the second half of the ID
# roughly 2/3 missing in the first half of the data arranged by ID
stroke_df[!complete.cases(stroke_df), ] %>% 
  arrange(id) %>% 
  filter(id > max(stroke_df$id/2)) %>% nrow()

# plot the distributions of variables with missing data
stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot(aes(x = id))+
  geom_histogram()+
  theme_classic()

stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot(aes(x = age))+
  geom_histogram(binwidth = 1)+
  theme_classic()

stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot(aes(x = avg_glucose_level))+
  geom_histogram(binwidth = 1)+
  theme_classic()


# plot a point plot of the observations with missing data

stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot()+
  geom_point(aes(x = id, y = age))+
  theme_classic()

# id and glucose
stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot()+
  geom_point(aes(x = id, y = avg_glucose_level))+
  theme_classic()

# age and glucose
stroke_df[!complete.cases(stroke_df), ] %>% 
  ggplot()+
  geom_point(aes(x = age, y = avg_glucose_level))+
  theme_classic()






# plot the distributions for the continuous variables for diabetes

# plotting the distribution of the numeric variable age for diabetes
diabetes_df %>% 
  ggplot(aes(x = age))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# examine the peak at 80 years old
# roughly 1/20 of the data set greater than 80 years old
diabetes_df %>% 
  filter(age > 79)

# plotting the distribution of the numeric variable avg glucose level for diabetes
diabetes_df %>% 
  ggplot(aes(x = blood_glucose_level))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# plotting the distribution of the numeric variable bmi for diabetes
diabetes_df %>% 
  ggplot(aes(x = bmi))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# examine the peak of bmi at 27
# 1/3 of the dataset has a bmi between 26 and 28
diabetes_df %>% 
  filter(bmi >26 & bmi < 28)

# plotting the distribution of the numeric variable hba1c for diabetes
diabetes_df %>% 
  ggplot(aes(x = HbA1c_level))+
  geom_histogram(binwidth = 1)+
  theme_classic()

# plot a point graph of each continuous variable with each other for diabetes
# ID and age
diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = age, y = blood_glucose_level))+
  theme_classic()

# id and glucose
diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = age, y = bmi))+
  theme_classic()

# id and glucose when age is less than 2
# constant BMI ~27 from 0 to 2
diabetes_df %>% 
  filter(age < 2) %>% 
  ggplot()+
  geom_point(aes(x = age, y = bmi))+
  theme_classic()

# age and hba1c
diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = age, y = HbA1c_level))+
  theme_classic()

# glucose and bmi
diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = blood_glucose_level, y = bmi))+
  theme_classic()

# avg blood glucose and hba1c
diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = blood_glucose_level, y = HbA1c_level))+
  theme_classic()




# examine missing data in the diabetes data set
diabetes_df[!complete.cases(diabetes_df), ]




# looking at the numerical difference between the id and subsequent ID
stroke_df %>% 
  select(id) %>% 
  arrange(id) %>% 
  mutate(difference = id - lag(.$id, n = 1)) %>%
  ggplot()+
  geom_histogram(aes(x = difference), binwidth = 1)+
  theme_classic()


stroke_df %>% 
  select(id) %>% 
  arrange(id) %>% 
  mutate(difference = id - lag(.$id, n = 1)) %>%
  ggplot()+
  geom_point(aes(x = id, y= difference))+
  theme_classic()

