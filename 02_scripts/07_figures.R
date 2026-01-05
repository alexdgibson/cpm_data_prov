# 07_figures.R
# created 30/10/2025

# load in libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(svglite)
library(showtext)

# load in all fonts to use for ggplot
font_add(family = "Garamond", regular = "GARA.ttc")

# load in each of the two kaggle data sets for exploratory analysis
stroke_df <- read.csv("01_data/01_kaggle/healthcare-dataset-stroke-data.csv")
diabetes_df <- read.csv("01_data/01_kaggle/diabetes_prediction_dataset.csv")

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





# STROKE DATASET BGL AND ID
stroke_df %>% 
  ggplot()+
  geom_vline(xintercept = max(stroke_df$id)/2, linewidth = 0.2, linetype = "dashed")+
  geom_point(aes(x = id, y = avg_glucose_level), alpha = 1, stroke = 0, size = 2, colour = "#163E64")+
  theme_classic()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12))+
scale_x_continuous(n.breaks = 8)+
  scale_y_continuous(limits = c(50,275), n.breaks = 6)+
  labs(x = "Identifier",
       y = "Avgerage Blood Glucose Level")

# save as jpg
ggsave(filename = "03_figures/stroke_id_glucose.jpg",
       height = 17.4,
       width = 23,
       dpi = 300,
       units = c("cm"))

# save as svg
ggsave(filename = "C:/Users/alexd/OneDrive - Queensland University of Technology/02_phd/06_prediction_data/06_aimos/02_present_figures/stroke_id_glucose.svg",
       height = 17.4,
       width = 23,
       dpi = 300,
       units = c("cm"))










diabetes_df %>% 
  ggplot()+
  geom_point(aes(x = blood_glucose_level, y = HbA1c_level), alpha = 1, stroke = 0, size = 2, colour = "#163E64")+
  theme_classic()+
  labs(y = "HbA1c Level",
       x = "Blood Glucose Level")+
  scale_x_continuous(limits = c(75,300), n.breaks = 8)+
  scale_y_continuous(limits = c(3,10))+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 12))


# save as jpg
ggsave(filename = "03_figures/diabetes_hba1c_bgl.jpg",
       height = 17.4,
       width = 23,
       dpi = 300,
       units = c("cm"))

# save as svg
ggsave(filename = "C:/Users/alexd/OneDrive - Queensland University of Technology/02_phd/06_prediction_data/06_aimos/02_present_figures/diabetes_hba1c_bgl.svg",
       height = 17.4,
       width = 23,
       dpi = 300,
       units = c("cm"))
