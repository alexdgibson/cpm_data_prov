# 09_final_figures
# final figures for the manuscript of the article by stroke and diabetes data

# load in libraries
library(tidyverse)
library(ggplot2)
library(cowplot)
library(svglite)
library(patchwork)


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




# stroke figures

## scatter plot with truncation label ##
to_plot <- select(stroke_df, id, avg_glucose_level) %>%
  mutate(patient_id = as.numeric(id),
         avg_glucose_level = as.numeric(avg_glucose_level))


label_colour <- 'navy'
point_colour <- 'darkred'

aplot <- ggplot(data = to_plot, aes(x = id, y = avg_glucose_level)) +
  geom_point(pch = 1, alpha = 0.9, col = point_colour) +
  xlab('Patient ID') +
  ylab('Blood glucose') +
  geom_vline(lty = 2, col = label_colour, xintercept = 36470) +
  #geom_text(data = label, aes(x = id, y = avg_glucose_level, label = text), col = label_colour) +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

aplot


## scatter plot with truncation label ##
to_plot = select(stroke_df, bmi, avg_glucose_level) %>%
  mutate(bmi = as.numeric(bmi),
         avg_glucose_level = as.numeric(avg_glucose_level))
#
label = data.frame(bmi = 75, avg_glucose_level = 125, text = 'Truncation')
#
label_colour = 'navy'
point_colour = 'darkred'
bplot = ggplot(data = to_plot, aes(x = bmi, y = avg_glucose_level))+
  geom_point(pch = 1, alpha = 0.9, col=point_colour)+
  xlab('BMI')+
  ylab('Blood glucose')+
  geom_hline(lty=2, col=label_colour, yintercept=55)+
  geom_text(data = label, aes(x=bmi, y=avg_glucose_level, label = text), col=label_colour)+
  geom_segment(aes(x = 75, y = 120, xend = 70, yend = 56),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               col = label_colour) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())

bplot
#ggsave(filename = 'stroke_bmi_glucose_scatter_withlabel.jpg', plot, width=5, height=5, dpi=500)



## age histogram with labels ##
to_plot = mutate(stroke_df, 
                 age = as.numeric(age),
                 agecut = cut(age, breaks=seq(0,100,1)),
                 num = as.numeric(agecut)) %>%
  group_by(agecut, num) %>%
  tally() %>%
  ungroup()
# make axis ticks
minor_breaks = seq(0,83,1) 
breaks = seq(0,80,10)
labels = as.character(breaks)
breaks = breaks + 0.5 # move to left
minor_breaks = minor_breaks + 0.5 # move to left
# make labels
#label = data.frame(num = 15, n = 120, text = 'Many 1 year olds')
#
cplot = ggplot(data = to_plot, aes(x=num, y=n))+
  geom_bar(stat='identity', width=1, fill='darkred', col=NA)+
  scale_x_continuous(expand = c(0,0), 
                     labels = labels,
                     minor_breaks = minor_breaks,
                     breaks = breaks)+
  scale_y_continuous(expand=c(0,0), limits=c(0,130))+
  #geom_text(data = label, aes(x = num, y = n, label = text), adj=0, col = label_colour)+
  #geom_segment(aes(x = 14.5, y = 120, xend = 3, yend = 110), # arrow
  #              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
  #              col = label_colour) +
  xlab('Age (years)')+
  ylab('Number of patients')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        axis.ticks.x = )

cplot





## glucose histogram with labels ##
to_plot = mutate(stroke_df, 
                 avg_glucose_level = as.numeric(avg_glucose_level),
                 avg_glucose_level = cut(avg_glucose_level, breaks=seq(55,280,1)),
                 num = 55 + as.numeric(avg_glucose_level)) %>% # add on minimum
  group_by(avg_glucose_level, num) %>%
  tally() %>%
  ungroup()
# make axis ticks
minor_breaks = seq(55,280,1) 
breaks = seq(100,250,50)
labels = as.character(breaks)
breaks = breaks + 0.5 # move to left
minor_breaks = minor_breaks + 0.5 # move to left
# make labels
label = data.frame(num = 130, n = 52, text = 'Truncation')
#
dplot = ggplot(data = to_plot, aes(x=num, y=n))+
  geom_bar(stat='identity', width=1, fill='darkred', col=NA)+
  scale_x_continuous(expand = c(0,0), 
                     labels = labels,
                     minor_breaks = minor_breaks,
                     breaks = breaks)+
  scale_y_continuous(expand=c(0,0), limits=c(0, 110))+
  geom_text(data = label, aes(x = num, y = n, label = text), adj=0, col = label_colour)+
  geom_segment(aes(x = 135, y = 48, xend = 55.5, yend = 23), # arrow
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               col = label_colour) +
  xlab('Glucose')+
  ylab('Number of patients')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

dplot

# combine them all into a single plot
stroke_final = (aplot + bplot) / (cplot + dplot) +
  plot_annotation(tag_levels = list(c("A", "B", "C" , "D")))

# save the stroke plot
ggsave(filename = '03_figures/stroke_final_plot.jpg', stroke_final, width=10, height=8, dpi=500)







# for the diabetes data
# scatter plot
to_plot = select(diabetes_df, bmi, blood_glucose_level) %>%
  mutate(bmi = as.numeric(bmi),
         blood_glucose_level = as.numeric(blood_glucose_level))
#
daplot = ggplot(data = to_plot, aes(x = bmi, y = blood_glucose_level))+
  geom_point(pch = 1, alpha = 0.5, col='navy')+
  xlab('BMI')+
  ylab('Blood glucose')+  
  theme_bw()+
  theme(panel.grid.minor = element_blank())

daplot

# age in single years
label_colour = 'navy'
to_plot = mutate(diabetes_df, 
                 age = as.numeric(age),
                 agecut = cut(age, breaks=seq(0,100,1)),
                 num = as.numeric(agecut)) %>%
  group_by(agecut, num) %>%
  tally() %>%
  ungroup()
# label
label = data.frame(num = 40, n = 4250, text = 'Unusual number of patients\nwith the same age')
#
dbplot = ggplot(data = to_plot, aes(x=num, y=n))+
  geom_bar(stat='identity', width=1, col=NA, fill = 'navy')+
  geom_text(data = label, aes(x = num, y = n, label = text), adj=0.5, col = label_colour)+
  geom_segment(aes(x = 35, y = 3700, xend = 3, yend = 2100), # arrow
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               col = label_colour) +
  geom_segment(aes(x = 45, y = 3700, xend = 79, yend = 3000), # arrow
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               col = label_colour) +
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0), limits=c(0,5700))+
  xlab('Age')+
  ylab('Number of patients')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())

dbplot


# BMI in single units
to_plot = mutate(diabetes_df, 
                 bmi = as.numeric(bmi),
                 bmicut = cut(bmi, breaks=seq(10,96,1)),
                 num = as.numeric(bmicut)) %>%
  group_by(bmicut, num) %>%
  tally() %>%
  ungroup()
# make label
label = data.frame(num = 25, n = 22000, text = 'Extreme number of patients\nwith the same BMI')
#
dcplot = ggplot(data = to_plot, aes(x=num, y=n))+
  geom_bar(stat='identity', width=1, fill='navy', col=NA)+
  scale_x_continuous(expand = c(0,0), limits=c(0,50))+ #
  scale_y_continuous(expand=c(0,0), limits=c(0,30000))+
  xlab('BMI')+
  ylab('Number of patients')+
  geom_text(data = label, aes(x = num, y = n, label = text), adj=0, col = label_colour)+
  geom_segment(aes(x = 24, y = 22000, xend = 19, yend = 20005), # arrow
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               col = label_colour) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())

dcplot


to_plot = select(diabetes_df, blood_glucose_level, HbA1c_level) %>%
  mutate(blood_glucose_level = as.numeric(blood_glucose_level),
         hba1c_level = as.numeric(HbA1c_level))

ddplot = ggplot(data = to_plot, aes(x = blood_glucose_level, y = HbA1c_level)) +
  geom_point(pch = 1, alpha = 0.5, col='navy')+
  xlab('Blood glucose level') +
  ylab('HbA1c') +
  theme_bw()+
  theme(panel.grid.minor = element_blank())




# combine them all into a single plot
diabetes_final = (daplot + dbplot) / (dcplot + ddplot) +
  plot_annotation(tag_levels = list(c("A", "B", "C" , "D")))

# save the stroke plot
ggsave(filename = '03_figures/diabetes_final_plot.jpg', diabetes_final, width=10, height=8, dpi=500)
