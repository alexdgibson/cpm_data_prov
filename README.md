cpm\_data\_prov 

This is a README.md file for the cpm_data_prov project as part of research project examining data provenance in published clinical prediction models.

## Metadata

R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

Primary packages used in the R project:

[tidyverse (2.0.0), openalexR (2.0.2)]

File structure of the R and GitHub project:
## 01_data 

This folder contains all of the data files and sub folders of data.

## 02_scripts

This folder contains all of the R scripts that were used in this research project

## 03_figures

This folder contains all of the figures produced from the R scripts in 02\_scripts


## 01_data explained

**01\_kaggle** contains two datasets both which are downloaded from the Kaggle website.
**02\_second\_screen** contains the random sample of articles which were screened by a second screener.
**03\_html** contains all of the google scholar HTML files from both search filters.

There are 10 csv files in **01\_data**

- **articles_overton.csv** 
- **diabetes_assessed.csv** 
- **diabetes_rescreen.csv** 
- **diabetes_tripod_screen.csv** 
- **kaggle_research_outputs.csv** 
- **practical_assessment.csv** 
- **stroke_assessed.csv** 
- **stroke_rescreen.csv** 
- **stroke_tripod_screen.csv** 
- **tripod_assessment.csv** 


## 02_scripts explained

- **01_examine_kaggle_data.R**
- **02_research_outputs.R**
- **03_tripod_assessments.R**
- **04_second_screen.R**
- **05_summary_statistics.R**
- **06_overton_assessment.R**
- **07_figures.R**
- **08_pubpeer_retraction_watch.R**
- **09_flowchart.R**
