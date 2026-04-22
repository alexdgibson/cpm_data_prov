## Evidence of Unreliable Data and Poor Data Provenance in Clinical Prediction Model Research and Clinical Practice

This is a README.md file as part of this research project examining data provenance in published clinical prediction models.

## Metadata

R version 4.5.2 (2025-10-31 ucrt) -- "[Not] Part in a Rumble"
Copyright (C) 2025 The R Foundation for Statistical Computing
Platform: x86_64-w64-mingw32/x64

Primary packages used in the R project:

[tidyverse (2.0.0), openalexR (2.0.2)]

## File structure of the R and GitHub project:
### 01_data 

This folder contains all of the data files and sub folders of data.

### 02_scripts

This folder contains all of the R scripts that were used in this research project

### 03_figures

This folder contains all of the figures produced from the R scripts in 02\_scripts


## 01_data explained

**01\_kaggle** contains two datasets that are both downloaded from the [Kaggle](www.kaggle.com) website and used for this study.
**02\_second\_screen** contains the random sample of articles which were screened by a second screener.
**03\_html** contains all of the Google Scholar HTML files from both searches finding research articles.
**04\_screen_shots_redacted** contains multiple screenshots of the Kaggle dataset webpages and discussion forum with personal data redacted.

There are 11 csv files in **01\_data**

- **articles_overton_altmetric.csv**: List of the stroke and diabetes articles to be searched in [Overton](https://www.overton.io/) and [Altmetric](https://www.altmetric.com/).

- **diabetes_assessed.csv**: This is a copy of the diabetes_tripod_screen.csv file which has added TRIPOD+AI screening. Assessment was completed by entering results into excel.

- **diabetes_rescreen.csv**: This is the rescreening of TRIPOD+AI item 7 for diabetes articles.

- **diabetes_tripod_screen.csv**: This is the set of diabetes articles there were identified for screening.

- **kaggle_research_outputs.csv**: This is all of the research outputs identified by Google Scholar and screening for inclusion or not.

- **practical_assessment.csv**: This is an export of all the articles and the practical assessments.

- **stroke_assessed.csv**: This is a copy of the stroke_tripod_screen.csv file which has added TRIPOD+AI screening. Assessment was completed by entering results into excel.

- **stroke_rescreen.csv**: This is the rescreening of TRIPOD+AI item 7 for stroke articles.

- **stroke_tripod_screen.csv**: This is the set of stroke articles there were identified for screening.

- **tripod_assessment.csv**: This is the TRIPOD+AI assessment of all articles included after screening.

- **altmetric_results.csv**: The results from the search in Altmetric, there is no csv file from overton as the search retrieved no results.



## 02_scripts explained

- **01_examine_kaggle_data.R**: This R file imports both Kaggle datasets for exploratory analyses of the data to determine if the data is authentic, simulated or fabricated.

- **02_research_outputs.R**: This R file imports the research outputs saved from Google Scholar and randomly sorts them into a list to be screened for inclusion. Research included for screening are saved as **stroke_tripod_screen.csv** and **diabetes_tripod_screen.csv**

- **03_oa_assessments.R**: This R file checks OpenAlex for citation counts and research interest over time for the included assessed articles.

- **04_second_screen.R**: This R file randomly sorts a new list of 10 articles to be screened by the second screener.

- **05_summary_statistics.R**: This R file gathers summary statistics from the TRIPOD+AI and screening assessments for included articles.

- **06_overton_altmetric.R**: This R file gathers the articles title and DOI to save for checking in Overton and Altmetric.

- **07_figures.R**: This R file produces some of the figures for the research project.

- **08_flowchart.R**: This R file produces the flowchart of for the screening of articles.

- **09_final_figures.R**: This R file produces the final two figures.

