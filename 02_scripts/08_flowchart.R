# 09_flowchart.R
# creating a flow chart diagram for the screening of articles for the study

# load in packages
library(tidyverse)
library(DiagrammeR)


grViz("digraph flowchart {
  # node definitions with substituted label text
  node [fontname = Helvetica, shape = rectangle, fixedsize = true, width = 4, height = 0.8, pathwidth = 3] 
  1 [label = 'Research Outputs Identified \n(n = 653)']
  2 [label = 'Uses Kaggle Data \n(n = 467)']
  3 [label = 'Full Text Screen (n = 190)']
  4 [label = 'Included Prediction Model Article (n = 124)']
  
  m1 [label = 'No Kaggle Data (n = 128)']
  m2 [label = 'Not a Research Article (n = 277)']
  m3 [label = 'Not a Prediction Model Article (n = 66)']

  node [shape = none, width=0, height=0, label='']
  p1 -> 2; p2 -> 3; p3 -> 4;
  {rank=same; p1 -> m1 [minlen = 10]}
  {rank=same; p2 -> m2 [minlen = 10]}
  {rank=same; p3 -> m3 [minlen = 10]}

  edge [dir=none]
  1 -> p1; 2 -> p2; 3 -> p3;
}")
