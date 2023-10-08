library (tidyverse)
library(caret)

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

diabetes_012 <-
  read_csv(paste0(parentFolder
                  ,"/Data/diabetes_012_health_indicators.csv"))




