library (tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)

###### Cargar el dataset en R

folder <- dirname(rstudioapi::getSourceEditorContext()$path)
parentFolder <- dirname(folder)

diabetes_012 <-
  read_csv(paste0(parentFolder
                  ,"/Data/diabetes_012_health_indicators.csv"))

diabetes_012$Diabetes_012 <- ifelse(diabetes_012$Diabetes_012 == 0, 0, 1)

set.seed(1)

### Tomar una muestra del dataset inicial, vamos a tomar 4000 muestras de las 253680
muestra <- diabetes_012[sample(nrow(diabetes_012), 4000), ]

table(muestra$Sex)
table(muestra$Smoker)
table(muestra$CholCheck)

########## Exploración de los datos

head(diabetes_012) ##  Primeras observaciones o muestras del dataset
summary(diabetes_012) ## Información estadistica del dataset

##### Realizar histograma por cada variable con el fin de ver la cantidad de observaciones realizadas por cada rango
## y con esto podria indicar la probabilidad que existe que una nueva observación caiga en cada valor
hist(diabetes_012$Diabetes_012, breaks = 100)
hist(diabetes_012$BMI, breaks = 100) # de acuerdo con el resultado podemos decir que tiene una distribución normal
hist(diabetes_012$GenHlth, breaks = 100)
hist(diabetes_012$Age, breaks = 100)
hist(diabetes_012$Education, breaks = 100)
hist(diabetes_012$Income, breaks = 100)

#Todas nuestras variables son numericas, por lo que no se requiere ejecutar el comando as factor
pairs(diabetes_012[-c(1)], pch = 21
      , bg = c("red", "green3", "blue")[unclass(diabetes_012$Diabetes_012)]) # En este caso nuestra variable clase es Diabetes_012
