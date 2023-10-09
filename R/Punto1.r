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

###### programación para que las personas que no tienen diabetes, queden en 0 y las personas
###### que tienen pre diabetes y diabetes, queden en 1
diabetes_012$Diabetes_012 <- ifelse(diabetes_012$Diabetes_012 == 0, 0, 1)

set.seed(1)

### Tomar una muestra del dataset inicial, vamos a tomar 4000 muestras de las 253680
muestra <- diabetes_012[sample(nrow(diabetes_012), 2800), ] #Muestre aleatorio simple

table(muestra$Sex)
table(muestra$Smoker)
table(muestra$CholCheck)

########## Exploración de los datos

head(muestra) ##  Primeras observaciones o muestras del dataset
summary(muestra) ## Información estadistica del dataset

##### Realizar histograma por cada variable con el fin de ver la cantidad de observaciones realizadas por cada rango
## y con esto podria indicar la probabilidad que existe que una nueva observación caiga en cada valor

par(mfrow = c(2, 3))
hist(muestra$Diabetes_012, breaks = 100, col = "red")
hist(muestra$BMI, breaks = 100, col = "blue") # de acuerdo con el resultado podemos decir que tiene una distribución normal
hist(muestra$GenHlth, breaks = 100, col = "green")
hist(muestra$Age, breaks = 100, col = "yellow")
hist(muestra$MentHlth, breaks = 100, col = "purple")
hist(muestra$PhysHlth, breaks = 100, col = "pink")

#Todas nuestras variables son numericas, por lo que no se requiere ejecutar el comando as factor
pairs.panels(muestra [c("Age", "BMI", "Education", "GenHlth")],
             pch = 21,
             bg = c("purple", "black", "red", "orange" , "yellow")[unclass(muestra$Diabetes_012)]) # En este caso nuestra variable clase es Diabetes_012

