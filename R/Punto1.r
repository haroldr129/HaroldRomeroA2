library (tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)
library(dplyr)

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

### Tomar una muestra del dataset inicial, vamos a tomar el 1%
muestra <- diabetes_012[sample(nrow(diabetes_012), 2536), ] #Muestre aleatorio simple

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


################################### Punto 2

#######Primer Modelo

###muestra estratificada del 1% de la población, variable clase Diabetes_012
set.seed(1)
Datos_Primer_Modelo <- diabetes_012 %>%
  group_by(Diabetes_012) %>%
  sample_n(1268, replace = TRUE) %>%
  ungroup()

sample.index1 <- sample(1:nrow(Datos_Primer_Modelo)
                       ,nrow(Datos_Primer_Modelo)*0.7
                       ,replace = F)


predictors <- colnames(Datos_Primer_Modelo)[-1]

train.data1 <- Datos_Primer_Modelo[sample.index1, c(predictors, "Diabetes_012"), drop = FALSE]
test.data1 <- Datos_Primer_Modelo[-sample.index1, c(predictors, "Diabetes_012"), drop = FALSE]

train.data1$Diabetes_012 <- factor(train.data1$Diabetes_012)
test.data1$Diabetes_012 <- factor(test.data1$Diabetes_012)

ctrl1 <- trainControl(method = "cv", p = 0.7)
knnFit1 <- train(Diabetes_012 ~ .
                , data = train.data1
                , method = "knn", trControl = ctrl1
                , preProcess = c("range") # c("center", "scale") for z-score
                , tuneLength = 50)

plot(knnFit1)

# Make predictions
knnPredict1 <- predict(knnFit1, newdata = test.data1)

# Creates the confusion matrix
confusionMatrix(data = knnPredict1, reference = test.data1$Diabetes_012)


###muestra estratificada del 1% de la población, variable clase HeartDiseaseorAttack
###### Segundo Modelo
set.seed(1)
Datos_Segundo_Modelo <- diabetes_012 %>%
  group_by(HeartDiseaseorAttack) %>%
  sample_n(1268, replace = TRUE) %>%
  ungroup()

sample.index2 <- sample(1:nrow(Datos_Segundo_Modelo)
                       ,nrow(Datos_Segundo_Modelo)*0.7
                       ,replace = F)

predictors2 <- colnames(Datos_Segundo_Modelo)[-8]

###muestra estratificada del 1% de la población, variable clase Sex
###### Tercer Modelo
set.seed(1)
Datos_Tercer_Modelo <- diabetes_012 %>%
  group_by(Sex) %>%
  sample_n(1268, replace = TRUE) %>%
  ungroup()

sample.index3 <- sample(1:nrow(Datos_Tercer_Modelo)
                       ,nrow(Datos_Tercer_Modelo)*0.7
                       ,replace = F)

predictors3 <- colnames(Datos_Tercer_Modelo)[-19]


