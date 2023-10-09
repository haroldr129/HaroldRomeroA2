library (tidyverse)
library(caret)
library(class)
library(gmodels)
library(psych)
library(dplyr)
library(rpart)

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
muestra <-
  diabetes_012[sample(nrow(diabetes_012), 2536), ] #Muestre aleatorio simple

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
pairs.panels(muestra [c("Age", "BMI", "HighChol", "GenHlth")],
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

summary(Datos_Primer_Modelo)

# Crear un modelo de árbol de decisión para determinar los datos mas relevantes
modelo_arbol <- rpart(Diabetes_012 ~ .
                      , data = Datos_Primer_Modelo)

# Calcular la importancia de las características
importancia_caracteristicas <- modelo_arbol$variable.importance

# Visualizar la importancia de las características
print(importancia_caracteristicas)

sample.index1 <- sample(1:nrow(Datos_Primer_Modelo)
                       ,nrow(Datos_Primer_Modelo)*0.7
                       ,replace = F)


predictors <-
  colnames(Datos_Primer_Modelo)[-1]

train.data1 <-
  Datos_Primer_Modelo[sample.index1
                      , c(predictors, "Diabetes_012")
                      , drop = FALSE] #cuales de las muestras fueron seleccionadas para el entrenamiento
test.data1 <-
  Datos_Primer_Modelo[-sample.index1
                      , c(predictors
                          , "Diabetes_012")
                      , drop = FALSE] #Cuales de las muestras fueron seleccionadas para test

####KNN

train.data1$Diabetes_012 <-
  factor(train.data1$Diabetes_012)
test.data1$Diabetes_012 <-
  factor(test.data1$Diabetes_012)

ctrl1 <- trainControl(method = "cv"
                      , p = 0.7, number = 10) #Control del entrenamiento
knnFit1 <- train(Diabetes_012 ~ .
                , data = train.data1
                , method = "knn", trControl = ctrl1
                , preProcess = c("center", "scale") #for z-score
                , tuneLength = 50) #Permite hallar el K adecuado

knnFit1
plot(knnFit1)

# Make predictions
knnPredict1 <- predict(knnFit1,
                       newdata = test.data1)

knnPredict1

# Creates the confusion matrix
confusionMatrix(data = knnPredict1,
                reference = test.data1$Diabetes_012)


#Remover 5 predictors
Ret_5_Predicts1 <- c("Smoker"
                     , "MentHlth"
                     , "AnyHealthcare"
                     , "NoDocbcCost"
                     , "Veggies")

train.data1_1 <- train.data1[, !(names(train.data1) %in% Ret_5_Predicts1)]
test.data1_1 <- test.data1[, !(names(test.data1) %in% Ret_5_Predicts1)]

ctrl1_1 <- trainControl(method = "cv"
                        ,p = 0.7, number = 5)

knnFit1_1 <- train(Diabetes_012 ~ .
                 , data = train.data1_1
                 , method = "knn", trControl = ctrl1_1
                 , preProcess = c("center", "scale") # for z-score
                 , tuneLength = 30)

knnFit1_1
plot(knnFit1_1)

knnPredict1_1 <- predict(knnFit1_1
                         , newdata = test.data1_1)

confusionMatrix(data = knnPredict1_1
                , reference = test.data1_1$Diabetes_012)

#Remover 5 predictores mas
Ret_5_Predicts1_1 <- c("HvyAlcoholConsump"
                       ,"Fruits"
                       ,"Sex"
                       ,"Stroke"
                       ,"CholCheck")

train.data1_2 <- train.data1_1[, !(names(train.data1_1) %in% Ret_5_Predicts1_1)]
test.data1_2 <- test.data1_1[, !(names(test.data1_1) %in% Ret_5_Predicts1_1)]

ctrl1_2 <- trainControl(method = "repeatedcv"
                        , p = 0.7, number = 10, repeats = 3)
knnFit1_2 <- train(Diabetes_012 ~ .
                 , data = train.data1_2
                 , method = "knn", trControl = ctrl1_2
                 , preProcess = c("center", "scale") # for z-score
                 , tuneLength = 30)

knnFit1_2
plot(knnFit1_2)

# Make predictions
knnPredict1_2 <- predict(knnFit1_2
                         , newdata = test.data1_2)

# Creates the confusion matrix
confusionMatrix(data = knnPredict1_2
                , reference = test.data1_2$Diabetes_012)

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

# Crear un modelo de árbol de decisión para determinar los datos mas relevantes
modelo_arbol1 <- rpart(HeartDiseaseorAttack ~ .
                       , data = Datos_Segundo_Modelo)

# Calcular la importancia de las características
importancia_caracteristicas1 <- modelo_arbol1$variable.importance

# Visualizar la importancia de las características
print(importancia_caracteristicas1)

predictors2 <- colnames(Datos_Segundo_Modelo)[-8]

train.data2 <- Datos_Segundo_Modelo[sample.index2, c(predictors2, "HeartDiseaseorAttack"), drop = FALSE]
test.data2 <- Datos_Segundo_Modelo[-sample.index2, c(predictors2, "HeartDiseaseorAttack"), drop = FALSE]

train.data2$HeartDiseaseorAttack <- factor(train.data2$HeartDiseaseorAttack)
test.data2$HeartDiseaseorAttack <- factor(test.data2$HeartDiseaseorAttack)

ctrl2 <- trainControl(method = "cv"
                      , p = 0.7, number=10)
knnFit2 <- train(HeartDiseaseorAttack ~ .
                 , data = train.data2
                 , method = "knn", trControl = ctrl2
                 , preProcess = c("center", "scale") # for z-score
                 , tuneLength = 30)

knnFit2
plot(knnFit2)

# Make predictions
knnPredict2 <- predict(knnFit2
                       , newdata = test.data2)

# Creates the confusion matrix
confusionMatrix(data = knnPredict2
                , reference = test.data2$HeartDiseaseorAttack)

#Remover 5 predictors
Ret_5_Predicts2 <- c("Sex", "HvyAlcoholConsump", "Fruits", "NoDocbcCost", "Veggies")
train.data2_1 <- train.data2[, !(names(train.data2) %in% Ret_5_Predicts2)]
test.data2_1 <- test.data2[, !(names(test.data2) %in% Ret_5_Predicts2)]

ctrl2_1 <- trainControl(method = "cv",p = 0.7, number = 5)
knnFit2_1 <- train(HeartDiseaseorAttack ~ .
                   , data = train.data2_1
                   , method = "knn", trControl = ctrl2_1
                   , preProcess = c("center", "scale") # for z-score
                   , tuneLength = 30)
knnFit2_1
plot(knnFit2_1)

knnPredict2_1 <- predict(knnFit2_1,
                         newdata = test.data2_1)

confusionMatrix(data = knnPredict2_1
                , reference = test.data2_1$HeartDiseaseorAttack)

#Remover 5 predictores mas
Ret_5_Predicts2_1 <- c("PhysActivity"
                       ,"Diabetes_012"
                       ,"CholCheck"
                       ,"AnyHealthcare"
                       ,"Stroke")

train.data2_2 <- train.data2_1[, !(names(train.data2_1) %in% Ret_5_Predicts2_1)]
test.data2_2 <- test.data2_1[, !(names(test.data2_1) %in% Ret_5_Predicts2_1)]

ctrl2_2 <- trainControl(method = "repeatedcv"
                        , p = 0.7
                        , number = 10
                        , repeats = 3)

knnFit2_2 <- train(HeartDiseaseorAttack ~ .
                   , data = train.data2_2
                   , method = "knn", trControl = ctrl2_2
                   , preProcess = c("range") # c("center", "scale") for z-score
                   , tuneLength = 30)
knnFit2_2
plot(knnFit2_2)

knnPredict2_2 <- predict(knnFit2_2
                         , newdata = test.data2_2)

confusionMatrix(data = knnPredict2_2
                , reference = test.data2_2$HeartDiseaseorAttack)

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

# Crear un modelo de árbol de decisión para determinar los datos mas relevantes
modelo_arbol2 <- rpart(Sex ~ .
                       , data = Datos_Tercer_Modelo)

# Calcular la importancia de las características
importancia_caracteristicas2 <- modelo_arbol2$variable.importance

# Visualizar la importancia de las características
print(importancia_caracteristicas2)

predictors3 <- colnames(Datos_Tercer_Modelo)[-19]

train.data3 <- Datos_Tercer_Modelo[sample.index3, c(predictors3, "Sex"), drop = FALSE]
test.data3 <- Datos_Tercer_Modelo[-sample.index3, c(predictors3, "Sex"), drop = FALSE]

train.data3$Sex <- factor(train.data3$Sex)
test.data3$Sex <- factor(test.data3$Sex)

ctrl3 <- trainControl(method = "cv"
                      , p = 0.7
                      , number = 10)

knnFit3 <- train(Sex ~ .
                 , data = train.data3
                 , method = "knn", trControl = ctrl3
                 , preProcess = c("center", "scale") # for z-score
                 , tuneLength = 50)

knnFit3
plot(knnFit3)

knnPredict3 <- predict(knnFit3
                       , newdata = test.data3)

confusionMatrix(data = knnPredict3
                , reference = test.data3$Sex)

#Remover 5 predictors
Ret_5_Predicts3 <- c("Stroke"
                     , "CholCheck"
                     , "AnyHealthcare"
                     , "NoDocbcCost"
                     , "Smoker")

train.data3_1 <- train.data3[, !(names(train.data3) %in% Ret_5_Predicts3)]
test.data3_1 <- test.data3[, !(names(test.data3) %in% Ret_5_Predicts3)]

ctrl3_1 <- trainControl(method = "cv"
                        ,p = 0.7, number = 5)
knnFit3_1 <- train(Sex ~ .
                   , data = train.data3_1
                   , method = "knn", trControl = ctrl3_1
                   , preProcess = c("center", "scale") # for z-score
                   , tuneLength = 30)

knnFit3_1
plot(knnFit3_1)

knnPredict3_1 <- predict(knnFit3_1
                         , newdata = test.data3_1)

confusionMatrix(data = knnPredict3_1
                , reference = test.data3_1$Sex)

#Remover 5 predictores mas
Ret_5_Predicts3_1 <- c("Diabetes_012"
                       ,"HighBP"
                       ,"HighChol"
                       ,"HeartDiseaseorAttack"
                       , "MentHlth")

train.data3_2 <- train.data3_1[, !(names(train.data3_1) %in% Ret_5_Predicts3_1)]
test.data3_2 <- test.data3_1[, !(names(test.data3_1) %in% Ret_5_Predicts3_1)]

ctrl3_2 <- trainControl(method = "repeatedcv"
                        , number = 10, repeats = 3)
knnFit3_2 <- train(Sex ~ .
                   , data = train.data3_2
                   , method = "knn"
                   , trControl = ctrl3_2
                   , preProcess = c("center", "scale") # for z-score
                   , tuneLength = 30)
knnFit3_2
plot(knnFit3_2)


knnPredict3_2 <- predict(knnFit3_2
                         , newdata = test.data3_2)

confusionMatrix(data = knnPredict3_2
                , reference = test.data3_2$Sex)


########### TERCER PUNTO Y ULTIMO :)

Predictors_Punto3 <- colnames(muestra)[-5]

sample.index_1 <- sample(1:nrow(muestra)
                       ,nrow(muestra)*0.7
                       ,replace = F)

train.data_Punto3 <- muestra[sample.index_1,c(Predictors_Punto3,"BMI"),drop=F]
test.data_Punto3 <- muestra[-sample.index_1,c(Predictors_Punto3,"BMI"),drop=F]
ins_model <- lm(BMI ~ .
                , data = train.data_Punto3)
ins_model
summary(ins_model)

train.control_Punto3 <- trainControl(method = "cv"
                                     , p = 0.7
                                     , number = 10 )

model_Punto3 <- train(BMI ~ ., data = train.data_Punto3
                      , method = "lm"
                      , trControl = train.control_Punto3)

print(model_Punto3)

#### Segundo Punto del tercero

Ret_5_Predicts_Punto3 <- c("Income"
                           ,"Sex"
                           ,"MentHlth"
                           ,"AnyHealthcare"
                           ,"HvyAlcoholConsump"
                           ,"NoDocbcCost"
                           ,"PhysHlth"
                           ,"Education"
                           ,"Veggies"
                           ,"Fruits"
                           ,"Smoker"
                           ,"CholCheck"
                           ,"HighChol")

train.data_Punto3_1 <- train.data_Punto3[, !(names(train.data_Punto3) %in% Ret_5_Predicts_Punto3)]
test.data_Punto3_1 <- test.data_Punto3[, !(names(test.data_Punto3) %in% Ret_5_Predicts_Punto3)]

ins_model2 <- lm(BMI ~ ., data = train.data_Punto3_1)

summary(ins_model2)

train.control_Punto3_1 <- trainControl(method = "cv", p = 0.7, number = 5)
model_Punto3_1 <- train(BMI ~ ., data = train.data_Punto3_1, method = "lm",
               trControl = train.control_Punto3_1)

print(model_Punto3_1)

############## Tercero del tercero
Ret_5_Predicts_Punto3_1 <- c("Diabetes_012"
                             , "Stroke"
                             , "PhysActivity"
                             , "GenHlth"
                             , "DiffWalk"
                             , "Age")

train.data_Punto3_2 <- train.data_Punto3_1[, !(names(train.data_Punto3_1) %in% Ret_5_Predicts_Punto3_1)]
test.data_Punto3_2 <- test.data_Punto3_1[, !(names(test.data_Punto3_1) %in% Ret_5_Predicts_Punto3_1)]

ins_model3 <- lm(BMI ~ ., data = train.data_Punto3_2)

summary(ins_model3)

# Entrenar el modelo
train.control_Punto3_2 <- trainControl(method = "repeatedcv", p = 0.7, number = 3, repeats = 10)
model_Punto3_2 <- train(BMI ~ HighBP
                        ,data = train.data_Punto3_2
                        , method = "lm",
               trControl = train.control_Punto3_2)

print(model_Punto3_2)
summary (model_Punto3_2)

### Regresion linear

set.seed(1)
Muestra2 <- diabetes_012[sample(nrow(diabetes_012), 2536), ]

predictors_Rl <- colnames(Muestra2)[-16]
sample.index_Rl <- sample(1:nrow(Muestra2),
                       nrow(Muestra2) * 0.7,
                       replace = FALSE)

train.data_Rl <- Muestra2[sample.index_Rl, c(predictors, "MentHlth"), drop = FALSE]
test.data_Rl <- Muestra2[-sample.index_Rl, c(predictors, "MentHlth"), drop = FALSE]

ins_model_Rl <- lm(MentHlth ~ ., data = train.data_Rl)
summary(ins_model_Rl)

train.control_Rl <- trainControl(method = "cv", number = 10 )
model_Rl <- train(MentHlth ~ ., data = train.data_Rl, method = "lm",
               trControl = train.control_Rl)

print(model_Rl)

###se retiran las variables que no aportan al modelo

Pred_Remov__Rl <- c("Fruits"
                    , "BMI"
                    , "HeartDiseaseorAttack"
                    , "Veggies"
                    , "HvyAlcoholConsump")

train.data_Rl_1 <- train.data_Rl[, !(names(train.data_Rl) %in% Pred_Remov__Rl)]
test.data_Rl_1 <- test.data_Rl[, !(names(test.data_Rl) %in% Pred_Remov__Rl)]

ins_model_Rl_1 <- lm(MentHlth ~ ., data = train.data_Rl_1)
summary(ins_model_Rl_1)


train.control_Rl_1 <- trainControl(method = "cv", number = 5)
model_Rl_1 <- train(MentHlth ~ ., data = train.data_Rl_1, method = "lm",
               trControl = train.control_Rl_1)


print(model_Rl_1)

#### Se retiran las variables que no aportan al modelo
Pred_Remov__Rl_1 <- c("CholCheck"
                      ,"HighBP"
                      ,"PhysActivity"
                      ,"AnyHealtcare"
                      ,"Education")

train.data_Rl_2 <- train.data_Rl_1[, !(names(train.data_Rl_1) %in% Pred_Remov__Rl_1)]
test.data_Rl_2 <- test.data_Rl_1[, !(names(test.data_Rl_1) %in% Pred_Remov__Rl_1)]

ins_model_Rl_2 <- lm(MentHlth ~ ., data = train.data_Rl_2)
summary(ins_model_Rl_2)

train.control_Rl_2 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
model_Rl_2 <- train(MentHlth ~ ., data = train.data_Rl_2, method = "lm",
               trControl = train.control_Rl_2)

print(model_Rl_2)





