#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 7 --- Training/Testing de Modelos de ML para PPS

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

#LIBRERIAS
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(nnet)
library(NeuralNetTools)
library(lightgbm)
library(e1071)


load("dataset.RData")
str(dataset)
dataset <- dataset %>% select(-CM_L1, -CM_L2)

#Particionar 20/80%
set.seed(35760768);part=createDataPartition(dataset$PPS,p=0.80,list=F)

entreno=dataset[part,]
testeo=dataset[-part,]

summary(entreno$PPS)
summary(testeo$PPS)

#######################################################################################
# AdD
arbol=rpart(PPS~.,entreno)
rpart.plot(arbol)
pred=predict(arbol,testeo,type="class")
confusionMatrix(pred,testeo$PPS)

arbol$control

arbol2=rpart(PPS~.,entreno,cp=0,minsplit=0)  #minsplit 0
rpart.plot(arbol2)
plotcp(arbol2)
printcp(arbol2)
pred2=predict(arbol2,testeo,type="class")
confusionMatrix(pred2,testeo$PPS)

# Entrenar el modelo con distintos cp (es lo unico que permite hacer grid caret)
modelo_AdD_tunegrid <- train(
  PPS ~ ., 
  data = entreno,
  method = "rpart", maxdepth=15,
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = expand.grid(
    cp = c(0.005, 0.01, 0.05) 
))

# Resumen de resultados
modelo_AdD_tunegrid
rpart.plot(modelo_AdD_tunegrid$finalModel)

#######################################################################################
#RF
set.seed(35760768);RF=randomForest(PPS~.,entreno,ntree=2000,mtry=20)
plot(RF)
RF
varImpPlot(RF,pch=19)
pred=predict(RF,testeo,type="class")
confusionMatrix(pred,testeo$PPS)

#######################################################################################
#lightGBM

# Crear las matrices para LightGBM
entreno$Sexo <- ifelse(entreno$Sexo == "Female", 0, 1)
testeo$Sexo <- ifelse(testeo$Sexo == "Female", 0, 1)
dtrain <- lgb.Dataset(data = as.matrix(entreno[,-1]), label = as.numeric(entreno$PPS) - 1)
dtest <- as.matrix(testeo[,-1])

# Definir hiperparámetros
params <- list(
  objective = "multiclass",  # Clasificación multiclase
  metric = "multi_logloss",  # Métrica de evaluación
  num_class = length(unique(entreno$PPS)),  # Número de clases
  learning_rate = 0.05,  # Tasa de aprendizaje
  num_leaves = 31,  # Número de hojas en los árboles
  feature_fraction = 0.8,  # Fracción de características usadas
  bagging_fraction = 0.8,  # Fracción de datos usados en cada árbol
  bagging_freq = 5,  # Frecuencia de bagging
  max_depth = -1,  # Profundidad del árbol (-1 significa sin límite)
  verbosity = -1  # Suprimir mensajes de advertencia
)

# Entrenar el modelo
set.seed(35760768);lgb_model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000,  # Número de iteraciones
  valids = list(train = dtrain),
  early_stopping_rounds = 50  # Detener si no hay mejora
)

# Predicción sobre testeo
pred_probs <- predict(lgb_model, dtest)  # Probabilidades
pred_class <- max.col(matrix(pred_probs, ncol = length(unique(entreno$PPS))))  # Obtener clase predicha

# Evaluar con matriz de confusión
testeo$PPS <- as.numeric(testeo$PPS)  # Convertir a numérico para comparar
conf_matrix <- confusionMatrix(factor(pred_class), factor(testeo$PPS))
print(conf_matrix)

lgb.importance(lgb_model) %>% 
  lgb.plot.importance(top_n = 20, measure = "Gain")

# Crear DataFrame con predicciones y valores reales
df_pred <- data.frame(
  Real = factor(testeo$PPS),
  Predicho = factor(pred_class)
)

# Gráfico de dispersión con jitter para visualizar mejor las clases
ggplot(df_pred, aes(x = Real, y = Predicho)) +
  geom_jitter(alpha = 0.5, width = 0.2, height = 0.2) +
  labs(title = "Comparación Predicciones vs. Valores Reales",
       x = "Clase Real", y = "Clase Predicha") +
  theme_minimal()

library(caret)
library(reshape2)

# Matriz de Confusión
conf_matrix
conf_table <- as.data.frame(conf_matrix$table)

# Graficar matriz de confusión
ggplot(conf_table, aes(Reference, Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Matriz de Confusión de LightGBM", x = "Clase Real", y = "Clase Predicha") +
  theme_minimal()


###############################################
#lightgbm con balanceo de clases:

params <- list(
  class_weight <- "balanced",
  objective = "multiclass",
  metric = "multi_logloss",
  num_class = length(unique(entreno$PPS)),
  learning_rate = 0.05,
  num_leaves = 30,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 5,
  max_depth = -1,
  verbosity = -1
)

lgb_model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 2000,
  valids = list(train = dtrain),
  early_stopping_rounds = 50
)

# Predicción sobre testeo
pred_probs <- predict(lgb_model, dtest)  # Probabilidades
pred_class <- max.col(matrix(pred_probs, ncol = length(unique(entreno$PPS))))  # Obtener clase predicha

# Evaluar con matriz de confusión
conf_matrix <- confusionMatrix(factor(pred_class), factor(testeo$PPS))
print(conf_matrix)

#####################
#optimizacion

# Definir un grid de parámetros
params_grid <- list(
  learning_rate = c(0.01, 0.05, 0.1),
  #num_leaves = c(31, 50, 128),
  max_depth = c(-1, 10, 15),
  feature_fraction = c(0.6, 0.8, 1.0)
  #bagging_fraction = c(0.6, 0.8, 1.0),
  #bagging_freq = c(5, 10),
  #lambda_l1 = c(0, 0.5, 1),
  #lambda_l2 = c(0, 0.5, 1),
  #min_data_in_leaf = c(20, 50, 100)
)

train_lightgbm <- function(learning_rate, num_leaves, max_depth, feature_fraction, 
                           bagging_fraction, bagging_freq, lambda_l1, lambda_l2, min_data_in_leaf) {
  
  # Definir los parámetros del modelo
  params <- list(
    objective = "multiclass",
    metric = "multi_logloss",
    num_class = length(unique(entreno$PPS)),
    learning_rate = learning_rate,
    num_leaves = num_leaves,
    max_depth = max_depth,
    feature_fraction = feature_fraction,
    bagging_fraction = bagging_fraction,
    bagging_freq = bagging_freq,
    lambda_l1 = lambda_l1,
    lambda_l2 = lambda_l2,
    min_data_in_leaf = min_data_in_leaf,
    verbosity = -1
  )
  
  # Convertir datos de entrenamiento y validación a formato lgb.Dataset
  dtrain <- lgb.Dataset(data = as.matrix(entreno[,-c(1, ncol(entreno))]), label = as.numeric(entreno$PPS) - 1)
  dtest <- lgb.Dataset(data = as.matrix(testeo[,-c(1, ncol(testeo))]), label = as.numeric(testeo$PPS) - 1)
  
  # Entrenar el modelo con early stopping utilizando el conjunto de validación
  model <- lgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    valids = list(test = dtest),  # Asegurarse de que el conjunto de validación esté correctamente pasado
    early_stopping_rounds = 10  # Detenerse si no mejora en 10 rondas
  )
  
  # Predecir en el conjunto de test (usamos la matriz, no lgb.Dataset)
  pred_probs <- predict(model, as.matrix(testeo[,-c(1, ncol(testeo))]))  # Usar matriz de datos
  
  # Calcular logloss (asegurándose de que se usa la probabilidad de cada clase)
  logloss <- -mean(log(pred_probs + 1e-15))  # Evitar log(0) añadiendo un pequeño valor
  
  return(logloss)
}

# Realizar Grid Search
results <- expand.grid(params_grid)  # Combina todas las combinaciones
results$logloss <- apply(results, 1, function(x) {
  train_lightgbm(x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9])
})

# Ver los mejores parámetros
best_params <- results[which.min(results$logloss),]





######################################################################################
#RN

set.seed(35760768); red=nnet(PPS~., entreno,size=30, maxit=200, MaxNWts=12000)
pred=predict(red, testeo, type="class")
pred <- factor(pred, levels = levels(testeo$PPS))
#plotnet(red)
confusionMatrix(pred,testeo$PPS)

#set.seed(35760768); red=nnet(PPS~., entreno,size=100, maxit=200, MaxNWts=40000)   #no corre casi
#pred=predict(red, testeo, type="class")
#pred <- factor(pred, levels = levels(testeo$PPS))
#confusionMatrix(pred,testeo$PPS)

# Estandarizar los datos (si es necesario)
entreno$Sexo <- ifelse(entreno$Sexo == "Female", 0, 1)
testeo$Sexo <- ifelse(testeo$Sexo == "Female", 0, 1)

entreno_scaled <- scale(entreno[,-1])  # Excluir la variable dependiente (PPS)
var_entreno <- apply(entreno_scaled, 2, var)
entreno_scaled_clean <- entreno_scaled[, var_entreno > 0]
entreno_scaled_clean_no_na <- entreno_scaled_clean[, colSums(is.na(entreno_scaled_clean)) == 0]
dim(entreno_scaled_clean_no_na) #curva 1, L_6

# Aplicar PCA sobre los datos limpiados
pca <- prcomp(entreno_scaled_clean_no_na, center = TRUE, scale. = TRUE)

# Ver el resumen del PCA
summary(pca)

# Biplot de los primeros dos componentes principales
biplot(pca)

# Gráfico de la varianza acumulada
plot(cumsum(pca$sdev^2) / sum(pca$sdev^2), xlab = "Número de Componentes", ylab = "Proporción de Varianza Acumulada", type = "b")
cumsum(summary(pca)$importance[2,])  #99$ PCA37
num_components <- which(cumsum(summary(pca)$importance[2,]) >= 0.99)[1]
num_components

# Proyectar los datos en los componentes principales
entreno_pca <- predict(pca, newdata = entreno_scaled_clean_no_na)
entreno_pca_reduced <- entreno_pca[, 1:num_components]

# Preparar los datos para entrenar la red neuronal
entreno_pca_df <- data.frame(PPS = entreno$PPS, entreno_pca_reduced)

# Entrenar la red neuronal con las componentes principales
set.seed(35760768);red_pca <- nnet(PPS ~ ., data = entreno_pca_df, size = 30, maxit = 200, MaxNWts = 2000)

# Ver los resultados
summary(red_pca)

# Predicciones con el conjunto de test
testeo_clean <- testeo[, !(names(testeo) %in% c("curva_1", "L_6"))]
dim(testeo_clean)
test_scaled <- scale(testeo_clean[,-1])
dim(test_scaled)
test_pca <- predict(pca, newdata = test_scaled)  # Proyección en PCA
test_pca_reduced <- test_pca[, 1:num_components]  # Seleccionar las mismas componentes

# Predecir con la red neuronal
pred <- predict(red_pca, newdata = data.frame(test_pca_reduced), type = "class")
pred <- factor(pred, levels = levels(testeo$PPS))

# Crear la matriz de confusión
confusionMatrix(pred, testeo$PPS)










# Entrenar la red neuronal con las componentes principales
set.seed(35760768);red_pca <- nnet(PPS ~ ., data = entreno_pca_df, size = 100, maxit = 200, MaxNWts = 5000)

# Ver los resultados
summary(red_pca)

# Predicciones con el conjunto de test
testeo_clean <- testeo[, !(names(testeo) %in% c("curva_1", "L_6"))]
dim(testeo_clean)
test_scaled <- scale(testeo_clean[,-1])
dim(test_scaled)
test_pca <- predict(pca, newdata = test_scaled)  # Proyección en PCA
test_pca_reduced <- test_pca[, 1:num_components]  # Seleccionar las mismas componentes

# Predecir con la red neuronal
pred <- predict(red_pca, newdata = data.frame(test_pca_reduced), type = "class")
pred <- factor(pred, levels = levels(testeo$PPS))

# Crear la matriz de confusión
confusionMatrix(pred, testeo$PPS)













# Entrenar la red neuronal con las componentes principales
set.seed(35760768);red_pca <- nnet(PPS ~ ., data = entreno_pca_df, size = 250, maxit = 200, MaxNWts = 12000)

# Ver los resultados
summary(red_pca)

# Predicciones con el conjunto de test
testeo_clean <- testeo[, !(names(testeo) %in% c("curva_1", "L_6"))]
dim(testeo_clean)
test_scaled <- scale(testeo_clean[,-1])
dim(test_scaled)
test_pca <- predict(pca, newdata = test_scaled)  # Proyección en PCA
test_pca_reduced <- test_pca[, 1:num_components]  # Seleccionar las mismas componentes

# Predecir con la red neuronal
pred <- predict(red_pca, newdata = data.frame(test_pca_reduced), type = "class")
pred <- factor(pred, levels = levels(testeo$PPS))

# Crear la matriz de confusión
confusionMatrix(pred, testeo$PPS)



#########################################################################################################
#SVM

entreno_clean <- entreno[, !(names(entreno) %in% c("curva_1", "L_6"))]

svm_model <- svm(PPS ~ .,entreno_clean, kernel = "radial")


# Hacer predicciones sobre el conjunto de datos de prueba (o sobre nuevos datos)
testeo_clean <- testeo[, !(names(testeo) %in% c("curva_1", "L_6"))]


svm_predictions <- predict(svm_model, testeo_clean)

confusionMatrix(svm_predictions, testeo$PPS)

