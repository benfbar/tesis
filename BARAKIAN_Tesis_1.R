#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 1 = Preprocesamiento de datos - Hoja de cálculo original

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

#LIBRERIAS
library(readxl)
library(forcats)
library(ggplot2)
library(gridExtra)
library(caret)
library(randomForest)
library(cluster)
library(GGally)
library(dplyr)
library(FSA)

#Se levanta el archivo y se recodifican las variables
datos <- read_excel("0_Dataset.xlsx", sheet = 1)
datos <- datos[, 1:6]
head(datos)
colnames(datos) <- c("ID", "Edad", "Sexo", "PT", "Alb_Q", "PPS")
str(datos)
datos$ID <- as.integer(datos$ID)
datos$Sexo <- as.factor(datos$Sexo)
datos$PPS <- as.factor(datos$PPS)
str(datos)

# Renombrar las categorías de la columna PPS

datos$PPS <- fct_recode(datos$PPS,
                        "2_RFA" = "Acute phase protein",
                        "3_HIPOPTG" = "Hypoproteinemia",
                        "6_GM" = "Monoclonal gammopathy",
                        "4_SN" = "Nephrotic syndrome",
                        "1_CSSP" = "Normal",
                        "5_HIPERG" = "Polyclonal gammopathy"
)
str(datos)

# Asegurarse de que las categorías están en el orden correcto
datos$PPS <- factor(datos$PPS, levels = c("1_CSSP", "2_RFA", "3_HIPOPTG", "4_SN", "5_HIPERG", "6_GM"))

# Verificar los nuevos niveles de la columna PPS
levels(datos$PPS)

#ANALISIS EXPLORATORIO

dim(datos)
str(datos)
summary(datos[, 2:6])
summary(datos[, 6:6])


colSums(is.na(datos))

#GRAFICOS EXPLORATORIOS

# Histogramas y boxplot de variables numéricas


# Histogramas
hist_edad <- ggplot(datos, aes(x = Edad)) + 
  geom_histogram(bins = 20, fill = "blue", color = "black") + 
  ggtitle("Distribución de Edad")

hist_PT <- ggplot(datos, aes(x = PT)) + 
  geom_histogram(bins = 20, fill = "green", color = "black") + 
  ggtitle("Distribución de PT")

hist_Alb_Q <- ggplot(datos, aes(x = Alb_Q)) + 
  geom_histogram(bins = 20, fill = "purple", color = "black") + 
  ggtitle("Distribución de Alb_Q")

# Boxplots
box_edad <- ggplot(datos, aes(y = Edad)) + 
  geom_boxplot(fill = "blue", color = "black") + 
  ggtitle("Boxplot de Edad")

box_PT <- ggplot(datos, aes(y = PT)) + 
  geom_boxplot(fill = "green", color = "black") + 
  ggtitle("Boxplot de PT")

box_Alb_Q <- ggplot(datos, aes(y = Alb_Q)) + 
  geom_boxplot(fill = "purple", color = "black") + 
  ggtitle("Boxplot de Alb_Q")

# Mostrar histogramas y boxplots en paralelo
grid.arrange(hist_edad, box_edad, hist_PT, box_PT, hist_Alb_Q, box_Alb_Q, ncol = 2)

# Distribución de 'Sexo'
table(datos$Sexo)

# Distribución de 'PPS'
table(datos$PPS)

# Gráfico de barras 'Sexo'
ggplot(datos, aes(x = Sexo, fill = Sexo)) + 
  geom_bar() + 
  scale_fill_manual(values = c("Female" = "darkviolet", "Male" = "darkgreen")) + 
  ggtitle("Distribución de Sexo") +
  theme_minimal()

# Colores personalizados para PPS
colores <- c(
  "1_CSSP" = "green", 
  "2_RFA" = "yellow", 
  "3_HIPOPTG" = "lightblue", 
  "4_SN" = "orange", 
  "5_HIPERG" = "purple", 
  "6_GM" = "red"
)

# Gráfico de barras 'PPS'
ggplot(datos, aes(x = PPS, fill = PPS)) + 
  geom_bar() + 
  scale_fill_manual(values =colores) + 
  ggtitle("Distribución de PPS") + 
  theme_minimal()

#ANALISIS BIVARIADOS

# Matriz de correlación
cor(datos[, c("Edad", "PT", "Alb_Q")], use = "complete.obs")

# Gráficos de dispersión
pairs(datos[, c("Edad", "PT", "Alb_Q")])

# Crear una paleta de colores personalizada
colores_puntos <- colores[as.character(datos$PPS)]

# Gráfico de dispersión con colores según PPS usando pairs
pairs(datos[, c("Edad", "PT", "Alb_Q")], 
      col = colores_puntos, 
      pch = 16)  # pch=16 es para puntos sólidos

# Boxplot para Edad por PPS
ggplot(datos, aes(x = PPS, y = Edad, fill = PPS)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_fill_manual(values = colores) + 
  ggtitle("Edad por PPS (con outliers)") +
  theme_minimal()


# Boxplot para PT por PPS
ggplot(datos, aes(x = PPS, y = PT, fill = PPS)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_fill_manual(values = colores) + 
  ggtitle("PT por PPS (con outliers)") +
  theme_minimal()

# Boxplot para Alb_Q por PPS
ggplot(datos, aes(x = PPS, y = Alb_Q, fill = PPS)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_fill_manual(values = colores) + 
  ggtitle("Alb_Q por PPS (con outliers)") +
  theme_minimal()

#estadistica

# Normalidad para Edad
shapiro.test(datos$Edad) #no es normal

# Normalidad para PT
shapiro.test(datos$PT) #no es normal

# Normalidad para Alb_Q
shapiro.test(datos$Alb_Q) #no es normal
 
# Kruskal-Wallis para Edad
kruskal.test(Edad ~ PPS, data = datos)

# Kruskal-Wallis para PT
kruskal.test(PT ~ PPS, data = datos)

# Kruskal-Wallis para Alb_Q
kruskal.test(Alb_Q ~ PPS, data = datos)


# Dunn para Edad
dunnTest(Edad ~ PPS, data = datos, method = "bonferroni")

# Dunn para PT
dunnTest(PT ~ PPS, data = datos, method = "bonferroni")

# Dunn para Alb_Q
dunnTest(Alb_Q ~ PPS, data = datos, method = "bonferroni")

#sexo por PPS
table(datos$Sexo, datos$PPS)

ggplot(datos, aes(x = PPS, fill = Sexo)) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("Female" = "darkviolet", "Male" = "darkgreen")) +
  labs(title = "Distribución de Sexo por PPS", x = "PPS", y = "Frecuencia") +
  theme_minimal()


# Calcular proporciones
datos_porcentaje <- datos %>%
  group_by(PPS, Sexo) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  group_by(PPS) %>%
  mutate(Porcentaje = Frecuencia / sum(Frecuencia) * 100)

ggplot(datos_porcentaje, aes(x = PPS, y = Porcentaje, fill = Sexo)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Female" = "darkviolet", "Male" = "darkgreen")) +
  labs(title = "Distribución de Sexo por PPS (%)", x = "PPS", y = "Porcentaje") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Crear tabla de contingencia
tabla_Sexo_PPS <- table(datos$Sexo, datos$PPS)

# Test de Chi-cuadrado
chisq_test <- chisq.test(tabla_Sexo_PPS)

# Resultados
chisq_test


#Correccion outlier Alb_Q

# Filtrar el grupo RFA
datos_RFA <- subset(datos, PPS == "2_RFA")

# Excluir el outlier
datos_RFA_clean <- subset(datos_RFA, Alb_Q != 0)

# Ajustar modelo lineal
modelo_RFA <- lm(Alb_Q ~ PT, data = datos_RFA_clean)

# Resumen del modelo
summary(modelo_RFA)

# Obtener el PT del outlier
PT_outlier <- datos_RFA$PT[datos_RFA$Alb_Q == 0]
PT_outlier
# Predecir el valor de Alb_Q usando el modelo
Alb_Q_predicho <- predict(modelo_RFA, newdata = data.frame(PT = PT_outlier))

# Verificar el valor predicho
Alb_Q_predicho

# Reemplazar el outlier en el dataset original
datos$Alb_Q[datos$PPS == "2_RFA" & datos$Alb_Q == 0] <- Alb_Q_predicho

# Verificar cambios en Alb_Q
summary(datos$Alb_Q)

# Boxplot para Alb_Q por PPS
ggplot(datos, aes(x = PPS, y = Alb_Q, fill = PPS)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 2) +
  scale_fill_manual(values = colores) + 
  ggtitle("Alb_Q por PPS (con outliers)") +
  theme_minimal()

####Otros graficos

ggplot(datos, aes(x = Edad, fill = Sexo)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ PPS) +
  labs(title = "Distribución de Edad por Sexo y PPS", x = "Edad", y = "Densidad") +
  theme_minimal()

ggplot(datos, aes(x = PT, fill = Sexo)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ PPS) +
  labs(title = "Distribución de PT por Sexo y PPS", x = "PT", y = "Densidad") +
  theme_minimal()

ggplot(datos, aes(x = Alb_Q, fill = Sexo)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ PPS) +
  labs(title = "Distribución de Alb_Q por Sexo y PPS", x = "Alb_Q", y = "Densidad") +
  theme_minimal()


ggpairs(datos, columns = c("Edad", "PT", "Alb_Q"), 
        aes(color = PPS)) +
  theme_minimal() +
  labs(title = "Correlaciones entre Edad, PT y Alb_Q por PPS")


#Clustering exploratorio


# Clustering con k-means
kmeans_result <- kmeans(scale(datos[, c("Edad", "PT", "Alb_Q")]), centers = 6)

datos$Cluster <- as.factor(kmeans_result$cluster)

# Gráfico 1: Clustering basado en PT y Alb_Q
grafico_cluster <- ggplot(datos, aes(x = PT, y = Alb_Q, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "Clustering basado en PT y Alb_Q", x = "PT", y = "Alb_Q") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Usar una paleta atractiva

grafico_pps <- ggplot(datos, aes(x = PT, y = Alb_Q, color = PPS)) +
  geom_point(size = 2) +
  labs(title = "Relación PT/Alb_Q por PPS", x = "PT", y = "Alb_Q") +
  theme_minimal() +
  scale_color_manual(values = colores)
  
grid.arrange(grafico_cluster, grafico_pps, ncol = 2)

#RF exploratorio

# Dividir en conjunto de entrenamiento (80%) y prueba (20%) en función de PPS
set.seed(35760768);indices_train <- createDataPartition(datos$PPS, p = 0.8, list = FALSE)

# Dividir los datos
datos_train <- datos[indices_train, ]
datos_test <- datos[-indices_train, ]

# Entrenar modelo de Random Forest

modelo_rf <- randomForest(PPS ~ Edad + PT + Alb_Q + Sexo, data = datos_train, importance = TRUE)

# Visualizar importancia de las variables
varImpPlot(modelo_rf)

# Predecir en el conjunto de prueba
predicciones <- predict(modelo_rf, newdata = datos_test)

# Crear tabla de contingencia (Predicciones vs Reales)
tabla_contingencia <- table(Predicción = predicciones, Real = datos_test$PPS)
print("Tabla de contingencia:")
print(tabla_contingencia)

# Calcular métricas de evaluación
conf_matrix <- confusionMatrix(tabla_contingencia)
print("Métricas de evaluación:")
print(conf_matrix)

## Generar registros para compensar imagen de mas en los grupos 3 y 4

# Función para calcular el sexo minoritario y los valores promedio con redondeo
crear_registro_ficticio <- function(ID,grupo) {
  # Filtrar los datos del grupo
  datos_grupo <- filter(datos, PPS == grupo)
  
  # Calcular el sexo minoritario
  sexo_min <- names(sort(table(datos_grupo$Sexo)))[1]  # El primero en la tabla ordenada por frecuencia
  
  # Calcular los valores promedio de Edad, PT y Alb_Q
  promedio <- datos_grupo %>%
    summarise(Edad = round(mean(Edad, na.rm = TRUE)),    # Redondear Edad a entero
              PT = round(mean(PT, na.rm = TRUE), 1),     # Redondear PT a 1 decimal
              Alb_Q = round(mean(Alb_Q, na.rm = TRUE), 1))  # Redondear Alb_Q a 1 decimal
  
  # Crear el registro ficticio como un data frame con las mismas columnas y tipos
  registro_ficticio <- data.frame(ID,
                                  Edad = promedio$Edad,
                                  PT = promedio$PT,
                                  Alb_Q = promedio$Alb_Q,
                                  Sexo = sexo_min,
                                  PPS = grupo)
  
  # Devolver el registro ficticio
  return(registro_ficticio)
}

# Generar registros ficticios para 3_HIPOPTG y 4_SN
registro_ficticio_SN <- crear_registro_ficticio(1290,"4_SN")
registro_ficticio_SN

# Agregar los registros ficticios al dataset original
datos <- bind_rows(datos, registro_ficticio_SN)

summary(datos)
datos$PPS <- as.factor(datos$PPS)
summary(datos)

datos$Alb_Q_Porcentaje <- (datos$Alb_Q / datos$PT) * 100
datos

save(datos,file = "datos.RData")
