#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 6 --- Generacion de variables

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

#LIBRERIAS
library(dplyr)
library(tidyr)
library(ggplot2)
library(clue) #algoritmo K-M

load("all_curvas_2.RData")

#Variables 1 a 7 _ Area Bajo Curva Total de la curva y de cada fracción
#F6.1A Función para calcular el área bajo la curva utilizando la regla del trapecio
F6.1A_ABC <- function(x, y) {
  # x: índices (o coordenadas X)
  # y: valores de la curva (o coordenadas Y)
  area <- sum(diff(x) * (y[-length(y)] + y[-1])) / 2  # Regla del trapecio
  return(area)
}

#Variable 2 (x6) _ Area Bajo Curva Total por fracción
#F 6.2 Función para calcular el área total y las áreas entre los límites ajustados
F6.1B_ABC_total_fraccion <- function(curvas, limites_lista) {
  resultados <- list()
  for (i in 1:length(curvas)) {
    curva <- curvas[[i]]  
    indices <- seq_along(curva)  
    area_total <- F6.1A_ABC(indices, curva)  
    limites <- as.numeric(unlist(limites_lista[[i]]))
    areas_limites <- numeric(length(limites))
    
    for (j in seq_along(limites)) {
      if (j == 1) {
        limite_inferior <- 1
        limite_superior <- limites[j]
      } else {
        limite_inferior <- limites[j - 1]
        limite_superior <- limites[j]
      }
      
      sub_indices <- indices[limite_inferior:limite_superior]
      sub_curva <- curva[limite_inferior:limite_superior]
      areas_limites[j] <- F6.1A_ABC(sub_indices, sub_curva)
    }
    
    resultados[[i]] <- list(area_total = area_total, areas_limites = areas_limites)
  }
  
  return(resultados)
}

#Aplicación de F6.1
limites_lista <- lapply(1:nrow(all_curvas), function(i) c(
  all_curvas$L_1[i],
  all_curvas$L_2[i],
  all_curvas$L_3[i],
  all_curvas$L_4[i],
  all_curvas$L_5[i],
  all_curvas$L_6[i]
))
resultados <- F6.1B_ABC_total_fraccion(all_curvas$curva, limites_lista)
all_curvas$A_tot <- sapply(resultados, function(x) x$area_total)
areas_matrix <- do.call(rbind, lapply(resultados, function(x) x$areas_limites))
all_curvas$A_1 <- areas_matrix[, 1]
all_curvas$A_2 <- areas_matrix[, 2]
all_curvas$A_3 <- areas_matrix[, 3]
all_curvas$A_4 <- areas_matrix[, 4]
all_curvas$A_5 <- areas_matrix[, 5]
all_curvas$A_6 <- areas_matrix[, 6]

#all_curvas$A_tot[1]
#all_curvas$A_1[1] + all_curvas$A_2[1] + all_curvas$A_3[1] + all_curvas$A_4[1] +all_curvas$A_5[1] +all_curvas$A_6[1]

summary(all_curvas$A_tot)
summary(all_curvas[, c("A_1", "A_2", "A_3", "A_4", "A_5", "A_6")])
boxplot(all_curvas[, c("A_tot", "A_1", "A_2", "A_3", "A_4", "A_5", "A_6")], 
        main = "Distribución de Áreas Bajo la Curva", 
        ylab = "Área",
        col = c("black",rainbow(6)))

#Variables 8 a 13 _ Porcentaje de cada fracción sobre total
all_curvas$P_1 <- all_curvas$A_1/all_curvas$A_tot*100
all_curvas$P_2 <- all_curvas$A_2/all_curvas$A_tot*100
all_curvas$P_3 <- all_curvas$A_3/all_curvas$A_tot*100
all_curvas$P_4 <- all_curvas$A_4/all_curvas$A_tot*100
all_curvas$P_5 <- all_curvas$A_5/all_curvas$A_tot*100
all_curvas$P_6 <- all_curvas$A_6/all_curvas$A_tot*100

#all_curvas$P_1[100]+all_curvas$P_2[100]+all_curvas$P_3[100]+all_curvas$P_4[100]+all_curvas$P_5[100]+all_curvas$P_6[100]

summary(all_curvas[, c("P_1", "P_2", "P_3", "P_4", "P_5", "P_6")])
boxplot(all_curvas[, c("P_1", "P_2", "P_3", "P_4", "P_5", "P_6")], 
        main = "Distribución de Porcentajes", 
        ylab = "Área",
        col = rainbow(6))
par(mfrow = c(3, 2))
boxplot(all_curvas$P_1 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_1",xlab = "PPS",
        col = "red")
boxplot(all_curvas$P_2 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_2",xlab = "PPS",
        col = "yellow")
boxplot(all_curvas$P_3 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_3",xlab = "PPS",
        col = "green")
boxplot(all_curvas$P_4 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_4",xlab = "PPS",
        col = "cyan")
boxplot(all_curvas$P_5 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_5",xlab = "PPS",
        col = "blue")
boxplot(all_curvas$P_6 ~ all_curvas$PPS, 
        main = "Distribución de Porcentajes", 
        ylab = "P_6",xlab = "PPS",
        col = "violet")
par(mfrow = c(1, 1))

# UNIFICACION DE BASES DE DATOS A PARITR DE P_1 (porcentaje de la fraccion albumina a partir de la curva electroforetica)
# y de Alb_Q_Porcentaje (hoja de calculos)

summary(all_curvas$PPS)
summary(all_curvas$P_1)

load("datos.RData")
datos <- datos[, !names(datos) %in% "Cluster"]
summary(datos$PPS)
summary(datos$Alb_Q_Porcentaje)

par(mfrow = c(1, 2))  
boxplot(all_curvas$P_1 ~ all_curvas$PPS, 
        main = "Distribución de P_1", 
        ylab = "P_1", xlab = "PPS",
        col = "red")
boxplot(datos$Alb_Q_Porcentaje ~ all_curvas$PPS, 
        main = "Distribución de Alb_Q_Porcentaje", 
        ylab = "Alb_Q_Porcentaje", xlab = "PPS",
        col = "red")
par(mfrow = c(1, 1))  

# Primero filtramos por cada categoría específica

PPS1_curvas <- all_curvas[all_curvas$PPS == "1_CSSP", ]
PPS2_curvas <- all_curvas[all_curvas$PPS == "2_RFA", ]
PPS3_curvas <- all_curvas[all_curvas$PPS == "3_HIPOPTG", ]
PPS4_curvas <- all_curvas[all_curvas$PPS == "4_SN", ]
PPS5_curvas <- all_curvas[all_curvas$PPS == "5_HIPERG", ]
PPS6_curvas <- all_curvas[all_curvas$PPS == "6_GM", ]

PPS1_datos <- datos[datos$PPS == "1_CSSP", ]
PPS2_datos <- datos[datos$PPS == "2_RFA", ]
PPS3_datos <- datos[datos$PPS == "3_HIPOPTG", ]
PPS4_datos <- datos[datos$PPS == "4_SN", ]
PPS5_datos <- datos[datos$PPS == "5_HIPERG", ]
PPS6_datos <- datos[datos$PPS == "6_GM", ]

#Simplemente de menor a mayor PPS1
P_1_sorted <- sort(PPS1_curvas$P_1)
Alb_Q_sorted <- sort(PPS1_datos$Alb_Q_Porcentaje)
matched_Alb_Q <- Alb_Q_sorted
error_cuadratico <- sum((P_1_sorted - Alb_Q_sorted)^2)
datos_matched <- data.frame(P_1 = P_1_sorted, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total:", error_cuadratico, "\n")

#Algoritmo de K_M PPS1
PPS1_curvas_sorted <- PPS1_curvas[order(PPS1_curvas$P_1), ]
PPS1_datos_sorted <- PPS1_datos[order(PPS1_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS1_curvas_sorted$P_1, PPS1_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS1_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS1_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS1_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS1_datos_sorted <- PPS1_datos_sorted[matching_indices_numeric, ]
PPS1_curvas_datos = cbind(PPS1_curvas_sorted,PPS1_datos_sorted)

# Algoritmo de K_M PPS2
PPS2_curvas_sorted <- PPS2_curvas[order(PPS2_curvas$P_1), ]
PPS2_datos_sorted <- PPS2_datos[order(PPS2_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS2_curvas_sorted$P_1, PPS2_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS2_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS2_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total PPS2:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS2_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS2_datos_sorted <- PPS2_datos_sorted[matching_indices_numeric, ]
PPS2_curvas_datos = cbind(PPS2_curvas_sorted, PPS2_datos_sorted)

# Algoritmo de K_M PPS3
PPS3_curvas_sorted <- PPS3_curvas[order(PPS3_curvas$P_1), ]
PPS3_datos_sorted <- PPS3_datos[order(PPS3_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS3_curvas_sorted$P_1, PPS3_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS3_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS3_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total PPS3:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS3_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS3_datos_sorted <- PPS3_datos_sorted[matching_indices_numeric, ]
PPS3_curvas_datos = cbind(PPS3_curvas_sorted, PPS3_datos_sorted)

# Algoritmo de K_M PPS4
PPS4_curvas_sorted <- PPS4_curvas[order(PPS4_curvas$P_1), ]
PPS4_datos_sorted <- PPS4_datos[order(PPS4_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS4_curvas_sorted$P_1, PPS4_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS4_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS4_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total PPS4:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS4_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS4_datos_sorted <- PPS4_datos_sorted[matching_indices_numeric, ]
PPS4_curvas_datos = cbind(PPS4_curvas_sorted, PPS4_datos_sorted)

# Algoritmo de K_M PPS5
PPS5_curvas_sorted <- PPS5_curvas[order(PPS5_curvas$P_1), ]
PPS5_datos_sorted <- PPS5_datos[order(PPS5_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS5_curvas_sorted$P_1, PPS5_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS5_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS5_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total PPS5:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS5_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS5_datos_sorted <- PPS5_datos_sorted[matching_indices_numeric, ]
PPS5_curvas_datos = cbind(PPS5_curvas_sorted, PPS5_datos_sorted)

# Algoritmo de K_M PPS6
PPS6_curvas_sorted <- PPS6_curvas[order(PPS6_curvas$P_1), ]
PPS6_datos_sorted <- PPS6_datos[order(PPS6_datos$Alb_Q_Porcentaje), ]
cost_matrix <- outer(PPS6_curvas_sorted$P_1, PPS6_datos_sorted$Alb_Q_Porcentaje, function(x, y) (x - y)^2)
matching_indices <- solve_LSAP(cost_matrix)
matched_Alb_Q <- PPS6_datos_sorted$Alb_Q_Porcentaje[matching_indices]
error_cuadratico <- sum(cost_matrix[cbind(1:length(matching_indices), matching_indices)])
datos_matched <- data.frame(P_1 = PPS6_curvas_sorted$P_1, Alb_Q_Porcentaje = matched_Alb_Q)
head(datos_matched)
cat("Error cuadrático total PPS6:", error_cuadratico, "\n")
# Usamos los índices de asignación para extraer las filas correspondientes de PPS6_datos
matching_indices_numeric <- as.vector(matching_indices)
PPS6_datos_sorted <- PPS6_datos_sorted[matching_indices_numeric, ]
PPS6_curvas_datos = cbind(PPS6_curvas_sorted, PPS6_datos_sorted)

all_curvas_datos = rbind.data.frame(PPS1_curvas_datos,PPS2_curvas_datos,PPS3_curvas_datos,PPS4_curvas_datos,PPS5_curvas_datos,PPS6_curvas_datos)
str(all_curvas_datos)
all_curvas_datos <- all_curvas_datos[, !colnames(all_curvas_datos) %in% c("ID_interno", "ID", "nombre_archivo", "calidad")]
all_curvas_datos <- all_curvas_datos[, c("PPS", "CM_L1", "CM_L2", "Edad", "Sexo", "PT", "Alb_Q", "Alb_Q_Porcentaje","curva", "L_1", "L_2", "L_3", "L_4", "L_5", "L_6", "A_tot", "A_1", "A_2", "A_3", "A_4", "A_5", "A_6", "P_1", "P_2", "P_3", "P_4", "P_5", "P_6")]
all_curvas_datos$Sexo <- factor(all_curvas_datos$Sexo)
head(all_curvas_datos)

#Variables 14 a 19 _ Concentración de cada fracción 
all_curvas_datos$C_1 <- all_curvas_datos$P_1*all_curvas_datos$PT/100
all_curvas_datos$C_2 <- all_curvas_datos$P_2*all_curvas_datos$PT/100
all_curvas_datos$C_3 <- all_curvas_datos$P_3*all_curvas_datos$PT/100
all_curvas_datos$C_4 <- all_curvas_datos$P_4*all_curvas_datos$PT/100
all_curvas_datos$C_5 <- all_curvas_datos$P_5*all_curvas_datos$PT/100
all_curvas_datos$C_6 <- all_curvas_datos$P_6*all_curvas_datos$PT/100

summary(all_curvas_datos[, c("C_1", "C_2", "C_3", "C_4", "C_5", "C_6")])
boxplot(all_curvas_datos[, c("C_1", "C_2", "C_3", "C_4", "C_5", "C_6")], 
        main = "Distribución de Concentraciones", 
        ylab = "Área",
        col = rainbow(6))
par(mfrow = c(3, 2))
boxplot(all_curvas_datos$C_1 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_1",xlab = "PPS",
        col = "red")
boxplot(all_curvas_datos$C_2 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_2",xlab = "PPS",
        col = "yellow")
boxplot(all_curvas_datos$C_3 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_3",xlab = "PPS",
        col = "green")
boxplot(all_curvas_datos$C_4 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_4",xlab = "PPS",
        col = "cyan")
boxplot(all_curvas_datos$C_5 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_5",xlab = "PPS",
        col = "blue")
boxplot(all_curvas_datos$C_6 ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones", 
        ylab = "C_6",xlab = "PPS",
        col = "violet")
par(mfrow = c(1, 1))

#Variables 20 a 28 – Posiciones medias de cada fracción (alb a b2) y posiciones 20/40/60/80% de fracción gamma
all_curvas_datos$L_05 = round(all_curvas_datos$L_1+1)/2
all_curvas_datos$L_15 = round((all_curvas_datos$L_1+all_curvas_datos$L_2)/2)
all_curvas_datos$L_25 = round((all_curvas_datos$L_2+all_curvas_datos$L_3)/2)
all_curvas_datos$L_35 = round((all_curvas_datos$L_3+all_curvas_datos$L_4)/2)
all_curvas_datos$L_45 = round((all_curvas_datos$L_4+all_curvas_datos$L_5)/2)
all_curvas_datos$L_52 = round((300-all_curvas_datos$L_5)/5+all_curvas_datos$L_5)
all_curvas_datos$L_54 = round(2*(300-all_curvas_datos$L_5)/5+all_curvas_datos$L_5)
all_curvas_datos$L_56 = round(3*(300-all_curvas_datos$L_5)/5+all_curvas_datos$L_5)
all_curvas_datos$L_58 = round(4*(300-all_curvas_datos$L_5)/5+all_curvas_datos$L_5)


boxplot(all_curvas_datos[, c("L_05", "L_15", "L_25", "L_35", "L_45", "L_52", "L_54", "L_56", "L_58")], 
        main = "Distribución de Posiciones intermedias", 
        ylab = "Posiciones",
        col = rainbow(6))

#Variables 29 a 43 – Concentraciones relativas de las subfracciones generadas por las posiciones intermedias
#Aplicación de F6.1
limites_lista <- lapply(1:nrow(all_curvas_datos), function(i) c(
  all_curvas_datos$L_05[i],
  all_curvas_datos$L_1[i],
  all_curvas_datos$L_15[i],
  all_curvas_datos$L_2[i],
  all_curvas_datos$L_25[i],
  all_curvas_datos$L_3[i],
  all_curvas_datos$L_35[i],
  all_curvas_datos$L_4[i],
  all_curvas_datos$L_45[i],
  all_curvas_datos$L_5[i],
  all_curvas_datos$L_52[i],
  all_curvas_datos$L_54[i],
  all_curvas_datos$L_56[i],
  all_curvas_datos$L_58[i],
  all_curvas_datos$L_6[i]
))
resultados <- F6.1B_ABC_total_fraccion(all_curvas_datos$curva, limites_lista)
areas_matrix <- do.call(rbind, lapply(resultados, function(x) x$areas_limites))
all_curvas_datos$P_1_1 <- areas_matrix[, 1]/all_curvas_datos$A_tot*100
all_curvas_datos$P_1_2 <- areas_matrix[, 2]/all_curvas_datos$A_tot*100
all_curvas_datos$P_2_1 <- areas_matrix[, 3]/all_curvas_datos$A_tot*100
all_curvas_datos$P_2_2 <- areas_matrix[, 4]/all_curvas_datos$A_tot*100
all_curvas_datos$P_3_1 <- areas_matrix[, 5]/all_curvas_datos$A_tot*100
all_curvas_datos$P_3_2 <- areas_matrix[, 6]/all_curvas_datos$A_tot*100
all_curvas_datos$P_4_1 <- areas_matrix[, 7]/all_curvas_datos$A_tot*100
all_curvas_datos$P_4_2 <- areas_matrix[, 8]/all_curvas_datos$A_tot*100
all_curvas_datos$P_5_1 <- areas_matrix[, 9]/all_curvas_datos$A_tot*100
all_curvas_datos$P_5_2 <- areas_matrix[, 10]/all_curvas_datos$A_tot*100
all_curvas_datos$P_6_1 <- areas_matrix[, 11]/all_curvas_datos$A_tot*100
all_curvas_datos$P_6_2 <- areas_matrix[, 12]/all_curvas_datos$A_tot*100
all_curvas_datos$P_6_3 <- areas_matrix[, 13]/all_curvas_datos$A_tot*100
all_curvas_datos$P_6_4 <- areas_matrix[, 14]/all_curvas_datos$A_tot*100
all_curvas_datos$P_6_5 <- areas_matrix[, 15]/all_curvas_datos$A_tot*100

#all_curvas_datos$P_1_1[1] + all_curvas_datos$P_1_2[1] + all_curvas_datos$P_2_1[1] + all_curvas_datos$P_2_2[1] + all_curvas_datos$P_3_1[1] + all_curvas_datos$P_3_2[1] + all_curvas_datos$P_4_1[1] + all_curvas_datos$P_4_2[1] + all_curvas_datos$P_5_1[1] + all_curvas_datos$P_5_2[1] + all_curvas_datos$P_6_1[1] + all_curvas_datos$P_6_2[1] + all_curvas_datos$P_6_3[1] + all_curvas_datos$P_6_4[1] + all_curvas_datos$P_6_5[1]

boxplot(all_curvas_datos[, c("P_1_1", "P_1_2", "P_2_1", "P_2_2", "P_3_1", "P_3_2", "P_4_1", "P_4_2", "P_5_1","P_5_2","P_6_1","P_6_2","P_6_3","P_6_4","P_6_5")], 
        main = "Distribución de Concentraciones Relativas de las Subfracciones", 
        ylab = "Concentración relativa (%)",
        col = rainbow(15))

#Variables 44 a 58 – Concentraciones absoluitas de las subfracciones generadas por las posiciones intermedias

all_curvas_datos$C_1_1 <- all_curvas_datos$P_1_1*all_curvas_datos$PT/100
all_curvas_datos$C_1_2 <- all_curvas_datos$P_1_2*all_curvas_datos$PT/100 
all_curvas_datos$C_2_1 <- all_curvas_datos$P_2_1*all_curvas_datos$PT/100
all_curvas_datos$C_2_2 <- all_curvas_datos$P_2_2*all_curvas_datos$PT/100
all_curvas_datos$C_3_1 <- all_curvas_datos$P_2_2*all_curvas_datos$PT/100
all_curvas_datos$C_3_2 <- all_curvas_datos$P_3_2*all_curvas_datos$PT/100
all_curvas_datos$C_4_1 <- all_curvas_datos$P_4_1*all_curvas_datos$PT/100
all_curvas_datos$C_4_2 <- all_curvas_datos$P_4_2*all_curvas_datos$PT/100
all_curvas_datos$C_5_1 <- all_curvas_datos$P_5_1*all_curvas_datos$PT/100
all_curvas_datos$C_5_2 <- all_curvas_datos$P_5_2*all_curvas_datos$PT/100
all_curvas_datos$C_6_1 <- all_curvas_datos$P_6_1*all_curvas_datos$PT/100
all_curvas_datos$C_6_2 <- all_curvas_datos$P_6_2*all_curvas_datos$PT/100
all_curvas_datos$C_6_3 <- all_curvas_datos$P_6_3*all_curvas_datos$PT/100
all_curvas_datos$C_6_4 <- all_curvas_datos$P_6_4*all_curvas_datos$PT/100
all_curvas_datos$C_6_5 <- all_curvas_datos$P_6_5*all_curvas_datos$PT/100

boxplot(all_curvas_datos[, c("C_1_1", "C_1_2", "C_2_1", "C_2_2", "C_3_1", "C_3_2", "C_4_1", "C_4_2", "C_5_1","C_5_2","C_6_1","C_6_2","C_6_3","C_6_4","C_6_5")], 
        main = "Distribución de Concentraciones Absolutas de las Subfracciones", 
        ylab = "Concentración Absoluta (g/dL)",
        col = rainbow(15))


#Variables 59 a 63 – Concentraciones relativas de las fracciones normalizadas min _max

VR_1_rel_inf = 47.6
VR_1_rel_sup = 61.9
VR_1_rel_rango = VR_1_rel_sup - VR_1_rel_inf
VR_2_rel_inf = 1.4
VR_2_rel_sup = 4.6
VR_2_rel_rango = VR_2_rel_sup - VR_2_rel_inf
VR_3_rel_inf = 7.3
VR_3_rel_sup = 13.9
VR_3_rel_rango = VR_3_rel_sup - VR_3_rel_inf
VR_45_rel_inf = 10.9
VR_45_rel_sup = 19.1
VR_45_rel_rango = VR_45_rel_sup - VR_45_rel_inf
VR_6_rel_inf = 9.5
VR_6_rel_sup = 24.8
VR_6_rel_rango = VR_6_rel_sup - VR_6_rel_inf

all_curvas_datos$F1_R_norm <- (all_curvas_datos$P_1 - VR_1_rel_inf)/VR_1_rel_rango
all_curvas_datos$F2_R_norm <- (all_curvas_datos$P_2 - VR_2_rel_inf)/VR_2_rel_rango
all_curvas_datos$F3_R_norm <- (all_curvas_datos$P_3 - VR_3_rel_inf)/VR_3_rel_rango
all_curvas_datos$F45_R_norm <- (all_curvas_datos$P_4+all_curvas_datos$P_5 - VR_45_rel_inf)/VR_45_rel_rango
all_curvas_datos$F6_R_norm <- (all_curvas_datos$P_6 - VR_6_rel_inf)/VR_6_rel_rango

boxplot(all_curvas_datos[, c("F1_R_norm", "F2_R_norm", "F3_R_norm", "F45_R_norm", "F6_R_norm")], 
        main = "Distribución de Concentraciones Relativa de las Fracciones Normalizadas (min - max)", 
        ylab = "Concentraciones Relativa Normalizada",
        col = rainbow(5))

par(mfrow = c(3, 2))
boxplot(all_curvas_datos$F1_R_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Relativas Normalizadas (min - max)", 
        ylab = "F1_R_norm",xlab = "PPS",
        col = "red")
boxplot(all_curvas_datos$F2_R_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Relativas Normalizadas (min - max)", 
        ylab = "F2_R_norm",xlab = "PPS",
        col = "yellow")
boxplot(all_curvas_datos$F3_R_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Relativas Normalizadas (min - max)", 
        ylab = "F3_R_norm",xlab = "PPS",
        col = "green")
boxplot(all_curvas_datos$F45_R_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Relativas Normalizadas (min - max)", 
        ylab = "F45_R_norm",xlab = "PPS",
        col = "cyan")
boxplot(all_curvas_datos$F6_R_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Relativas Normalizadas (min - max)", 
        ylab = "F6_R_norm",xlab = "PPS",
        col = "blue")
par(mfrow = c(1, 1))

#Variables 64 a 68 – Concentraciones absolutas de las fracciones normalizadas min _max

VR_1_abs_inf = 3.5
VR_1_abs_sup = 5.0
VR_1_abs_rango = VR_1_abs_sup - VR_1_abs_inf
VR_2_abs_inf = 0.1
VR_2_abs_sup = 0.3
VR_2_abs_rango = VR_2_abs_sup - VR_2_abs_inf
VR_3_abs_inf = 0.5
VR_3_abs_sup = 0.9
VR_3_abs_rango = VR_3_abs_sup - VR_3_abs_inf
VR_45_abs_inf = 0.7
VR_45_abs_sup = 1.4
VR_45_abs_rango = VR_45_abs_sup - VR_45_abs_inf
VR_6_abs_inf = 0.65
VR_6_abs_sup = 1.6
VR_6_abs_rango = VR_6_abs_sup - VR_6_abs_inf

all_curvas_datos$F1_A_norm <- (all_curvas_datos$C_1 - VR_1_abs_inf)/VR_1_abs_rango
all_curvas_datos$F2_A_norm <- (all_curvas_datos$C_2 - VR_2_abs_inf)/VR_2_abs_rango
all_curvas_datos$F3_A_norm <- (all_curvas_datos$C_3 - VR_3_abs_inf)/VR_3_abs_rango
all_curvas_datos$F45_A_norm <- (all_curvas_datos$C_4+all_curvas_datos$C_5 - VR_45_abs_inf)/VR_45_abs_rango
all_curvas_datos$F6_A_norm <- (all_curvas_datos$C_6 - VR_6_abs_inf)/VR_6_abs_rango

boxplot(all_curvas_datos[, c("F1_A_norm", "F2_A_norm", "F3_A_norm", "F45_A_norm", "F6_A_norm")], 
        main = "Distribución de Concentraciones Absolutas de las Fracciones Normalizadas (min - max)", 
        ylab = "Concentraciones Relativa Normalizada",
        col = rainbow(5))

par(mfrow = c(3, 2))
boxplot(all_curvas_datos$F1_A_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Absolutas Normalizadas (min - max)", 
        ylab = "F1_A_norm",xlab = "PPS",
        col = "red")
boxplot(all_curvas_datos$F2_A_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Absolutas Normalizadas (min - max)", 
        ylab = "F2_A_norm",xlab = "PPS",
        col = "yellow")
boxplot(all_curvas_datos$F3_A_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Absolutas Normalizadas (min - max)", 
        ylab = "F3_A_norm",xlab = "PPS",
        col = "green")
boxplot(all_curvas_datos$F45_A_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Absolutas Normalizadas (min - max)", 
        ylab = "F45_A_norm",xlab = "PPS",
        col = "cyan")
boxplot(all_curvas_datos$F6_A_norm ~ all_curvas_datos$PPS, 
        main = "Distribución de Concentraciones Absolutas Normalizadas (min - max)", 
        ylab = "F6_A_norm",xlab = "PPS",
        col = "blue")
par(mfrow = c(1, 1))

#Variables 69 a 368 - Puntos de la curva
dataset <- all_curvas_datos %>%
  unnest_wider(curva, names_sep = "_")
dataset = as.data.frame(dataset)

save(dataset, file ="dataset.RData")
