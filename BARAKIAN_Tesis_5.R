#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 5 = Chequeo y corrección de limites entre fracciones/CM

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

#LIBRERIAS
library(dplyr)
library(ggplot2)

load("all_curvas.RData")

summary(all_curvas$limites)

all_curvas$limites %>%
  unlist() %>%  # Convertir la lista a un vector si es necesario
  summary()  # Resumen básico

#Eliminar limites arriba de 300
all_curvas$limites <- lapply(all_curvas$limites, function(limite) {
  limite[limite < 301]  # Mantener solo los valores menores a 301
})

all_curvas$limites %>%
  unlist() %>%
  summary()  

#hay limites duplicados dentro del mismo registro
all_curvas$limites <- lapply(all_curvas$limites, unique)

#cantidad de limites
cantidad_limites <- sapply(all_curvas$limites, length)  # Contar los límites en cada elemento
summary(cantidad_limites)
all_curvas$cant_limite <- cantidad_limites

# Comprobamos si los límites están en orden ascendente para cada registro
no_ordenados <- sapply(all_curvas$limites, function(limites) {
  # Verificamos si los límites no están en orden ascendente
  !all(diff(limites) > 0)
})
indices_no_ordenados <- which(no_ordenados)
indices_no_ordenados #todo ok

#extraer los limites en (max cant de limites) nuevas variables
max_limites <- max(sapply(all_curvas$limites, length))
all_curvas[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(all_curvas$limites, function(x) x[i])))

#Se trabaja PPS 1 a 5 primero y el PPS 6 por separado
curvas_GM <- all_curvas[all_curvas$PPS == "6_GM", ]
curvas_noGM <- all_curvas[all_curvas$PPS != "6_GM", ]

# Histograma de la columna cant_limite 
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6_GM", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")

# Boxplot de las columnas L_n 
boxplot(curvas_noGM[, paste0("L_", 1:25)], 
        main = "Boxplot de L_1 a L_25 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_25", 
        xlab = "Límites", 
        col = rainbow(25))
boxplot(curvas_GM[, paste0("L_", 1:25)], 
        main = "Boxplot de L_1 a L_25 PPS 6_GM", 
        ylab = "Valores de L_1 a L_25", 
        xlab = "Límites", 
        col = rainbow(25))

#########################################################################Corrección Grupos PPS 1 a 5 (no_GM)

#A _ Eliminar límites erroneos 

#Ploteo de graficos para inspeccionar de forma global:
plotear_grupo_curvas <- function(indices, curvas, limites_ajustados) {
  indices <- indices  
  par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))  # 5 filas y 5 columnas
  for (i in indices) {
    plot(curvas[[i]], col = "blue", pch = 16, cex = 0.3, 
         xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva", i), type = "l")
    # Agregar las líneas de los límites ajustados
    abline(v = limites_ajustados[[i]], col = "red", lwd = 2, lty = 2)
  }
  par(mfrow = c(1, 1))
}

indices_limites_7_o_mas <- which(sapply(curvas_noGM$cant_limite, function(x) any(x >= 7)))
indices_limites_7_o_mas #215 en total

indices_limites_10_o_mas <- which(sapply(curvas_noGM$cant_limite, function(x) any(x >= 10)))
plotear_grupo_curvas(indices_limites_10_o_mas, curvas_noGM$curva, curvas_noGM$limites)
rangos_eliminar <- c(100:125, 250:299)
curvas_noGM$limites[indices_limites_10_o_mas] <- lapply(curvas_noGM$limites[indices_limites_10_o_mas],function(lim) lim[!(lim %in% rangos_eliminar)])
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_10_o_mas <- which(sapply(curvas_noGM$cant_limite, function(x) any(x >= 10)))
indices_limites_10_o_mas #9 remanentes
rangos_eliminar <- c(120:140)
indices = c(318,656,657,671,955)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices], function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(70:85)
indices = c(364)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices],function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(200:210)
indices = c(656)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices],function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(140:150)
indices = c(657)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices],function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(160:180)
indices = c(659,657)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices],function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(120:150)
indices = c(859)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices], function(lim) lim[!(lim %in% rangos_eliminar)])
rangos_eliminar <- c(120:130,200:230)
indices = c(908)
curvas_noGM$limites[indices] <- lapply(curvas_noGM$limites[indices],function(lim) lim[!(lim %in% rangos_eliminar)])
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_10_o_mas <- which(sapply(curvas_noGM$cant_limite, function(x) any(x >= 10)))
indices_limites_10_o_mas #0 remanentes

indices_limites_9 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 9)))
plotear_grupo_curvas(indices_limites_9, curvas_noGM$curva, curvas_noGM$limites)
indices345 = c(17,304,324,325,333,466,617,623,626,643,658,668,681,683,697,700,703,716,727,758,776,822,878)
curvas_noGM$limites[indices345] <- lapply(curvas_noGM$limites[indices345], function(lim) lim[-c(3, 4, 5)])
indices346 = c(310)
curvas_noGM$limites[indices346] <- lapply(curvas_noGM$limites[indices346], function(lim) lim[-c(3, 4, 6)])
indices2345 = c(314,675)
curvas_noGM$limites[indices2345] <- lapply(curvas_noGM$limites[indices2345], function(lim) lim[-c(2,3, 4, 5)])
indices356 = c(627)
curvas_noGM$limites[indices356] <- lapply(curvas_noGM$limites[indices356], function(lim) lim[-c(3, 5, 6)])
indices358 = c(642)
curvas_noGM$limites[indices358] <- lapply(curvas_noGM$limites[indices358], function(lim) lim[-c(3, 5, 8)])
indices348 = c(677,752,889)
curvas_noGM$limites[indices348] <- lapply(curvas_noGM$limites[indices348], function(lim) lim[-c(3, 4, 8)])
indices678 = c(982)
curvas_noGM$limites[indices678] <- lapply(curvas_noGM$limites[indices678], function(lim) lim[-c(6, 7, 8)])
plotear_grupo_curvas(indices_limites_9, curvas_noGM$curva, curvas_noGM$limites) #corregido
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_9 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 9)))
indices_limites_9 #0 remanentes

indices_limites_8 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 8)))
indices_limites_8
plotear_grupo_curvas(indices_limites_8, curvas_noGM$curva, curvas_noGM$limites)
indices34 = c(297,307,316,328,329,332,348,363,495,582,621,631,633,636,641,649,664,678,710,731,740,804,908,916,921,927,957)
curvas_noGM$limites[indices34] <- lapply(curvas_noGM$limites[indices34], function(lim) lim[-c(3, 4)])
indices67 = c(345,589,657,812,868,942)
curvas_noGM$limites[indices67] <- lapply(curvas_noGM$limites[indices67], function(lim) lim[-c(6, 7)])
indices234 = c(628)
curvas_noGM$limites[indices234] <- lapply(curvas_noGM$limites[indices234], function(lim) lim[-c(2,3,4)])
indices37 = c(648,735,751)
curvas_noGM$limites[indices37] <- lapply(curvas_noGM$limites[indices37], function(lim) lim[-c(3, 7)])
indices35 = c(660,739,775,929)
curvas_noGM$limites[indices35] <- lapply(curvas_noGM$limites[indices35], function(lim) lim[-c(3, 5)])
indices36 = c(694)
curvas_noGM$limites[indices36] <- lapply(curvas_noGM$limites[indices36], function(lim) lim[-c(3, 6)])
indices27 = c(719)
curvas_noGM$limites[indices27] <- lapply(curvas_noGM$limites[indices27], function(lim) lim[-c(2, 7)])
indices126 = c(724)
curvas_noGM$limites[indices126] <- lapply(curvas_noGM$limites[indices126], function(lim) lim[-c(1,2,6)])
plotear_grupo_curvas(indices_limites_8, curvas_noGM$curva, curvas_noGM$limites) #corregido
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_8 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 8)))
indices_limites_8 #0 remanentes

indices_limites_7 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 7)))
indices_limites_7 #116 
plotear_grupo_curvas(indices_limites_7, curvas_noGM$curva, curvas_noGM$limites)
rangos_eliminar <- c(270:299)
curvas_noGM$limites[indices_limites_7] <- lapply(curvas_noGM$limites[indices_limites_7],function(lim) lim[!(lim %in% rangos_eliminar)])
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_7 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 7)))
indices_limites_7 #62 remanentes
indices3 = c(137,273,311,337,341,342,343,346,350,352,355,357,364,365,379,565,624,632,638,646,656,673,679,684,686,689,691,692,693,711,717,726,728,745,781,832,903,907,917,920,926,974,984,993,1013,1020)
curvas_noGM$limites[indices3] <- lapply(curvas_noGM$limites[indices3], function(lim) lim[-c(3)])
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_7 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 7)))
indices_limites_7 #16 remanentes
indices4 = c(165,417,620,674,682)
curvas_noGM$limites[indices4] <- lapply(curvas_noGM$limites[indices4], function(lim) lim[-c(4)])
curvas_noGM$cant_limite <- sapply(curvas_noGM$limites, length)
indices_limites_7 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 7)))
indices_limites_7 #11 remanentes
indices23 = c(317,637,706,722,986,992)
curvas_noGM$limites[indices23] <- lapply(curvas_noGM$limites[indices23], function(lim) lim[-c(2,3)])
indices6 = c(393,455,867)
curvas_noGM$limites[indices6] <- lapply(curvas_noGM$limites[indices6], function(lim) lim[-c(6)])
curvas_noGM$limites[[296]] <- curvas_noGM$limites[[296]][-c(1,3)]
curvas_noGM$limites[[738]] <- curvas_noGM$limites[[738]][-c(1,2)]
plotear_grupo_curvas(indices_limites_7, curvas_noGM$curva, curvas_noGM$limites) #corregido

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#B _ Agregar límites faltantes 
indices_limites_0 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 0)))
indices_limites_0  #0
indices_limites_1 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 1)))
indices_limites_1 #0
indices_limites_2 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 2)))
indices_limites_2 #1 --> 768 --> faltan 4 limites
indices_limites_3 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 3)))
indices_limites_3 #1 --> 1010 --> faltan 3 limites
indices_limites_4 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 4)))
indices_limites_4 #0
indices_limites_5 <- which(sapply(curvas_noGM$cant_limite, function(x) any(x == 5)))
indices_limites_5 #60

#correccion 768
plot(curvas_noGM$curva[[768]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 768", type = "l")
abline(v =curvas_noGM$limites[[768]], col = "red", lwd = 2, lty = 2)
#coords <- locator(4)  
#print(coords)
#add_lim_768 <- round(coords$x)
add_lim_768 = c(59, 146, 182, 204)
curvas_noGM$limites[[768]] <- sort(c(curvas_noGM$limites[[768]], add_lim_768))
#correccion 1010
plot(curvas_noGM$curva[[1010]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 1010", type = "l")
abline(v =curvas_noGM$limites[[1010]], col = "red", lwd = 2, lty = 2)
#coords <- locator(3)  
#print(coords)
#add_lim_1010 <- round(coords$x)
add_lim_1010 = c(91, 175, 196)
curvas_noGM$limites[[1010]] <- sort(c(curvas_noGM$limites[[1010]], add_lim_1010))

#correccipon 60 registros segun indices_limites_5
plotear_grupo_curvas(indices_limites_5, curvas_noGM$curva, curvas_noGM$limites) #siempre falta el primero
summary(curvas_noGM$L_1) #rango minimo 46 a 3Q 65 (el maximo no tiene sentido porque son justamente los que faltan)
#Corregir L_1
minimos_X <- numeric(0)
for (i in indices_limites_5) {
  curva <- curvas_noGM$curva[[i]]
  curva_filtrada <- curva[46:65]
  indice_minimo <- which.min(curva_filtrada)
  minimo_X <- indice_minimo + 45  # Ajuste para obtener el valor de X
  minimos_X <- c(minimos_X, minimo_X)
}
for (i in indices_limites_5) {
  minimo_X <- minimos_X[which(indices_limites_5 == i)]
    curvas_noGM$limites[[i]] <- sort(c(curvas_noGM$limites[[i]], minimo_X))
}
plotear_grupo_curvas(indices_limites_5, curvas_noGM$curva, curvas_noGM$limites) #todo ok

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C) Chequeo de límites erroneos:
#C1) Curvas con ultimo lim < 300
indices_bajo_300 <- which(sapply(curvas_noGM$limites, function(l) tail(l, 1) < 300))
indices_bajo_300 #4

plot(curvas_noGM$curva[[182]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 182", type = "l")
abline(v =curvas_noGM$limites[[182]], col = "red", lwd = 2, lty = 2)
plot(curvas_noGM$curva[[366]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 366", type = "l")
abline(v =curvas_noGM$limites[[366]], col = "red", lwd = 2, lty = 2)
plot(curvas_noGM$curva[[400]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 400", type = "l")
abline(v =curvas_noGM$limites[[400]], col = "red", lwd = 2, lty = 2)
plot(curvas_noGM$curva[[961]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva 961", type = "l")
abline(v =curvas_noGM$limites[[961]], col = "red", lwd = 2, lty = 2)

indices_bajo_300 = c(182,366,400) #en estos 3 casos, se reemplaza el último
for (i in indices_bajo_300) {
  curvas_noGM$limites[[i]][length(curvas_noGM$limites[[i]])] <- 300
}

curvas_noGM$limites[[961]] <- curvas_noGM$limites[[961]][-c(3)] #en este caso, el limite erroneo es el 3
curvas_noGM$limites[[961]] <- sort(c(curvas_noGM$limites[[961]], 300))

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C2)  Filtrar índices donde el 5to límite es mayor que 250
indices_limites_5to_250 <- which(sapply(curvas_noGM$limites, function(l) l[5] > 250))
indices_limites_5to_250 #25
plotear_grupo_curvas(indices_limites_5to_250, curvas_noGM$curva, curvas_noGM$limites) #todos falta el primero y sobra el 5to

# Eliminar el 5to límite de cada curva
for (i in indices_limites_5to_250) {
  curvas_noGM$limites[[i]] <- curvas_noGM$limites[[i]][-5]
}

#Corregir L_1
minimos_X <- numeric(0)
for (i in indices_limites_5to_250) {
  curva <- curvas_noGM$curva[[i]]
  curva_filtrada <- curva[46:65]
  indice_minimo <- which.min(curva_filtrada)
  minimo_X <- indice_minimo + 45  # Ajuste para obtener el valor de X
  minimos_X <- c(minimos_X, minimo_X)
}
for (i in indices_limites_5to_250) {
  minimo_X <- minimos_X[which(indices_limites_5to_250 == i)]
  curvas_noGM$limites[[i]] <- sort(c(curvas_noGM$limites[[i]], minimo_X))
}

plotear_grupo_curvas(indices_limites_5to_250, curvas_noGM$curva, curvas_noGM$limites) #todo ok salvo 133
curvas_noGM$limites[[133]] <- curvas_noGM$limites[[133]][-c(1)]
curvas_noGM$limites[[133]] <- sort(c(curvas_noGM$limites[[133]], 45))

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C3)  Chequeo de outliers por limites
outliers_L1 = quantile(curvas_noGM$L_1, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_noGM$L_1, na.rm = TRUE)
indices_outliers_L1 <- which(curvas_noGM$L_1 > outliers_L1)
plotear_grupo_curvas(indices_outliers_L1, curvas_noGM$curva, curvas_noGM$limites) 
indices3 = c(68,351)
indices2 = c(299,705,730,734,754,757,795,848,976,1026)
indices1 = 972
curvas_noGM$limites[indices3] <- lapply(curvas_noGM$limites[indices3], function(lim) lim[-c(3)])
curvas_noGM$limites[indices2] <- lapply(curvas_noGM$limites[indices2], function(lim) lim[-c(2)])
curvas_noGM$limites[indices1] <- lapply(curvas_noGM$limites[indices1], function(lim) lim[-c(1)])
indices123 = sort(c(indices3,indices2,indices1))
#Corregir L_1
minimos_X <- numeric(0)
for (i in indices123) {
  curva <- curvas_noGM$curva[[i]]
  curva_filtrada <- curva[46:65]
  indice_minimo <- which.min(curva_filtrada)
  minimo_X <- indice_minimo + 45  # Ajuste para obtener el valor de X
  minimos_X <- c(minimos_X, minimo_X)
}
for (i in indices123) {
  minimo_X <- minimos_X[which(indices123 == i)]
  curvas_noGM$limites[[i]] <- sort(c(curvas_noGM$limites[[i]], minimo_X))
}
plotear_grupo_curvas(indices123, curvas_noGM$curva, curvas_noGM$limites) #todo ok salvo 754
curvas_noGM$limites[[754]] <- curvas_noGM$limites[[754]][-c(1)]
curvas_noGM$limites[[754]] <- sort(c(curvas_noGM$limites[[754]], 75))

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

outliers_L2 = quantile(curvas_noGM$L_2, probs = 0.25, na.rm = TRUE) - 1.5*IQR(curvas_noGM$L_2, na.rm = TRUE)
indices_outliers_L2 <- which(curvas_noGM$L_2 < outliers_L2)
plotear_grupo_curvas(indices_outliers_L2, curvas_noGM$curva, curvas_noGM$limites) #todo ok 

outliers_L3 = quantile(curvas_noGM$L_3, probs = 0.25, na.rm = TRUE) - 1.5*IQR(curvas_noGM$L_3, na.rm = TRUE)
indices_outliers_L3 <- which(curvas_noGM$L_3 < outliers_L3)
plotear_grupo_curvas(indices_outliers_L3, curvas_noGM$curva, curvas_noGM$limites) #859
curvas_noGM$limites[[859]] <- curvas_noGM$limites[[859]][-c(1)]
curvas_noGM$limites[[859]] <- sort(c(curvas_noGM$limites[[859]], 125))
plotear_grupo_curvas(indices_outliers_L3, curvas_noGM$curva, curvas_noGM$limites) #859 ok

#Recalcular columnas:
curvas_noGM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_noGM$limites, length)
curvas_noGM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_noGM$limites, length))
curvas_noGM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_noGM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_noGM$cant_limite, main = "Histograma de Cantidad de límites de PPS 1 a 5 (no_GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_noGM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))

outliers_L4 = quantile(curvas_noGM$L_4, probs = 0.25, na.rm = TRUE) - 1.5*IQR(curvas_noGM$L_4, na.rm = TRUE)
indices_outliers_L4 <- which(curvas_noGM$L_4 < outliers_L4)
plotear_grupo_curvas(indices_outliers_L4, curvas_noGM$curva, curvas_noGM$limites) #todo ok

outliers_L5 = quantile(curvas_noGM$L_5, probs = 0.25, na.rm = TRUE) - 1.5*IQR(curvas_noGM$L_5, na.rm = TRUE)
indices_outliers_L5 <- which(curvas_noGM$L_5 < outliers_L4)
plotear_grupo_curvas(indices_outliers_L5, curvas_noGM$curva, curvas_noGM$limites) #todo ok

outliers_L2high = quantile(curvas_noGM$L_2, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_noGM$L_2, na.rm = TRUE)
indices_outliers_L2high <- which(curvas_noGM$L_2 > outliers_L2high)
plotear_grupo_curvas(indices_outliers_L2high, curvas_noGM$curva, curvas_noGM$limites) #todo ok

outliers_L3high = quantile(curvas_noGM$L_3, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_noGM$L_3, na.rm = TRUE)
indices_outliers_L3high <- which(curvas_noGM$L_3 > outliers_L3high)
plotear_grupo_curvas(indices_outliers_L3high, curvas_noGM$curva, curvas_noGM$limites) #todo ok

outliers_L4high = quantile(curvas_noGM$L_4, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_noGM$L_4, na.rm = TRUE)
indices_outliers_L4high <- which(curvas_noGM$L_4 > outliers_L4high)
plotear_grupo_curvas(indices_outliers_L4high, curvas_noGM$curva, curvas_noGM$limites) #todo ok

outliers_L5high = quantile(curvas_noGM$L_5, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_noGM$L_5, na.rm = TRUE)
indices_outliers_L5high <- which(curvas_noGM$L_5 > outliers_L5high)
plotear_grupo_curvas(indices_outliers_L5high, curvas_noGM$curva, curvas_noGM$limites) #todo ok

summary(curvas_noGM[, c("L_1", "L_2", "L_3", "L_4", "L_5", "L_6")])
curvas_noGM$limites_CM <- replicate(nrow(curvas_noGM), list(NA, NA), simplify = FALSE)
curvas_noGM <- curvas_noGM[, !names(curvas_noGM) %in% c("L_7", "L_8", "L_9", "L_10", "L_11","L_12", "L_13", "L_14", "L_15", "L_16","L_17", "L_18", "L_19", "L_20", "L_21","L_22", "L_23", "L_24", "L_25")]
boxplot(curvas_noGM[, paste0("L_", 1:6)], 
        main = "Boxplot de L_1 a L_6 PPS 1 a 5 (no_GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(6))

#########################################################################Corrección Grupos PPS 6 (GM)

curvas_GM$limites_CM <- replicate(nrow(curvas_GM), list(NA, NA), simplify = FALSE)

#Ploteo de graficos para inspeccionar de forma global:
plotear_grupo_curvas_con_CM <- function(indices, curvas, limites,limites_CM) {
  indices <- indices  
  par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))  # 5 filas y 5 columnas
  for (i in indices) {
    plot(curvas[[i]], col = "blue", pch = 16, cex = 0.3, 
         xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva", i), type = "l")
    # Agregar las líneas de los límites ajustados
    abline(v = limites[[i]], col = "red", lwd = 2, lty = 2)
    abline(v = limites_CM[[i]], col = "green", lwd = 2, lty = 2)
    
  }
  par(mfrow = c(1, 1))
}

#A _ Eliminar límites erroneos 
indices_limites_12_o_mas <- which(sapply(curvas_GM$cant_limite, function(x) any(x >= 12)))
indices_limites_12_o_mas #46
plotear_grupo_curvas(indices_limites_12_o_mas, curvas_GM$curva, curvas_GM$limites)
rangos_eliminar <- c(100:125)
curvas_GM$limites[indices_limites_12_o_mas] <- lapply(curvas_GM$limites[indices_limites_12_o_mas],function(lim) lim[!(lim %in% rangos_eliminar)])
curvas_GM$cant_limite <- sapply(curvas_GM$limites, length)
indices_limites_12_o_mas <- which(sapply(curvas_GM$cant_limite, function(x) any(x >= 12)))
indices_limites_12_o_mas #41 remanentes
plotear_grupo_curvas(indices_limites_12_o_mas, curvas_GM$curva, curvas_GM$limites)
indices_5_2 = c(3,71,73,75,85,87,102,115,160,161,185,186,217,228,230)
curvas_GM$limites[indices_5_2] <- lapply(curvas_GM$limites[indices_5_2], function(lim) c(lim[1:6], lim[(length(lim)-1):length(lim)]))
curvas_GM$limites[[6]] <- sort(c(curvas_GM$limites[[6]][c(1:4, 12:14)], 60))
curvas_GM$limites[[19]] <- sort(c(curvas_GM$limites[[19]][c(1:5, 13:14)], 60))
curvas_GM$limites[[22]] <- curvas_GM$limites[[22]][c(1:4,7,10,11,13)]
curvas_GM$limites[[53]] <- curvas_GM$limites[[53]][c(1:6, 11,13)]
curvas_GM$limites[[55]] <- curvas_GM$limites[[55]][c(1:5, 12,14)]
curvas_GM$limites[[58]] <- curvas_GM$limites[[58]][c(1:2, 8:13)]
curvas_GM$limites[[69]] <- sort(c(curvas_GM$limites[[69]][c(1,5:8,13:14)], 100))
curvas_GM$limites[[74]] <- curvas_GM$limites[[74]][c(1:2,5:8,15:16)]
curvas_GM$limites[[76]] <- curvas_GM$limites[[76]][c(1:2,4:7,16,18)]
curvas_GM$limites[[77]] <- curvas_GM$limites[[77]][c(1:2,5:8,14:15)]
curvas_GM$limites[[88]] <- curvas_GM$limites[[88]][c(1:2,5:8,13:14)]
curvas_GM$limites[[139]] <- curvas_GM$limites[[139]][c(1:5,11:13)]
curvas_GM$limites[[141]] <- sort(c(curvas_GM$limites[[141]][c(1:6,11)], 300))
curvas_GM$limites[[146]] <- curvas_GM$limites[[146]][c(1:7,12)]
curvas_GM$limites[[155]] <- curvas_GM$limites[[155]][c(1,3,4,13,16,17,23,24)]
curvas_GM$limites[[156]] <- curvas_GM$limites[[156]][c(1:3,5,9:11,13)]
curvas_GM$limites[[164]] <- sort(c(curvas_GM$limites[[164]][c(1:4,11:12)], 168,227))
curvas_GM$limites[[166]] <- curvas_GM$limites[[166]][c(1:2,7:10,17,19)]
curvas_GM$limites[[193]] <- curvas_GM$limites[[193]][c(1:5,7,10,12)]
curvas_GM$limites[[195]] <- curvas_GM$limites[[195]][c(1:2,5:8,14,15)]
curvas_GM$limites[[209]] <- curvas_GM$limites[[209]][c(1:3,5:7,10,12)]
curvas_GM$limites[[211]] <- curvas_GM$limites[[211]][c(1:3,5,6,8,13,14)]
curvas_GM$limites[[223]] <- curvas_GM$limites[[223]][c(1:5,11:13)]
curvas_GM$limites[[224]] <- curvas_GM$limites[[224]][c(1:5,7,11,13)]
curvas_GM$limites[[259]] <- curvas_GM$limites[[259]][c(1:2,6:8,10,13,15)]
curvas_GM$limites[[261]] <- sort(c(curvas_GM$limites[[261]][c(5:9,12,13)], 60))
plotear_grupo_curvas(indices_limites_12_o_mas, curvas_GM$curva, curvas_GM$limites) #todo ok

indices_limites_11 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 11)))
indices_limites_11 #15
plotear_grupo_curvas_con_CM(indices_limites_11,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
curvas_GM$limites[[43]] <- sort(c(curvas_GM$limites[[43]][-c(2,6,8,9)], 75))
curvas_GM$limites[[70]] <- curvas_GM$limites[[70]][-c(3, 8, 9)]
curvas_GM$limites[[72]] <- curvas_GM$limites[[72]][-c(7, 8, 9)]
curvas_GM$limites[[80]] <- curvas_GM$limites[[80]][-c(3, 4, 9)]
curvas_GM$limites[[86]] <- curvas_GM$limites[[86]][-c(6, 8, 10)]
curvas_GM$limites[[90]] <- curvas_GM$limites[[90]][-c(3, 4, 7)]
curvas_GM$limites[[126]] <- curvas_GM$limites[[126]][-c(3, 4, 10)]
curvas_GM$limites[[143]] <- curvas_GM$limites[[143]][-c(7, 8, 9)]
curvas_GM$limites[[159]] <- curvas_GM$limites[[159]][-c(7, 8, 9)]
curvas_GM$limites[[171]] <- curvas_GM$limites[[171]][-c(3, 8, 9)]
curvas_GM$limites[[184]] <- curvas_GM$limites[[184]][-c(7, 8, 9)]
curvas_GM$limites[[191]] <- sort(c(curvas_GM$limites[[191]][-c(6,7,8,9)], 65))
curvas_GM$limites[[192]] <- sort(c(curvas_GM$limites[[192]][-c(6,7,8,9)], 65))
curvas_GM$limites[[199]] <- curvas_GM$limites[[199]][-c(7, 8, 9)]
curvas_GM$limites[[250]] <- curvas_GM$limites[[250]][-c(5, 7, 10)]
plotear_grupo_curvas_con_CM(indices_limites_11,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

indices_limites_10 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 10)))
indices_limites_10 #27
plotear_grupo_curvas_con_CM(indices_limites_10,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
indices78 = c(16,78,107,108,110,125,144,182,197,203,219,226,251,258)
curvas_GM$limites[indices78] <- lapply(curvas_GM$limites[indices78], function(lim) lim[-c(7, 8)])
curvas_GM$limites[[21]] <- curvas_GM$limites[[21]][-c(6,7)]
curvas_GM$limites[[128]] <- curvas_GM$limites[[128]][-c(6,7)]
curvas_GM$limites[[68]] <- curvas_GM$limites[[68]][-c(3,8)]
curvas_GM$limites[[89]] <- curvas_GM$limites[[89]][-c(3,4)]
curvas_GM$limites[[95]] <- curvas_GM$limites[[95]][-c(3,4)]
curvas_GM$limites[[147]] <- curvas_GM$limites[[147]][-c(3,4)]
curvas_GM$limites[[188]] <- curvas_GM$limites[[188]][-c(5,7)]
curvas_GM$limites[[204]] <- curvas_GM$limites[[204]][-c(5,6)]
curvas_GM$limites[[206]] <- curvas_GM$limites[[206]][-c(5,6)]
curvas_GM$limites[[97]] <- sort(c(curvas_GM$limites[[97]][-c(6,7,8)], 202))
curvas_GM$limites[[114]] <- sort(c(curvas_GM$limites[[114]][-c(7,8,9)], 295))
curvas_GM$limites[[168]] <- sort(c(curvas_GM$limites[[168]][-c(6,7,9)], 65))
curvas_GM$limites[[239]] <- sort(c(curvas_GM$limites[[239]][-c(7,8,9)], 160))
plotear_grupo_curvas_con_CM(indices_limites_10,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

indices_limites_9 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 9)))
indices_limites_9 #47
plotear_grupo_curvas_con_CM(indices_limites_9,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
indices6 = c(31,232)
curvas_GM$limites[indices6] <- lapply(curvas_GM$limites[indices6], function(lim) lim[-c(6)])
indices8 = c(33,44,49,92,113,158,162,174,181,198,201,221,249)
curvas_GM$limites[indices8] <- lapply(curvas_GM$limites[indices8], function(lim) lim[-c(8)])
indices7 = c(46,48,65,79,82,84,118,119,140,167,169,190,194,208,213,241,248,252)
curvas_GM$limites[indices7] <- lapply(curvas_GM$limites[indices7], function(lim) lim[-c(7)])
indices3 = c(91,177,227)
curvas_GM$limites[indices3] <- lapply(curvas_GM$limites[indices3], function(lim) lim[-c(3)])
indices4 = c(99,100)
curvas_GM$limites[indices4] <- lapply(curvas_GM$limites[indices4], function(lim) lim[-c(4)])
curvas_GM$limites[[20]] <- sort(c(curvas_GM$limites[[20]][-c(5,6)], 65))
curvas_GM$limites[[42]] <- sort(c(curvas_GM$limites[[42]][-c(7,8)], 300))
curvas_GM$limites[[47]] <- sort(c(curvas_GM$limites[[47]][-c(6,7)], 65))
curvas_GM$limites[[94]] <- sort(c(curvas_GM$limites[[94]][-c(7,8)], 60))
curvas_GM$limites[[149]] <- sort(c(curvas_GM$limites[[149]][-c(3,8)], 185))
curvas_GM$limites[[176]] <- sort(c(curvas_GM$limites[[176]][-c(3,8)], 290))
curvas_GM$limites[[202]] <- sort(c(curvas_GM$limites[[202]][-c(7,8)], 60))
curvas_GM$limites[[216]] <- sort(c(curvas_GM$limites[[216]][-c(7,8)], 300))
curvas_GM$limites[[262]] <- sort(c(curvas_GM$limites[[262]][-c(2,7,8)], 65,290))
plotear_grupo_curvas_con_CM(indices_limites_9,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#B - agregar limites
indices_limites_0 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 0)))
indices_limites_0  #0
indices_limites_1 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 1)))
indices_limites_1 #0
indices_limites_2 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 2)))
indices_limites_2 #0
indices_limites_3 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 3)))
indices_limites_3 #0
indices_limites_4 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 4)))
indices_limites_4 #0
indices_limites_5 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 5)))
indices_limites_5 #6

indices_limites_6 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 6)))
indices_limites_6 #13
plotear_grupo_curvas_con_CM(indices_limites_6,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
curvas_GM$limites[[15]] <- sort(c(curvas_GM$limites[[15]], 200,205))
curvas_GM$limites[[26]] <- sort(c(curvas_GM$limites[[26]][-c(5)], 70,195,200))
curvas_GM$limites[[32]] <- sort(c(curvas_GM$limites[[32]], 170,175))
curvas_GM$limites[[57]] <- sort(c(curvas_GM$limites[[57]], 165,170))
curvas_GM$limites[[62]] <- sort(c(curvas_GM$limites[[62]], 70,280))
curvas_GM$limites[[93]] <- sort(c(curvas_GM$limites[[93]][-c(5)], 65,190,195))
curvas_GM$limites[[129]] <- sort(c(curvas_GM$limites[[129]], 70,175))
curvas_GM$limites[[135]] <- sort(c(curvas_GM$limites[[135]], 195,200))
curvas_GM$limites[[138]] <- sort(c(curvas_GM$limites[[138]], 260,280))
curvas_GM$limites[[157]] <- sort(c(curvas_GM$limites[[157]][-c(5)], 55,225,240))
curvas_GM$limites[[214]] <- sort(c(curvas_GM$limites[[214]], 65,140))
curvas_GM$limites[[222]] <- sort(c(curvas_GM$limites[[222]], 165,225))
curvas_GM$limites[[237]] <- sort(c(curvas_GM$limites[[237]], 170,198))
plotear_grupo_curvas_con_CM(indices_limites_6,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

indices_limites_7 <- which(sapply(curvas_GM$cant_limite, function(x) any(x == 7)))
indices_limites_7 #29
plotear_grupo_curvas_con_CM(indices_limites_7,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
curvas_GM$limites[[2]] <- sort(c(curvas_GM$limites[[2]][-c(7)], 215,260))
curvas_GM$limites[[55]] <- sort(c(curvas_GM$limites[[55]][-c(6)],197,300))
indices299 <- c(4,27,60,64,150,264)
curvas_GM$limites[indices299] <- lapply(curvas_GM$limites[indices299], function(lim) {sort(c(lim, 299))})
indices300 <- c(9,38,61)
curvas_GM$limites[indices300] <- lapply(curvas_GM$limites[indices300], function(lim) {sort(c(lim, 300))})
indices55 <- c(17,131)
curvas_GM$limites[indices55] <- lapply(curvas_GM$limites[indices55], function(lim) {sort(c(lim, 55))})
indices65 <- c(23,106,112,145,173,205,218,243)
curvas_GM$limites[indices65] <- lapply(curvas_GM$limites[indices65], function(lim) {sort(c(lim, 65))})
indices60 <- c(25,34,130,257)
curvas_GM$limites[indices60] <- lapply(curvas_GM$limites[indices60], function(lim) {sort(c(lim, 60))})
indices290 <- c(36,260)
curvas_GM$limites[indices290] <- lapply(curvas_GM$limites[indices290], function(lim) {sort(c(lim, 290))})
indices285 <- c(40,153)
curvas_GM$limites[indices285] <- lapply(curvas_GM$limites[indices285], function(lim) {sort(c(lim, 285))})
plotear_grupo_curvas_con_CM(indices_limites_7,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C1) Curvas con ultimo lim < 300
indices_bajo_300 <- which(sapply(curvas_GM$limites, function(l) tail(l, 1) < 300))
indices_bajo_300 #4
i=2 #ultimo
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(8)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 300))
i=152 #7
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 300))
i=171 #ultimo
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(8)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 300))
i=210 #7
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 300))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C2) Curvas con L_1 out sup
outliers_L1 = quantile(curvas_GM$L_1, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_GM$L_1, na.rm = TRUE)
indices_outliers_L1 <- which(curvas_GM$L_1 > outliers_L1)
plotear_grupo_curvas_con_CM(indices_outliers_L1,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok salvo 43,109,136
i=43
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(6)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 64))
i=109
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(6,7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 70,295))
i=136
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(6)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 65))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C3) Curvas con L_2 out inf
outliers_L2 = quantile(curvas_GM$L_2, probs = 0.25, na.rm = TRUE) - 1.5*IQR(curvas_GM$L_2, na.rm = TRUE)
indices_outliers_L2 <- which(curvas_GM$L_2 < outliers_L2)
i=43
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(1)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 225))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#C4 outliers L_3
lim_inf_L3 <- quantile(curvas_GM$L_3, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(curvas_GM$L_3, na.rm = TRUE)
indices_outliers_L3_inf <- which(curvas_GM$L_3 < lim_inf_L3)
plotear_grupo_curvas_con_CM(indices_outliers_L3_inf,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok salvo 172
i=172
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(3)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 290))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#outliers L_4
lim_inf_L4 <- quantile(curvas_GM$L_4, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(curvas_GM$L_4, na.rm = TRUE)
indices_outliers_L4_inf <- which(curvas_GM$L_4 < lim_inf_L4)
plotear_grupo_curvas_con_CM(indices_outliers_L4_inf,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok 

#outliers L_5
lim_inf_L5 <- quantile(curvas_GM$L_5, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(curvas_GM$L_5, na.rm = TRUE)
indices_outliers_L5_inf <- which(curvas_GM$L_5 < lim_inf_L5)
plotear_grupo_curvas_con_CM(indices_outliers_L5_inf,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok 

#outliers L_2 high
outliers_L2high = quantile(curvas_GM$L_2, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_GM$L_2, na.rm = TRUE)
indices_outliers_L2high <- which(curvas_GM$L_2 > outliers_L2high)
plotear_grupo_curvas(indices_outliers_L2high, curvas_GM$curva, curvas_GM$limites) #154,175,263
i=154
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(3)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 60))
i=175
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(2)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 60))
i=263
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(2)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 65))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

#outliers L_3 high
outliers_L3high = quantile(curvas_GM$L_3, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_GM$L_3, na.rm = TRUE)
indices_outliers_L3high <- which(curvas_GM$L_3 > outliers_L3high)
plotear_grupo_curvas(indices_outliers_L3high, curvas_GM$curva, curvas_GM$limites) #todo ok

#outliers L_4 high
outliers_L4high = quantile(curvas_GM$L_4, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_GM$L_4, na.rm = TRUE)
indices_outliers_L4high <- which(curvas_GM$L_4 > outliers_L4high)
plotear_grupo_curvas(indices_outliers_L4high, curvas_GM$curva, curvas_GM$limites) #todo ok

#outliers L_5 high
outliers_L5high = quantile(curvas_GM$L_5, probs = 0.75, na.rm = TRUE) +1.5*IQR(curvas_GM$L_5, na.rm = TRUE)
indices_outliers_L5high <- which(curvas_GM$L_5 > outliers_L5high)
plotear_grupo_curvas(indices_outliers_L5high, curvas_GM$curva, curvas_GM$limites) #todo ok

#C3 inspección visual
curvas_GM <- curvas_GM[, !names(curvas_GM) %in% c("L_9", "L_10", "L_11","L_12", "L_13", "L_14", "L_15", "L_16","L_17", "L_18", "L_19", "L_20", "L_21","L_22", "L_23", "L_24", "L_25")]
curvas_GM<- curvas_GM[order(curvas_GM$L_7, decreasing = TRUE), ]
plotear_grupo_curvas_con_CM(1:264,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
indices_a_revisar = c(28,86,97,100,146,197,239)
i=28
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 238))
i=86
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 299))
i=97
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 290))
i=100
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 222))
i=146
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(7)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 285))
i=197
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(6)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 222))
i=239
plot(curvas_GM$curva[[i]], col = "blue", pch = 16, cex = 0.3, 
     xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva",i), type = "l")
abline(v =curvas_GM$limites[[i]], col = "red", lwd = 2, lty = 2)
curvas_GM$limites[[i]] <- curvas_GM$limites[[i]][-c(5)]
curvas_GM$limites[[i]] <- sort(c(curvas_GM$limites[[i]], 176))

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_8 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_8", 
        xlab = "Límites", 
        col = rainbow(max_limites))

################################################################### 2° Corrección a 6 limites + 2 limites_CM
curvas_GM<- curvas_GM[order(curvas_GM$L_7, decreasing = TRUE), ]
plotear_grupo_curvas_con_CM(1:264,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM)
CM67 = c(1:168,170:191,193:206,208,209,212,213,215,216,218,219,221:223,225,228,232,235:237)
CM56 = c(169,192,210,211,214,217,220,224,226,227,229:231,233,234,238:242,245:248,250:257,259,260,262,264)
CM45 = c(207,243,244,249,258,261)
CM46 = 263
for (i in CM67) {
  # Extraer los límites originales
  limites_originales <- curvas_GM$limites[[i]]
    # Guardar los valores en posición 6 y 7 en limites_CM
  curvas_GM$limites_CM[[i]] <- limites_originales[6:7]
    # Eliminar los valores de posición 6 y 7 en limites y conservar los demás
  curvas_GM$limites[[i]] <- limites_originales[c(1:5, 8)]
}
for (i in CM56) {
  # Extraer los límites originales
  limites_originales <- curvas_GM$limites[[i]]
  # Guardar los valores en posición 5 y 6 en limites_CM
  curvas_GM$limites_CM[[i]] <- limites_originales[5:6]
  # Eliminar los valores de posición 5 y 6 en limites y conservar los demás
  curvas_GM$limites[[i]] <- limites_originales[c(1:4, 7:8)]
}
for (i in CM45) {
  # Extraer los límites originales
  limites_originales <- curvas_GM$limites[[i]]
  # Guardar los valores en posición 4 y 5 en limites_CM
  curvas_GM$limites_CM[[i]] <- limites_originales[4:5]
  # Eliminar los valores de posición 4 y 5 en limites y conservar los demás
  curvas_GM$limites[[i]] <- limites_originales[c(1:3, 6:8)]
}
for (i in CM46) {
  # Extraer los límites originales
  limites_originales <- curvas_GM$limites[[i]]
  # Guardar los valores en posición 4 y 6 en limites_CM
  curvas_GM$limites_CM[[i]] <- limites_originales[c(4,6)]
  # Eliminar los valores de posición 4 y 6 en limites y conservar los demás
  curvas_GM$limites[[i]] <- limites_originales[c(1:3,5,7:8)]
}
plotear_grupo_curvas_con_CM(1:264,curvas_GM$curva,curvas_GM$limites,curvas_GM$limites_CM) #todo ok

#Recalcular columnas:
curvas_GM[, c("cant_limite", "L_1","L_2","L_3","L_4","L_5","L_6","L_7","L_8","L_9","L_10","L_11","L_12","L_13","L_14","L_15","L_16","L_17","L_18","L_19","L_20","L_21","L_22","L_23","L_24","L_25")] <- NA
cantidad_limites <- sapply(curvas_GM$limites, length)
curvas_GM$cant_limite <- cantidad_limites
max_limites <- max(sapply(curvas_GM$limites, length))
curvas_GM[c(paste0("L_", 1:max_limites))] <- do.call(cbind, lapply(1:max_limites, function(i) sapply(curvas_GM$limites, function(x) x[i])))
#Regraficar:
hist(curvas_GM$cant_limite, main = "Histograma de Cantidad de límites de PPS 6 (GM)", xlab = "Cantidad de límites", ylab = "Frecuencia", col = "lightblue", border = "black")
boxplot(curvas_GM[, paste0("L_", 1:max_limites)], 
        main = "Boxplot de L_1 a L_6 PPS 6 (GM)", 
        ylab = "Valores de L_1 a L_6", 
        xlab = "Límites", 
        col = rainbow(max_limites))
summary(curvas_GM[, c("L_1", "L_2", "L_3", "L_4", "L_5", "L_6")])

# Limites de CM
limite_1 <- sapply(curvas_GM$limites_CM, `[`, 1)
limite_2 <- sapply(curvas_GM$limites_CM, `[`, 2)
boxplot(limite_1, limite_2, names = c("Límite 1", "Límite 2"), 
        main = "Distribución de límites_CM", ylab = "Valores", col = c("lightgreen", "darkgreen"))
summary(limite_1)
summary(limite_2)

curvas_GM <- curvas_GM[, !names(curvas_GM) %in% c("L_7", "L_8", "L_9", "L_10", "L_11","L_12", "L_13", "L_14", "L_15", "L_16","L_17", "L_18", "L_19", "L_20", "L_21","L_22", "L_23", "L_24", "L_25")]
all_curvas = rbind(curvas_noGM,curvas_GM)
all_curvas$CM_L1 <- sapply(all_curvas$limites_CM, function(x) as.numeric(x[[1]]))
all_curvas$CM_L2 <- sapply(all_curvas$limites_CM, function(x) as.numeric(x[[2]]))
all_curvas <- all_curvas[, !names(all_curvas) %in% c("limites", "cant_limite","limites_CM")]
save(all_curvas,file="all_curvas_2.RData")
