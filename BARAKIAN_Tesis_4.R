#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 4 = Tratamiento de las imagenes - Escalado a todas las imagenes

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")
gc()

load("fx_img_a_curva.RData")
load("all_images.RData")

#LIBRERIAS
library(dplyr)
library(ggplot2)
library(purrr)

#APLICACION DE LAS FUNCIONES 1 a 12

#A_F3.1 (Aplicación de la F3.1)
all_images$imagen_recortada1 <- lapply(all_images$imagen_matrix_RGB, fx_img_a_curva$F1)
all_images <- all_images[, !(names(all_images) %in% c("imagen_matrix_RGB"))]
gc()
#A_F3.2 (Aplicación de la F3.2)
all_images$imagen_recortada2 <- lapply(all_images$imagen_recortada1, fx_img_a_curva$F2)
all_images <- all_images[, !(names(all_images) %in% c("imagen_recortada1"))]
gc()
#A_F3.3 (Aplicación de la F3.3)
all_images$red_umb <- lapply(all_images$imagen_recortada2, fx_img_a_curva$F3)
all_images <- all_images[, !(names(all_images) %in% c("imagen_recortada2"))]
gc()
#A_F3.4 (Aplicación de la F3.4)
all_images$bordes1 <- lapply(all_images$red_umb, fx_img_a_curva$F4)
#A_F3.5 (Aplicación de la F3.5)
all_images$bordes2 <- lapply(all_images$bordes1, fx_img_a_curva$F5)
#A_F3.6A (Aplicación de la F3.6A)
all_images$eje1 <- lapply(all_images$bordes2, fx_img_a_curva$F6A)
all_images <- all_images[, !(names(all_images) %in% c("bordes1","bordes2"))]
gc()
#A_F3.6B (Aplicación de la F3.6B)
all_images$eje2 <- lapply(all_images$eje1, fx_img_a_curva$F6B)
#A_F3.6C (Aplicación de la F3.6C)
all_images$eje3 <- lapply(all_images$eje2, fx_img_a_curva$F6C)
#A_F3.7A (Aplicación de la F3.7A)
all_images$imagen_curva_limites <- lapply(all_images$eje3, fx_img_a_curva$F7A)
all_images <- all_images[, !(names(all_images) %in% c("eje1","eje2","eje3"))]
gc()
#A_F3.7B (Aplicación de la F3.7B)
all_images$limites <- fx_img_a_curva$F7B(all_images$imagen_curva_limites)
#A_F3.7C (Aplicación de la F3.7C)
all_images <- fx_img_a_curva$F7C(all_images)
#A_F3.7B (Aplicación de la F3.7B)  #REAPLICACION
all_images$limites <- fx_img_a_curva$F7B(all_images$imagen_curva_limites)
#A_F3.8 (Aplicación de la F3.8)
all_images$curva1 <- lapply(all_images$imagen_curva_limites, fx_img_a_curva$F8)
#A_F3.9 (Aplicación de la F3.9)
all_images$curva2 <- lapply(all_images$curva1, fx_img_a_curva$F9)
#A_F3.10 (Aplicación de la F3.10)
all_images$curva3 <- lapply(all_images$curva2, fx_img_a_curva$F10, num_puntos = 300)
#A_F3.11 (Aplicación de la F3.11)
all_images$curva4 <- lapply(all_images$curva3, fx_img_a_curva$F11)
#A_F3.12 (Aplicación de la F3.12)
all_images$limites_ajustados <- mapply(
  fx_img_a_curva$F12,
  limites = all_images$limites,
  longitud_original = lengths(all_images$curva2),
  MoreArgs = list(longitud_nueva = 300),
  SIMPLIFY = FALSE
)
all_images <- all_images[, !(names(all_images) %in% c("limites","curva1","curva2","curva3"))]
gc()
str(all_images)

#CHEQUEO GENERAL DE ERRORES DE CURVA

plotear_grupo_curvas <- function(inicio, curvas, limites) {
  # Calcular el rango de las 100 curvas a mostrar
  fin <- inicio + 99  # Fin del rango (100 curvas)
  indices <- inicio:fin  # Secuencia de índices de las curvas a mostrar
  
  # Asegurarse de que los índices no excedan el número de curvas disponibles
  indices <- indices[indices <= length(curvas)]
  
  # Configurar el diseño de gráficos 10x10
  par(mfrow = c(10, 10), mar = c(3, 3, 2, 1))  # 10 filas y 10 columnas
  
  # Graficar cada una de las curvas seleccionadas
  for (i in indices) {
    plot(curvas[[i]], col = "blue", pch = 16, cex = 0.3, 
         xlab = "Índice (X)", ylab = "Fila (Y)", main = paste("Curva", i), type = "l")
    # Agregar las líneas de los límites ajustados
    #abline(v = limites[[i]], col = "red", lwd = 2, lty = 2)
  }
  
  # Restaurar el diseño de gráficos por defecto
  par(mfrow = c(1, 1))
}

x11();plotear_grupo_curvas(1, all_images$curva4, all_images$limites_ajustados)
x11();plotear_grupo_curvas(101, all_images$curva4, all_images$limites_ajustados) #183 ult
x11();plotear_grupo_curvas(201, all_images$curva4, all_images$limites_ajustados) 
x11();plotear_grupo_curvas(301, all_images$curva4, all_images$limites_ajustados) #308,309, eje y
x11();plotear_grupo_curvas(401, all_images$curva4, all_images$limites_ajustados) 
x11();plotear_grupo_curvas(501, all_images$curva4, all_images$limites_ajustados) #541 ult
x11();plotear_grupo_curvas(601, all_images$curva4, all_images$limites_ajustados) #642, 659, 691, 698 ult
x11();plotear_grupo_curvas(701, all_images$curva4, all_images$limites_ajustados) #754 eje y
x11();plotear_grupo_curvas(801, all_images$curva4, all_images$limites_ajustados) #822 ult
x11();plotear_grupo_curvas(901, all_images$curva4, all_images$limites_ajustados) 
x11();plotear_grupo_curvas(1001, all_images$curva4, all_images$limites_ajustados) 
x11();plotear_grupo_curvas(1101, all_images$curva4, all_images$limites_ajustados) 
x11();plotear_grupo_curvas(1201, all_images$curva4, all_images$limites_ajustados) 

############### CORRECCIÓN DE ERRORES DE CURVA

#A) ERROR ASOCIADO AL ÚLTIMO PUNTO

plot(as.raster(all_images$imagen_curva_limites[[183]]))
plot(as.raster(all_images$imagen_curva_limites[[541]]))
plot(as.raster(all_images$imagen_curva_limites[[822]]))

# Filtrar registros y devolver solo el número de registro y los últimos 10 valores de curva4
ult_10 <- all_images %>% 
  mutate(registro = row_number()) %>% # Agregar número de registro como columna
  filter(map_lgl(curva4, ~ any(.x[291:300] > 80, na.rm = TRUE))) %>% 
  transmute(
    registro,  # Mantener solo el número de registro
    valores_ultimos_10 = map(curva4, ~ round(.x[291:300])) # Extraer y redondear últimos 10 valores
  )

indices = ult_10$registro
indices
all_images$curva4[indices]
ult_10

# Modificar las curvas correspondientes
all_images$curva4[indices] <- lapply(all_images$curva4[indices], function(curva) {
  curva[300] <- 0       # Reemplazar la posición 300 por 0
  curva[298:299] <- NA  # Reemplazar las posiciones 298 a 299 por NA
  return(curva)
})

# Verificar los resultados para los índices seleccionados
all_images$curva4[indices]
all_images$curva4[indices] <- lapply(all_images$curva4[indices], fx_img_a_curva$F9)
all_images$curva4[183]

#verif
curva_183 <- unlist(all_images$curva4[183])
curva_541 <- unlist(all_images$curva4[541])
curva_822 <- unlist(all_images$curva4[822])

plot(curva_183, col = "blue", pch = 16, cex = 0.5, type = "l",
     xlab = "Índice (X)", ylab = "Valor (Y)",
     main = paste("Curva 183"))
plot(curva_541, col = "blue", pch = 16, cex = 0.5, type = "l",
     xlab = "Índice (X)", ylab = "Valor (Y)",
     main = paste("Curva 541"))
plot(curva_822, col = "blue", pch = 16, cex = 0.5, type = "l",
     xlab = "Índice (X)", ylab = "Valor (Y)",
     main = paste("Curva 822"))

#B) 
#ERROR ASOCIADO AL EJE Y
# Índices específicos a graficar
indices <- c(308, 309, 754)

# Graficar las curvas seleccionadas
par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))  # 4 filas y 4 columnas para visualización
for (i in indices) {
  # Obtener los primeros 50 puntos de la curva
  curva <- all_images$curva4[[i]][1:50]
  
  # Graficar la curva
  plot(curva, col = "blue", pch = 16, cex = 0.5, type = "l", 
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))

# Función de ajuste
ajustar_curva <- function(curva) {
  curva_ajustada <- numeric(length(curva))
  curva_ajustada[1] <- 0  # Posición 1 se fija en 0
  curva_ajustada[20] <- curva[20]  # Posición 25 conserva su valor
  curva_ajustada[2:19] <- seq(from = 0, to = curva[20], length.out = 19)[2:19]  # Interpolación
  curva_ajustada[20:length(curva)] <- curva[20:length(curva)]  # Mantener el resto sin cambios
  return(curva_ajustada)
}

# Aplicar la función solo a los índices seleccionados
for (i in indices) {
  all_images$curva4[[i]] <- ajustar_curva(all_images$curva4[[i]])
}

# Graficar las curvas seleccionadas
par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))  # 4 filas y 4 columnas para visualización
for (i in indices) {
  # Obtener los primeros 50 puntos de la curva
  curva <- all_images$curva4[[i]][1:50]
  
  # Graficar la curva
  plot(curva, col = "blue", pch = 16, cex = 0.5, type = "l", 
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))

#Guardar en objeto para continuar trabajando
str(all_images)
all_images <- all_images[, !(names(all_images) %in% c("alto","ancho","pixeles_totales","imagen_matrix_RGB","red_umb","imagen_curva_limites"))]
names(all_images)[names(all_images) == "curva4"] <- "curva"
names(all_images)[names(all_images) == "limites_ajustados"] <- "limites"
names(all_images)[names(all_images) == "carpeta"] <- "PPS"
all_curvas <- all_images
rm(all_images)
gc()
save(all_curvas,file = "all_curvas.RData")

load("all_curvas.RData")


#Chequeo de errores pequeños en la curva por limites mal detectados

# Búsqueda de pequeñas distorsiones (subidas y bajadas rapidas)
indices_cumple_condicion <- vector("list", length = nrow(all_curvas))
for (i in 1:nrow(all_curvas)) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva en la posición i
  # Inicializar un vector para almacenar los índices que cumplen la condición en cada curva
  indices <- vector()
  # Recorrer la curva (hasta el antepenúltimo valor para no salir de los límites)
  for (j in 1:(length(curva) - 2)) {
    if (curva[j + 1] - curva[j] > 2 && curva[j + 2] - curva[j + 1] < -2) {
      indices <- c(indices, j)  # Agregar el índice si cumple la condición
    }
  }
  # Almacenar los índices de la curva que cumplen la condición
  if (length(indices) > 0) {
    indices_cumple_condicion[[i]] <- indices
  }
}

# Filtrar las curvas donde se cumple la condición
resultados <- which(sapply(indices_cumple_condicion, length) > 0)
resultados


# Graficar cada curva en la cuadrícula
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
for (i in resultados) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva correspondiente
  # Graficar la curva
  plot(curva, type = "l", col = "blue", pch = 16, cex = 0.5,
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))


# Modificar las curvas que cumplen la condición y actualizar el dataset
correccion_fina_mascara3 <- function(curva) {
  for (j in 1:(length(curva) - 2)) {
    if (curva[j + 1] - curva[j] > 2 && curva[j + 2] - curva[j + 1] < -2) {
      curva[j + 1] <- mean(c(curva[j], curva[j + 2]))  # Reemplazar X+1 con el promedio de X y X+2
    }
  }
  return(curva)
}

for (i in resultados) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva correspondiente
  curva_modificada <- correccion_fina_mascara3(curva)  # Modificar la curva
  
  # Guardar la curva modificada en el dataset
  all_curvas$curva[[i]] <- curva_modificada
}

# Graficar las curvas modificadas
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
for (i in resultados) {
  curva_modificada <- all_curvas$curva[[i]]  # Extraer la curva modificada
  # Graficar la curva modificada
  plot(curva_modificada, type = "l", col = "blue", pch = 16, cex = 0.5,
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))


#### con mascara de 5

# Inicializar una lista para guardar los índices de las curvas que cumplen la condición
indices_cumple_condicion <- vector("list", length = nrow(all_curvas))

for (i in 1:nrow(all_curvas)) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva en la posición i
  # Inicializar un vector para almacenar los índices que cumplen la condición en cada curva
  indices <- vector()
  # Recorrer la curva (hasta el antepenúltimo valor para no salir de los límites)
  for (j in 1:(length(curva) - 4)) {
    # Verificar si la diferencia entre x y x+2 es positiva y entre x+2 y x+4 es negativa
    if (curva[j + 2] - curva[j] > 3 && curva[j + 4] - curva[j + 2] < -3) {
      indices <- c(indices, j)  # Agregar el índice si cumple la condición
    }
  }
  # Almacenar los índices de la curva que cumplen la condición
  if (length(indices) > 0) {
    indices_cumple_condicion[[i]] <- indices
  }
}

# Filtrar las curvas donde se cumple la condición
resultados <- which(sapply(indices_cumple_condicion, length) > 0)
resultados

# Graficar cada curva en la cuadrícula
par(mfrow = c(3, 3), mar = c(3, 3, 2, 1))
for (i in resultados) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva correspondiente
  
  # Graficar la curva
  plot(curva, type = "l", col = "blue", pch = 16, cex = 0.5,
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))


# Modificar las curvas que cumplen la condición y actualizar el dataset
correccion_fina_mascara5 <- function(curva) {
  for (j in 1:(length(curva) - 4)) {
    # Verificar la condición en la cual la diferencia entre x y x+2 es positiva y entre x+2 y x+4 es negativa
    if (curva[j + 2] - curva[j] > 3 && curva[j + 4] - curva[j + 2] < -3) {
      # Primero actualizar j + 2 con el valor medio entre j y j + 4 (es lo que afecta las siguientes actualizaciones)
      curva[j + 2] <- mean(c(curva[j], curva[j + 4]))
      
      # Luego, reemplazar j + 1 con el promedio de j y j + 2 (ya actualizados)
      curva[j + 1] <- mean(c(curva[j], curva[j + 2]))  
      
      # Finalmente, reemplazar j + 3 con el promedio de j + 2 y j + 4 (ya actualizados)
      curva[j + 3] <- mean(c(curva[j + 2], curva[j + 4]))
    }
  }
  return(curva)
}

# Modificar y actualizar el dataset con las curvas modificadas
for (i in resultados) {
  curva <- all_curvas$curva[[i]]  # Extraer la curva correspondiente
  curva_modificada <- correccion_fina_mascara5(curva)  # Modificar la curva
  
  # Guardar la curva modificada en el dataset
  all_curvas$curva[[i]] <- curva_modificada
}

# Graficar las curvas modificadas
par(mfrow = c(3,3), mar = c(3, 3, 2, 1))
for (i in resultados) {
  curva_modificada <- all_curvas$curva[[i]]  # Extraer la curva modificada
  # Graficar la curva modificada
  plot(curva_modificada, type = "l", col = "blue", pch = 16, cex = 0.5,
       xlab = "Índice (X)", ylab = "Valor (Y)", 
       main = paste("Curva", i))
}
par(mfrow = c(1, 1))

save(all_curvas,file = "all_curvas.RData")
