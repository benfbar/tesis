#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 3A = Tratamiento de las imagenes - Pruebas con subconjunto de imágenes "extremas"

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

#librerias
library(dplyr)
library(ggplot2)

load("test_images.RData")

plot(as.raster(test_images$imagen_matrix_RGB[[1]]))
plot(as.raster(test_images$imagen_matrix_RGB[[2]]))
plot(as.raster(test_images$imagen_matrix_RGB[[3]]))
plot(as.raster(test_images$imagen_matrix_RGB[[4]]))
plot(as.raster(test_images$imagen_matrix_RGB[[5]]))
plot(as.raster(test_images$imagen_matrix_RGB[[6]]))
plot(as.raster(test_images$imagen_matrix_RGB[[7]]))
plot(as.raster(test_images$imagen_matrix_RGB[[8]]))
plot(as.raster(test_images$imagen_matrix_RGB[[9]]))
plot(as.raster(test_images$imagen_matrix_RGB[[10]]))
plot(as.raster(test_images$imagen_matrix_RGB[[11]]))
plot(as.raster(test_images$imagen_matrix_RGB[[12]]))

# Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$imagen_matrix_RGB[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))

#P1 (Problema 1)_ Hay imágenes con bloques negros 
#F3.1 (Función 3.1) _ RECORTAR LAS IMAGENES CON BLOQUES NEGROS (más del 95% con negro)
F3.1_recortar_bloques_negros <- function(matriz) {
  matriz_2d <- matriz[, , 1]  # Usar solo el primer canal, si hay varios
    # Umbral para determinar si una fila o columna es "mayormente negra"
  umbral_negro <- 0.95
    # Identificar las filas mayormente negras
  filas_no_cero <- apply(matriz_2d, 1, function(fila) mean(fila == 0) < umbral_negro)
    # Identificar las columnas mayormente negras
  columnas_no_cero <- apply(matriz_2d, 2, function(columna) mean(columna == 0) < umbral_negro)
    # Recortar la matriz eliminando filas y columnas mayormente negras
  matriz_recortada <- matriz[filas_no_cero, columnas_no_cero, , drop = FALSE]
    return(matriz_recortada)
}

#A_F3.1 (Aplicación de la F3.1)
test_images$imagen_recortada1 <- lapply(test_images$imagen_matrix_RGB, F3.1_recortar_bloques_negros)


#P2 (Problema 2)_ Hay imágenes con bloques inferior blanco 
#plot(as.raster(test_images$imagen_recortada1[[2]]))
#tail(test_images$imagen_recortada1[[2]], 1)

#F3.2 (Función 3.2) _ RECORTAR LAS IMAGENES CON BLOQUE INFERIOR BLANCO (más del 95% con BLANCO)
F3.2_recortar_bloques_blancos <- function(matriz) {
  # Limitar la iteración a las últimas 10 filas
  filas_a_eliminar <- c()  # Vector para almacenar las filas a eliminar
  for (i in (nrow(matriz) - 9):nrow(matriz)) {  # Asegura que no haya más de 10 filas
    # Verificar si el promedio del segundo canal es mayor a 0.95
    if (mean(matriz[i, , 2]) > 0.95) {
      filas_a_eliminar <- c(filas_a_eliminar, i)  # Almacenar las filas a eliminar
    }
  }
    # Eliminar las filas que son mayormente blancas
  if (length(filas_a_eliminar) > 0) {
    matriz <- matriz[-filas_a_eliminar, , ]
  }
    return(matriz)
}

#A_F3.2 (Aplicación de la F2)
test_images$imagen_recortada2 <- lapply(test_images$imagen_recortada1, F3.2_recortar_bloques_blancos)

# VERIFICACION 1 y 2 _ Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$imagen_recortada2[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))


#Problema 3: Imagen de 3 dimensiones, se decide trabajar con canal ROJO tras haber descartado otras opciones (script 3B)
plot_rgb_channels <- function(image, main_title = "RGB Channels") {
  channel_r <- image[,,1]
  channel_g <- image[,,2]
  channel_b <- image[,,3]
  par(mfrow = c(1, 3), mar = c(3, 3, 2, 1))
  plot(as.raster(channel_r), main = paste(main_title, "- Red"), col = gray.colors(256))
  plot(as.raster(channel_g), main = paste(main_title, "- Green"), col = gray.colors(256))
  plot(as.raster(channel_b), main = paste(main_title, "- Blue"), col = gray.colors(256))
  par(mfrow = c(1, 1))
}

plot_rgb_channels(test_images$imagen_recortada2[[1]])

#F3.3 (Función 3.3) _ Función para aislar y umbralizar solamente canal R
F3.3_procesar_canal_rojo <- function(imagen, umbral = 0.9) {
  # Verificar si la imagen tiene 3 dimensiones
  if (length(dim(imagen)) != 3 || dim(imagen)[3] < 1) {
    stop("La imagen debe tener al menos una capa en la tercera dimensión.")
  }
    # Extraer el canal rojo (primer canal)
  canal_rojo <- imagen[, , 1]
    # Umbralizar el canal rojo
  canal_rojo_umbralizado <- ifelse(canal_rojo >= umbral, 1, 0)
    return(canal_rojo_umbralizado)
}

#A_F3.3 (Aplicación de la F3.3)
test_images$red_umb <- lapply(test_images$imagen_recortada2, F3.3_procesar_canal_rojo)

# VERIFICACION 3 _ Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$red_umb[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))


#Problema 4 y 5 - Imagenes con bordes superior o derecho negros
#plot(as.raster(test_images$red_umb[[5]])) #arriba
#plot(as.raster(test_images$red_umb[[1]])) #derecha
#plot(as.raster(test_images$red_umb[[3]])) #derecha
#plot(as.raster(test_images$red_umb[[11]])) #derecha

#F3.4 (Función 3.4) - Función para modificar las primeras 15 filas
F3.4_eliminar_linea_arriba <- function(matriz) {
  for (i in 1:min(15, nrow(matriz))) {  # Asegura que no haya más de 15 filas
        if (mean(matriz[i, ] < 1) > 0.5) {
          matriz[i, ] <- 1
    }
  }
    return(matriz)
}

#A_F3.4 (Aplicación de la F3.4)
test_images$bordes1 <- lapply(test_images$red_umb, F3.4_eliminar_linea_arriba)


#F3.5 (Función 3.5) - para modificar las últimas 15 columnas
F3.5_eliminar_linea_derecha <- function(matriz) {
  for (j in (ncol(matriz) - 14):ncol(matriz)) {  
    if (mean(matriz[, j] < 1) > 0.5) {
      matriz[, j] <- 1
    }
  }
    return(matriz)
}


#A_F3.5 (Aplicación de la F5)
test_images$bordes2 <- lapply(test_images$bordes1, F3.5_eliminar_linea_derecha)


# VERIFICACION 4 y 5 _ Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$bordes2[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))


#Problema 6 - No todas las imágenes tienen ejes X e Y, las que tienen eje Y no necesariamente está en la primer columna
#F3.6A (Función 3.6A) _ Detección y eliminación de líneas horizontales asociadas al EJE Y
F3.6A_lineas_horiz_ejeY <- function(imagen) {
  # Calcular el número de columnas a considerar (7% de las columnas)
  n_columnas <- ncol(imagen)
  n_columnas_5 <- ceiling(0.07 * n_columnas)  
  columnas_seleccionadas <- imagen[, 1:n_columnas_5]  
  # Calcular el número de filas a considerar (95% de las filas superiores)
  n_filas <- nrow(imagen)
  n_filas_95 <- floor(0.95 * n_filas)  
  filas_seleccionadas <- columnas_seleccionadas[1:n_filas_95, ]  
  # Crear una copia de la imagen para modificar solo los valores detectados
  imagen_modificada <- imagen
  # Iterar sobre cada fila de la matriz seleccionada
  for (fila_idx in 1:nrow(filas_seleccionadas)) {
    fila <- filas_seleccionadas[fila_idx, ]
    # Detectar secuencias consecutivas de ceros
    rle_result <- rle(fila)
    start_indices <- cumsum(c(1, rle_result$lengths[-length(rle_result$lengths)]))
    end_indices <- cumsum(rle_result$lengths)
    # Identificar tramos de ceros con longitud mínima de 2
    for (i in seq_along(rle_result$values)) {
      if (rle_result$values[i] == 0 && rle_result$lengths[i] >= 2) {
        # Modificar únicamente los elementos correspondientes a esta línea horizontal,
        # excepto la primera columna
        start <- max(start_indices[i], 2)  # Asegurar que no modifique la primera columna
        end <- end_indices[i]
        imagen_modificada[fila_idx, start:end] <- 1
      }
    }
  }
  return(imagen_modificada)
} 

#A_F3.6A (Aplicación de la F3.6A)
test_images$eje1 <- lapply(test_images$bordes2, F3.6A_lineas_horiz_ejeY)

#F3.6B (Función 3.6B) _ Detección y eliminación del EJE Y
F3.6B_ejeY <- function(imagen) {
  # Número de filas y columnas de la imagen
  n_filas <- nrow(imagen)
  n_columnas <- ncol(imagen)
  
  # Calcular el 7% de las columnas desde el lado izquierdo
  n_columnas_5 <- floor(0.07 * n_columnas)
  columnas_seleccionadas <- imagen[, 1:n_columnas_5]
  
  # Lista para almacenar las posiciones de las columnas donde se detectan líneas verticales de ceros
  posiciones_ceros <- integer(0)
  
  # Iterar sobre cada columna de la región seleccionada
  for (col_idx in 1:ncol(columnas_seleccionadas)) {
    columna <- columnas_seleccionadas[, col_idx]
    
    # Detectar secuencias consecutivas de ceros
    rle_result <- rle(columna)
    tramos_ceros <- which(rle_result$values == 0 & rle_result$lengths >= 15)  # Longitud mínima de 15
    # Si hay tramos de ceros, almacenar la posición de la columna
    if (length(tramos_ceros) > 0) {
      posiciones_ceros <- c(posiciones_ceros, col_idx)
    }
  }
  
  # Pintar las columnas completas seleccionadas en toda la imagen
  for (col_idx in posiciones_ceros) {
    imagen[, col_idx] <- 1
  }
  
  return(imagen)
}

#A_F3.6B (Aplicación de la F3.6B)
test_images$eje2 <- lapply(test_images$eje1, F3.6B_ejeY)

#F3.6C (Función 3.6C) Agregar EJES en bordes inferior e izquierdo
F3.6C_agregar_ejes <- function(matriz) {
  matriz[, 1] <- 0
  matriz[nrow(matriz),] <- 0
  return(matriz)
}

#A_F3.6C (Aplicación de la F3.6C)
test_images$eje3 <- lapply(test_images$eje2, F3.6C_agregar_ejes)

# VERIFICACION 6A-C _ Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$eje3[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))

#Problema 7 : Detección de limites entre fracciones
#F3.7A (Función 3.7A) Detección de límites de fracciones (líneas verticales)
F3.7A_deteccion_limites <- function(imagen) {
  # Calcular el número de filas a considerar
  n_filas <- nrow(imagen)
  filas_seleccionadas <- imagen[1:n_filas, ]
  
  # Calcular el número de columnas a considerar (evitar 12% de las columnas a la izq)
  n_columnas <- ncol(imagen)
  n_columnas_12 <- floor(0.12 * n_columnas)  # Seleccionar el 12% de las columnas
  columnas_seleccionadas <- filas_seleccionadas[, n_columnas_12:n_columnas]  # Seleccionar las columnas del 12% al 100%
  
  # Remover la primera columna
  imagen_sin_primera <- columnas_seleccionadas[, -1]
  
  # Crear un vector para marcar las columnas a modificar
  columnas_a_modificar <- rep(FALSE, ncol(imagen_sin_primera))
  columnas_a_modificar2 <- rep(FALSE, ncol(imagen_sin_primera))  # Nueva variable para reemplazar columnas pegadas
  
  # Lista para almacenar las posiciones de las columnas donde se detectan líneas verticales de ceros
  posiciones_ceros <- integer(0)
  
  # Iterar sobre cada columna de la matriz
  for (col_idx in 1:ncol(imagen_sin_primera)) {
    columna <- imagen_sin_primera[, col_idx]
    
    # Detectar secuencias consecutivas de ceros
    rle_result <- rle(columna)
    tramos_ceros <- which(rle_result$values == 0 & rle_result$lengths >= 4)  # Longitud mínima de 5
    
    # Si hay tramos de ceros, marcar la columna como a modificar
    if (length(tramos_ceros) > 0) {
      columnas_a_modificar[col_idx] <- TRUE
      posiciones_ceros <- c(posiciones_ceros, col_idx)
    }
  }
  
  # Chequeo 1: En las primeras 15% de columnas, solo conservar la última
  posiciones_ceros_en_15 <- posiciones_ceros[posiciones_ceros <= floor(0.15 * ncol(imagen_sin_primera))]
  if (length(posiciones_ceros_en_15) > 1) {
    columnas_a_modificar[posiciones_ceros_en_15[-length(posiciones_ceros_en_15)]] <- FALSE
  }
  
  # Chequeo 2: En el 85% restante, si hay límites detectados consecutivos en 3 (x, x+1, x+2, etc...), conservar solo el último
  posiciones_ceros_en_85 <- posiciones_ceros[posiciones_ceros > floor(0.15 * ncol(imagen_sin_primera))]
  if (length(posiciones_ceros_en_85) >= 3) {
    for (i in 1:(length(posiciones_ceros_en_85) - 2)) {
      if (posiciones_ceros_en_85[i + 1] == posiciones_ceros_en_85[i] + 1 && posiciones_ceros_en_85[i + 2] == posiciones_ceros_en_85[i] + 2) {
        columnas_a_modificar[posiciones_ceros_en_85[i]] <- FALSE
        columnas_a_modificar[posiciones_ceros_en_85[i + 1]] <- FALSE
      }
    }
  }
  
  # Chequeo 3: Verificar si las columnas detectadas están "pegadas" (diferencia de 1)
  if (length(posiciones_ceros) >= 2) {
    for (i in 1:(length(posiciones_ceros) - 1)) {
      if (abs(posiciones_ceros[i] - posiciones_ceros[i + 1]) == 1) {
        # Si están pegadas (diferencia de 1), marcar la segunda para reemplazar por la siguiente
        columnas_a_modificar2[posiciones_ceros[i]] <- FALSE
        if (i + 2 <= length(posiciones_ceros)) {
          columnas_a_modificar2[posiciones_ceros[i + 1]] <- TRUE
        }
      }
    }
  }
  
  # Pintar las columnas completas seleccionadas en toda la imagen,
  # excepto la última fila (ajustando ambas modificaciones)
  for (col_idx in which(columnas_a_modificar)) {
    imagen[-n_filas, n_columnas_12 + col_idx] <- 0.75
  }
  
  # Ahora reemplazar las columnas pegadas con la siguiente columna
  for (col_idx in which(columnas_a_modificar2)) {
    # Asegurarse de que no se pase del límite
    if (col_idx + 1 <= ncol(imagen_sin_primera)) {
      imagen[-n_filas, n_columnas_12 + col_idx] <- imagen[-n_filas, n_columnas_12 + col_idx + 1]
    }
  }
  
  return(imagen)
}


#A_F3.7A (Aplicación de la F3.7A)
test_images$imagen_curva_limites <- lapply(test_images$eje3, F3.7A_deteccion_limites)


# VERIFICACION 7A_ Graficar las imágenes en la grilla
par(mfrow = c(6, 2), mar = c(1, 1, 1, 1))
for (i in 1:12) {
  plot(as.raster(test_images$imagen_curva_limites[[i]]), main = paste("Imagen", i))
}
par(mfrow = c(1, 1))


#CANTIDAD de limites
#F3.7B (Función 3.7B) Almacenar las posiciones de las columnas en 0.75
F3.7B_limites <- function(lista_imagenes) {
  # Crear un vector para almacenar las posiciones de las columnas con 0.75
  limites <- vector("list", length(lista_imagenes))
  
  # Recorrer cada imagen en la lista
  for (i in seq_along(lista_imagenes)) {
    # Obtener la primera fila de la imagen actual
    primera_fila <- lista_imagenes[[i]][1, ]
    
    # Encontrar las posiciones de las columnas que contienen 0.75
    posiciones_75 <- which(primera_fila == 0.75)
    
    # Guardar las posiciones en el vector de límites
    limites[[i]] <- posiciones_75
  }
  
  # Devolver la lista de límites
  return(limites)
}

#A_F3.7B (Aplicación de la F3.7B)
test_images$limites <- F3.7B_limites(test_images$imagen_curva_limites)


# Verificar las posiciones de las columnas en la primera imagen
plot(as.raster(test_images$imagen_curva_limites[[1]]))
test_images$limites[1]  #6 lim
plot(as.raster(test_images$imagen_curva_limites[[2]]))
test_images$limites[2] #7 lim, doble limite al final
plot(as.raster(test_images$imagen_curva_limites[[3]]))
test_images$limites[3] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[4]]))
test_images$limites[4] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[5]]))
test_images$limites[5] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[6]]))
test_images$limites[6] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[7]]))
test_images$limites[7] #1, imagen pesima
plot(as.raster(test_images$imagen_curva_limites[[8]]))
test_images$limites[8] #9 lim, 3 erroneos
plot(as.raster(test_images$imagen_curva_limites[[9]]))
test_images$limites[9] #2, imagen pesima
plot(as.raster(test_images$imagen_curva_limites[[10]]))
test_images$limites[10] #5 lim
plot(as.raster(test_images$imagen_curva_limites[[11]]))
test_images$limites[11]  #9 lim, 1 erroneo
plot(as.raster(test_images$imagen_curva_limites[[12]]))
test_images$limites[12] #8 lim

F3.7C_ultimo_limite <- function(test_images) {
  # Recorre todas las imágenes en test_images
  for (i in seq_along(test_images$limites)) {
    limites <- test_images$limites[[i]]  # Obtener los límites de la imagen actual
    
    # Obtener la imagen actual
    imagen <- test_images$imagen_curva_limites[[i]]
    n_columnas <- ncol(imagen)
    
    # Calcular el rango del 5% final de columnas
    rango_5_final <- (n_columnas - floor(0.05 * n_columnas) + 1):n_columnas
    
    # Verificar las condiciones para agregar un nuevo límite
    if (length(limites) <= 5 || (length(limites) > 0 && !limites[length(limites)] %in% rango_5_final)) {
      n_filas <- nrow(imagen)
      
      # Reemplazar la última columna (excepto la última fila) con 0.75
      imagen[1:(n_filas - 1), n_columnas] <- 0.75  # Modificar la columna, excluyendo la última fila
      
      # Actualizar la imagen en la lista
      test_images$imagen_curva_limites[[i]] <- imagen
      
      # Agregar la posición del nuevo límite al final de la lista de límites
      test_images$limites[[i]] <- c(limites, n_columnas)
    }
  }
  
  return(test_images)
}


#A_F3.7C (Aplicación de la F3.7C)
test_images <- F3.7C_ultimo_limite(test_images)


#A_F3.7B (Aplicación de la F3.7B)  #REAPLICACION
test_images$limites <- F3.7B_limites(test_images$imagen_curva_limites)


# Verificar las posiciones de las columnas en la primera imagen
plot(as.raster(test_images$imagen_curva_limites[[1]]))
test_images$limites[1]  #6 lim
plot(as.raster(test_images$imagen_curva_limites[[2]]))
test_images$limites[2] #7 lim, doble limite al final
plot(as.raster(test_images$imagen_curva_limites[[3]]))
test_images$limites[3] #6 lim --> pasa 7 limites (ultimo doble)
plot(as.raster(test_images$imagen_curva_limites[[4]]))
test_images$limites[4] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[5]]))
test_images$limites[5] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[6]]))
test_images$limites[6] #6 lim
plot(as.raster(test_images$imagen_curva_limites[[7]]))
test_images$limites[7] #1, imagen pesima --> pasa a 2 lim
plot(as.raster(test_images$imagen_curva_limites[[8]]))
test_images$limites[8] #9 lim, 3 erroneos
plot(as.raster(test_images$imagen_curva_limites[[9]]))
test_images$limites[9] #2, imagen pesima --> pasa a 3 lim
plot(as.raster(test_images$imagen_curva_limites[[10]]))
test_images$limites[10] #5 lim --> pasa a 6 lim
plot(as.raster(test_images$imagen_curva_limites[[11]]))
test_images$limites[11]  #9 lim,  1 erroneo
plot(as.raster(test_images$imagen_curva_limites[[12]]))
test_images$limites[12] #8 lim

#Problema 8: Generacion de la CURVA
#F3.8 (Funcion 8)  - Obtencion de curva a partir de imagen_curva_limites (curva1)
F3.8_curva <- function(matriz) {
  # Obtener el número de filas y columnas de la matriz
  num_filas <- nrow(matriz)
  num_columnas <- ncol(matriz)
  
  # Inicializar un vector para almacenar los resultados
  fila_con_0 <- vector("list", num_columnas)  # Cambiamos a lista para permitir `NULL`
  
  # Recorrer cada columna
  for (col_index in seq_len(num_columnas)) {
    if (col_index == 1) {
      # Para la primera columna, devolver el número máximo de filas
      fila_con_0[[col_index]] <- num_filas
    } else {
      # Para las demás columnas, buscar el primer índice con valor 0 hasta la penúltima fila
      indices <- which(matriz[1:(num_filas - 1), col_index] == 0)
      if (length(indices) > 0) {
        fila_con_0[[col_index]] <- indices[1]  # Índice del primer 0 encontrado
      } else {
        fila_con_0[[col_index]] <- NULL  # Si no hay 0, asignar NULL
      }
    }
  }
  
  # Convertir la lista a un vector numérico, manteniendo NULL como vacío
  fila_con_0 <- sapply(fila_con_0, function(x) if (!is.null(x)) x else NA, simplify = TRUE)
  
  # Realizar la transformación adicional ((x * -1) + max(x)) solo en los valores no NULL
  if (any(!is.na(fila_con_0))) {
    fila_con_0 <- (fila_con_0 * -1) + max(fila_con_0, na.rm = TRUE)
  }
  
  return(fila_con_0)
}

#A_F3.8 (Aplicación de la F3.8)
test_images$curva1 <- lapply(test_images$imagen_curva_limites, F3.8_curva)

plot(test_images$curva1[[1]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[1]], col = "black")
plot(test_images$curva1[[2]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[2]], col = "black")
plot(test_images$curva1[[3]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[3]], col = "black")
plot(test_images$curva1[[4]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[4]], col = "black")
plot(test_images$curva1[[5]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[5]], col = "black")
plot(test_images$curva1[[6]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[6]], col = "black")
plot(test_images$curva1[[7]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[7]], col = "black")
plot(test_images$curva1[[8]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[8]], col = "black")
plot(test_images$curva1[[9]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[9]], col = "black")
plot(test_images$curva1[[10]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[10]], col = "black")
plot(test_images$curva1[[11]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[11]], col = "black")
plot(test_images$curva1[[12]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Curva de puntos");lines(test_images$curva1[[12]], col = "black")


test_images$curva1[[1]]
test_images$curva1[[2]]
test_images$curva1[[3]]
test_images$curva1[[4]]
test_images$curva1[[5]]
test_images$curva1[[6]]
test_images$curva1[[7]]
test_images$curva1[[8]]
test_images$curva1[[9]]
test_images$curva1[[10]]
test_images$curva1[[11]]
test_images$curva1[[12]]

#Problema 9: CURVA con NAs
#F3.9 (Funcion 3.9)  - Rellenar NAs
F3.9_rellenar_na_interpolado <- function(vector) {
  # Identificar índices válidos (no NA) y sus valores correspondientes
  indices_validos <- which(!is.na(vector))
  valores_validos <- vector[indices_validos]
  
  # Interpolar en todo el rango del vector
  vector_rellenado <- approx(
    x = indices_validos,       # Índices de valores válidos
    y = valores_validos,       # Valores válidos
    xout = seq_along(vector),  # Índices completos del vector
    method = "linear",         # Método de interpolación lineal
    rule = 2                   # Extrapolación con valores de los extremos
  )$y
  
  return(vector_rellenado)
}

#A_F3.9 (Aplicación de la F3.9)
test_images$curva2 <- lapply(test_images$curva1, F3.9_rellenar_na_interpolado)

test_images$curva2[[1]]
test_images$curva2[[2]]
test_images$curva2[[3]]
test_images$curva2[[4]]
test_images$curva2[[5]]
test_images$curva2[[6]]
test_images$curva2[[7]]
test_images$curva2[[8]]
test_images$curva2[[9]]
test_images$curva2[[10]]
test_images$curva2[[11]]
test_images$curva2[[12]]


plot(test_images$curva2[[1]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[1]], col = "black")
plot(test_images$curva2[[2]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[2]], col = "black")
plot(test_images$curva2[[3]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[3]], col = "black")
plot(test_images$curva2[[4]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[4]], col = "black")
plot(test_images$curva2[[5]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[5]], col = "black")
plot(test_images$curva2[[6]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[6]], col = "black")
plot(test_images$curva2[[7]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[7]], col = "black")
plot(test_images$curva2[[8]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[8]], col = "black")
plot(test_images$curva2[[9]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[9]], col = "black")
plot(test_images$curva2[[10]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[10]], col = "black")
plot(test_images$curva2[[11]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[11]], col = "black")
plot(test_images$curva2[[12]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva2[[12]], col = "black")



#Problema 10: Falta de estandarziacion eje X
#F3.10 (Funcion 3.10)  - Estandarización a 300 puntos
F3.10_estandarizar_curva_X <- function(curva, num_puntos = 300) {
  # Generar los puntos originales en la escala actual (1 a longitud de la curva)
  x_original <- seq_along(curva)
  
  # Generar los nuevos puntos en una escala uniforme de 1 a longitud de la curva
  x_nuevo <- seq(1, length(curva), length.out = num_puntos)
  
  # Realizar la interpolación
  curva_estandarizada <- approx(x = x_original, y = curva, xout = x_nuevo, method = "linear")$y
  
  return(curva_estandarizada)
}

#A_F3.10 (Aplicación de la F3.10)
test_images$curva3 <- lapply(test_images$curva2, F3.10_estandarizar_curva_X, num_puntos = 300)

plot(test_images$curva3[[1]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[1]], col = "black")
plot(test_images$curva3[[2]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[2]], col = "black")
plot(test_images$curva3[[3]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[3]], col = "black")
plot(test_images$curva3[[4]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[4]], col = "black")
plot(test_images$curva3[[5]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[5]], col = "black")
plot(test_images$curva3[[6]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[6]], col = "black")
plot(test_images$curva3[[7]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[7]], col = "black")
plot(test_images$curva3[[8]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[8]], col = "black")
plot(test_images$curva3[[9]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[9]], col = "black")
plot(test_images$curva3[[10]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[10]], col = "black")
plot(test_images$curva3[[11]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[11]], col = "black")
plot(test_images$curva3[[12]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[12]], col = "black")


#Problema 11: Falta de estandarziacion eje Y
#F3.11 (Funcion 3.11)  - Estandarización al 100%
F3.11_estandarizar_curva_Y <- function(curva) {
  # Encontrar el punto más alto de la curva (valor máximo)
  max_val <- max(curva)
  # Estandarizar la curva dividiendo cada valor por el máximo y multiplicando por 100
  curva_estandarizada <- (curva / max_val) * 100
  
  return(curva_estandarizada)
}


#A_F3.11 (Aplicación de la F3.11)
test_images$curva4 <- lapply(test_images$curva3, F3.11_estandarizar_curva_Y)


# VERIFICACION 11 - Graficar las curvas en la grilla
par(mfrow = c(6, 2), mar = c(3, 3, 2, 1))
# Graficar las curvas en la grilla
for (i in 1:12) {
  plot(
    test_images$curva4[[i]],
    col = "blue",
    pch = 16,
    cex = 0.3,
    xlab = "Índice (X)",
    ylab = "Fila (Y)",
    main = paste("Curva", i)
  )
  lines(test_images$curva4[[i]], col = "black")
}
# Restaurar la configuración gráfica por defecto
par(mfrow = c(1, 1))

#Problema 12 - Limites desajustados
#F3.12 (Función 3.12) - Ajustar limites
F3.12_ajustar_limites <- function(limites, longitud_original, longitud_nueva = 300) {
  # Ajustar cada valor de límite a la nueva escala proporcional
  limites_ajustados <- round((limites / longitud_original) * longitud_nueva)
  
  # Verificar si el último límite está arriba de 290, y si es así, ajustarlo a 300
  if (length(limites_ajustados) > 0) {
    if (limites_ajustados[length(limites_ajustados)] >= 290) {
      limites_ajustados[length(limites_ajustados)] <- 300
    }
  }
  
  return(limites_ajustados)
}

#A_F3.12 (Aplicación de la F3.12)
test_images$limites_ajustados <- mapply(
  F3.12_ajustar_limites,
  limites = test_images$limites,
  longitud_original = lengths(test_images$curva2),
  MoreArgs = list(longitud_nueva = 300),
  SIMPLIFY = FALSE
)

test_images$limites_ajustados


plot(test_images$curva3[[1]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[1]], col = "black");abline(v = test_images$limites_ajustados[[1]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[2]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[2]], col = "black");abline(v = test_images$limites_ajustados[[2]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[3]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[3]], col = "black");abline(v = test_images$limites_ajustados[[3]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[4]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[4]], col = "black");abline(v = test_images$limites_ajustados[[4]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[5]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[5]], col = "black");abline(v = test_images$limites_ajustados[[5]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[6]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[6]], col = "black");abline(v = test_images$limites_ajustados[[6]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[7]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[7]], col = "black");abline(v = test_images$limites_ajustados[[7]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[8]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[8]], col = "black");abline(v = test_images$limites_ajustados[[8]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[9]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[9]], col = "black");abline(v = test_images$limites_ajustados[[9]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[10]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[10]], col = "black");abline(v = test_images$limites_ajustados[[10]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[11]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[11]], col = "black");abline(v = test_images$limites_ajustados[[11]], col = "red", lwd = 2, lty = 2)
plot(test_images$curva3[[12]], col = "blue", pch = 16, cex= 0.3, xlab = "Índice (X)", ylab = "Fila (Y)", main = "Posiciones de las filas con valor 0");lines(test_images$curva3[[12]], col = "black");abline(v = test_images$limites_ajustados[[12]], col = "red", lwd = 2, lty = 2)


# VERIFICACION 12 - Graficar las curvas en la grilla
par(mfrow = c(6, 2), mar = c(3, 3, 2, 1))
# Graficar las curvas en la grilla
for (i in 1:12) {
  plot(
    test_images$curva4[[i]],
    col = "blue",
    pch = 16,
    cex = 0.3,
    xlab = "Índice (X)",
    ylab = "Fila (Y)",
    main = paste("Curva", i)
  )
  lines(test_images$curva4[[i]], col = "black")
  abline(v = test_images$limites_ajustados[[i]], col = "red", lwd = 2, lty = 2)
}
# Restaurar la configuración gráfica por defecto
par(mfrow = c(1, 1))



# Crear un contenedor con las funciones
fx_img_a_curva <- list(
  F1 = F3.1_recortar_bloques_negros,
  F2 = F3.2_recortar_bloques_blancos,
  F3 = F3.3_procesar_canal_rojo,
  F4 = F3.4_eliminar_linea_arriba,
  F5 = F3.5_eliminar_linea_derecha,
  F6A = F3.6A_lineas_horiz_ejeY,
  F6B = F3.6B_ejeY,
  F6C = F3.6C_agregar_ejes,
  F7A = F3.7A_deteccion_limites,
  F7B = F3.7B_limites,
  F7C = F3.7C_ultimo_limite,
  F8 = F3.8_curva,
  F9 = F3.9_rellenar_na_interpolado,
  F10 = F3.10_estandarizar_curva_X,
  F11 = F3.11_estandarizar_curva_Y,
  F12 = F3.12_ajustar_limites
)

# Guardar la lista de funciones en un archivo RData
save(fx_img_a_curva, file = "fx_img_a_curva.RData")
