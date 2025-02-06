#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 3B = Tratamiento de las imagenes - Pruebas con subconjunto de imágenes "extremas" - otras pruebas que no se utilizaron

# Función para convertir una matriz de imagen a escala de grises
convert_to_gray <- function(imagen_bordes_final) {
  # Fórmula ponderada para escala de grises
  gray_matrix <- 0.2126 * imagen_bordes_final[,,1] + 
    0.7152 * imagen_bordes_final[,,2] + 
    0.0722 * imagen_bordes_final[,,3]
  return(gray_matrix)
}

# Agregar una nueva columna con las matrices de imágenes en escala de grises
test_images <- test_images %>%
  mutate(imagen_gris = purrr::map(imagen_bordes_final, convert_to_gray))


#verificar
plot(as.raster(test_images$imagen_gris[[1]])) 
plot(as.raster(test_images$imagen_gris[[2]]))
plot(as.raster(test_images$imagen_gris[[3]])) 
plot(as.raster(test_images$imagen_gris[[4]]))
plot(as.raster(test_images$imagen_gris[[5]]))
plot(as.raster(test_images$imagen_gris[[6]]))
plot(as.raster(test_images$imagen_gris[[7]]))
plot(as.raster(test_images$imagen_gris[[8]]))
plot(as.raster(test_images$imagen_gris[[9]]))
plot(as.raster(test_images$imagen_gris[[10]]))
plot(as.raster(test_images$imagen_gris[[11]]))
plot(as.raster(test_images$imagen_gris[[12]]))
plot(as.raster(test_images$imagen_gris[[13]]))


#histogramas de color:

library(ggplot2)

# Generar histogramas de las imágenes recortadas
for (i in seq_along(test_images$imagen_gris)) {
  # Extraer la matriz recortada
  matriz <- test_images$imagen_gris[[i]]
  
  # Convertir la matriz en un vector
  valores <- as.vector(matriz)
  
  # Crear un data.frame para ggplot
  df <- data.frame(Intensidad = valores)
  
  # Generar el histograma
  p <- ggplot(df, aes(x = Intensidad)) +
    geom_histogram(binwidth = 0.05, fill = "gray", color = "black") +
    labs(title = paste("Histograma de imagen recortada", i),
         x = "Intensidad de píxel (escala de grises)",
         y = "Frecuencia") +
    theme_minimal()
  
  # Mostrar el histograma
  print(p)
}

#limites

# Función para umbralizar una matriz
umbralizar_matriz_2 <- function(matriz) {
  matriz_umbralizada_2 <- matriz  # Copia de la matriz
  
  # Aplicar los rangos de umbralización
  matriz_umbralizada_2[matriz = 0] <- 0
  matriz_umbralizada_2[matriz > 0 & matriz <= 0.05] <- 0.8
  matriz_umbralizada_2[matriz > 0.05& matriz <= 1] <- 1
  
  return(matriz_umbralizada_2)
}

# Crear una nueva columna con las imágenes umbralizadas
test_images$imagen_gris_umbralizada_2 <- lapply(test_images$imagen_gris, umbralizar_matriz_2)

plot(as.raster(test_images$imagen_gris_umbralizada_2[[1]])) #solo ejes
plot(as.raster(test_images$imagen_gris_umbralizada_2[[2]])) #ejes + 6 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[3]])) #solo ejes
plot(as.raster(test_images$imagen_gris_umbralizada_2[[4]])) #ejes + 5 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[5]])) #solo ejes
plot(as.raster(test_images$imagen_gris_umbralizada_2[[6]])) #ejes + 5 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[7]])) #ejes + 6 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[8]])) #ejes + 5 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[9]])) #ejes + 1 limite
plot(as.raster(test_images$imagen_gris_umbralizada_2[[10]])) #ejes + 5 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[11]])) #ejes + 2 limite
plot(as.raster(test_images$imagen_gris_umbralizada_2[[12]])) #ejes + 6 limites
plot(as.raster(test_images$imagen_gris_umbralizada_2[[13]])) #ejes


str(test_images$imagen_gris_umbralizada_2)
# Crear una lista para almacenar los resultados
columnas_distintas_de_1 <- list()

# Recorrer cada matriz en la lista
for (i in seq_along(test_images$imagen_gris_umbralizada_2)) {
  # Seleccionar la matriz actual
  matriz <- test_images$imagen_gris_umbralizada_2[[i]]
  
  # Asegurarse de que la matriz tenga al menos 6 filas
  if (nrow(matriz) >= 6) {
    # Seleccionar las últimas 5 filas (sin incluir la última)
    fila_menos1 <- matriz[nrow(matriz) - 1, ]
    fila_menos2 <- matriz[nrow(matriz) - 2, ]
    fila_menos3 <- matriz[nrow(matriz) - 3, ]
    fila_menos4 <- matriz[nrow(matriz) - 4, ]
    fila_menos5 <- matriz[nrow(matriz) - 5, ]
    
    # Identificar las columnas con valores distintos de 1
    columnas1 <- which(fila_menos1 != 1)
    columnas2 <- which(fila_menos2 != 1)
    columnas3 <- which(fila_menos3 != 1)
    columnas4 <- which(fila_menos4 != 1)
    columnas5 <- which(fila_menos5 != 1)
    
    # Unir, eliminar duplicados y ordenar
    columnas_unicas <- sort(unique(c(columnas1, columnas2, columnas3, columnas4, columnas5)))
    
    # Guardar el resultado
    columnas_distintas_de_1[[i]] <- columnas_unicas
  } else {
    # Si la matriz tiene menos de 6 filas, guardar NA o NULL
    columnas_distintas_de_1[[i]] <- NA
  }
}

# Agregar como una nueva columna al dataset
test_images$columnas_distintas_de_1 <- columnas_distintas_de_1


str(test_images$imagen_gris_umbralizada)


# Función para graficar la diferencia entre los canales Rojo (R) y Verde (G) con colores invertidos
plot_red_green_inverted <- function(image, main_title = "Red and Green Channels (Inverted Colors)") {
  # Separar los canales Rojo y Verde
  channel_r <- image[,,1]
  channel_g <- image[,,2]
  
  # Calcular la diferencia entre los canales Rojo y Verde
  diff_rg <- channel_r - channel_g
  
  # Normalizar la diferencia para que esté en el rango [0, 1]
  diff_rg_normalized <- (diff_rg - min(diff_rg)) / (max(diff_rg) - min(diff_rg))
  
  # Invertir los colores, es decir, hacer que los valores más altos sean oscuros y los más bajos brillantes
  diff_rg_inverted <- 1 - diff_rg_normalized
  
  # Graficar la diferencia invertida
  plot(as.raster(diff_rg_inverted), main = main_title)
}

# Usar la función para graficar el primer elemento de imagen_bordes_final con colores invertidos
plot_red_green_inverted(test_images$imagen_bordes_final[[1]])
plot_red_green_inverted(test_images$imagen_bordes_final[[2]])
plot_red_green_inverted(test_images$imagen_bordes_final[[3]])
plot_red_green_inverted(test_images$imagen_bordes_final[[4]])
plot_red_green_inverted(test_images$imagen_bordes_final[[5]])
plot_red_green_inverted(test_images$imagen_bordes_final[[1]])
plot_red_green_inverted(test_images$imagen_bordes_final[[1]])
plot_red_green_inverted(test_images$imagen_bordes_final[[1]])


# Función para graficar la diferencia entre los canales Rojo (R) y Verde (G) con umbralización invertida
plot_red_green_thresholded_inverted <- function(image, main_title = "Red and Green Channels (Thresholded Inverted)") {
  # Separar los canales Rojo y Verde
  channel_r <- image[,,1]
  channel_g <- image[,,2]
  
  # Calcular la diferencia entre los canales Rojo y Verde
  diff_rg <- channel_r - channel_g
  
  # Calcular la media de la diferencia
  mean_diff <- mean(diff_rg)
  
  # Umbralizar invertido: asignar 1 para valores menores que la media y 0 para valores mayores o iguales
  diff_rg_thresholded_inverted <- ifelse(diff_rg < mean_diff, 1, 0)
  
  # Graficar la imagen umbralizada invertida
  plot(as.raster(diff_rg_thresholded_inverted), main = main_title)
}

# Usar la función para graficar el primer elemento de imagen_bordes_final con umbralización invertida
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[1]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[2]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[3]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[4]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[5]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[6]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[7]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[8]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[9]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[10]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[11]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[12]])
plot_red_green_thresholded_inverted(test_images$imagen_bordes_final[[13]])



# Usar curva1 como canal Red (R) y curva2 como canal Blue (B)
# Mantener las dimensiones originales de la imagen (158 x 254)

canal_R <- matrix(test_images$curvaR[[1]], nrow = 158, ncol = 254)  # Canal Red (curva1)
canal_G <- matrix(0, nrow = 158, ncol = 254)  # Canal Green vacío
canal_B <- matrix(test_images$curva1[[1]], nrow = 158, ncol = 254)  # Canal Blue (curva2)

# Crear la imagen RGB combinando los tres canales
imagen_rgb <- array(0, dim = c(158, 254, 3))  # Imagen de 158x254 con 3 canales
imagen_rgb[,,1] <- canal_R  # Canal Red
imagen_rgb[,,2] <- canal_G  # Canal Green
imagen_rgb[,,3] <- canal_B  # Canal Blue

# Visualizar la imagen como un raster
plot(as.raster(imagen_rgb), main = "Imagen RGB: Curva1 como Red y Curva2 como Blue")


