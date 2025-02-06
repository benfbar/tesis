#Tesis de Maestría en Cs. de Datos
#BARAKIAN, Benjamin Federico
#Script 2 = Preprocesamiento de datos - Carpetas de imagenes - Carga del dataset de imagenes en R

setwd("C:/Users/autoinmunidad/Desktop/BARAKIAN_Tesis_Maestria/Dataset/")

# Librerías necesarias:
library(dplyr)
library(png)
library(tidyr)
library(ggplot2)

#F2.1 (Función 2.1) -Obtener las imágenes y crear el data frame
F2.1_get_image_data <- function(folder_path) {
  # Verificar la existencia de la carpeta
  if (!dir.exists(folder_path)) {
    cat("Carpeta no encontrada:", folder_path, "\n")
    return(NULL)  # Si la carpeta no existe, devolver NULL
  }
    # Obtener todos los archivos PNG en la carpeta
  files <- list.files(path = folder_path, pattern = "a\\.png$", full.names = TRUE)
    # Verificar si se encontraron archivos
  if (length(files) == 0) {
    cat("No se encontraron imágenes PNG en:", folder_path, "\n")
    return(NULL)  # Si no hay archivos, devolver NULL
  }
    # Mostrar los archivos encontrados (depuración)
  cat("Archivos encontrados en", folder_path, ":", files, "\n")
    # Crear el data frame con la información de las imágenes (sin incluir las imágenes en sí)
  image_data <- data.frame(
    carpeta = rep(basename(folder_path), length(files)),
    ID_interno = 1:length(files),
    nombre_archivo = basename(files),
    ruta_imagen = files  # Guardamos la ruta de la imagen, no la imagen en sí
  )
    return(image_data)
}

# Lista de las carpetas
folders <- c("1_CSSP", "2_RFA", "3_HIPOPTG", "4_SN", "5_HIPERG", "6_GM", "7_HIPOA", "8_HIPOG")
# Crear un data frame vacío para acumular los datos
all_images <- data.frame()
# Recorrer las carpetas y obtener los datos
for (folder in folders) {
  folder_path <- file.path(getwd(), folder) 
  image_data <- F2.1_get_image_data(folder_path)
    if (!is.null(image_data)) {
    all_images <- bind_rows(all_images, image_data)
  }
}

#Contar cantidad de imágenes por carpeta
summary(all_images)

# Filtrar el data frame para excluir las carpetas 7_HIPERG y 8_GM
all_images <- all_images %>%
  filter(!carpeta %in% c("7_HIPOA", "8_HIPOG"))
all_images$carpeta = as.factor(all_images$carpeta)
str(all_images)

# Filtrar registro con imagen erronea del grupo 3_HIPOPTG
all_images <- all_images[all_images$nombre_archivo != "190183673_190624_ca.png", ]

#contar
summary(all_images)

#F2.2 (Función 2.2) - Obtener las dimensiones de la imagen
F2.2_get_image_dimensions <- function(image_path) {
  img <- readPNG(image_path)  # Leer la imagen
  return(dim(img)[1:2])  # Retornar solo las dos primeras dimensiones (alto y ancho)
}

# Calcular las dimensiones y crear columnas
all_images <- all_images %>%
  mutate(
    dimensiones = purrr::map(ruta_imagen, F2.2_get_image_dimensions),  # Obtener dimensiones como lista
    alto = purrr::map_dbl(dimensiones, 1),  # Extraer el alto
    ancho = purrr::map_dbl(dimensiones, 2),  # Extraer el ancho
    pixeles_totales = alto * ancho  # Calcular los píxeles totales
  )

#Eliminar "dimensiones"
all_images <- all_images[, !(names(all_images) %in% c("dimensiones"))]
gc()

#Promedio total de píxeles
promedio_total_pixeles <- mean(all_images$pixeles_totales)
promedio_total_pixeles
# Promedio de píxeles por carpeta
promedio_por_carpeta <- all_images %>%
  group_by(carpeta) %>%
  summarise(promedio_pixeles = mean(pixeles_totales))
promedio_por_carpeta

# Graficar el histograma de píxeles totales por carpeta
ggplot(all_images, aes(x = pixeles_totales, fill = carpeta)) +
  geom_histogram(binwidth = 100000, position = "dodge") +
  labs(
    title = "Histograma de Píxeles Totales por Carpeta",
    x = "Píxeles Totales",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Graficar un histograma de píxeles totales por carpeta usando facetas
ggplot(all_images, aes(x = pixeles_totales, fill = carpeta)) +
  geom_histogram(binwidth = 50000, alpha = 0.7) +
  facet_wrap(~ carpeta) +  # Crear un gráfico por cada carpeta
  labs(
    title = "Histograma de Píxeles Totales por Carpeta",
    x = "Píxeles Totales",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Agregar la columna 'calidad' basada en la cantidad de píxeles totales
all_images <- all_images %>%
  mutate(
    calidad = ifelse(pixeles_totales < 250000, "baja", "alta")
  )
all_images$calidad = as.factor(all_images$calidad)


#2.3 (Función 2.3) - Función para obtener la matriz de píxeles
F2.3_get_image_matrix <- function(image_path) {
  img <- readPNG(image_path)  # Leer la imagen
  return(img)  # Retornar la matriz de píxeles
}

#Probar con primer imagen
prueba_matriz_imagen = F2.3_get_image_matrix(all_images$ruta_imagen[[1]])

#ver primera
prueba_matriz_imagen
plot(as.raster(prueba_matriz_imagen)) #plotea bien
dim(prueba_matriz_imagen) #tiene 4 capas, es RGBA

# Extraer los canales para ver si tienen información relevante
rojo <- prueba_matriz_imagen[,,1]
verde <- prueba_matriz_imagen[,,2]
azul <- prueba_matriz_imagen[,,3]
alfa <- prueba_matriz_imagen[,,4]

plot(as.raster(rojo))  #Si, en negro solo ejes, curva y limites
plot(as.raster(verde)) #Si, en negro ejes, curva y limties + relleno en gris
plot(as.raster(azul))  #Si, en negro ejes, curva y limties + relleno en gris
plot(as.raster(alfa))  #No aporta info, no hace falta cargar la 4ta dimension de todo

# F2.4 (Función 2.4) - Obtener la matriz de píxeles con solo los canales RGB
F2.4_get_image_matrix_RGB <- function(image_path) {
  img <- readPNG(image_path)  # Leer la imagen
  img <- img[,,1:3]  # Seleccionar solo los canales RGB
  return(img)  # Retornar la matriz de píxeles
}
# Agregar la columna de matrices de imágenes
all_images <- all_images %>%
  mutate(imagen_matrix_RGB = purrr::map(ruta_imagen, F2.4_get_image_matrix_RGB))

str(all_images)

#eliminar "ruta_imagen" y "imagen_matrix"
all_images <- all_images[, !(names(all_images) %in% c("ruta_imagen"))]
gc()

# Chequear si existen archivos duplicados  con el mismo nombre_archivo en distintas carpetas
duplicados <- all_images$nombre_archivo[duplicated(all_images$nombre_archivo)]

if (length(duplicados) > 0) {
  cat("Archivos duplicados encontrados:\n")
  print(duplicados)
} else {
  cat("No se encontraron archivos duplicados.\n")
}

duplicados <- all_images$nombre_archivo[duplicated(all_images$nombre_archivo)]
filas_duplicadas <- all_images[all_images$nombre_archivo %in% duplicados, ]
resultados_duplicados <- data.frame(
  fila = rownames(filas_duplicadas),
  nombre_archivo = filas_duplicadas$nombre_archivo
)
cat("Archivos duplicados y sus IDs de fila:\n")
print(resultados_duplicados)

plot(as.raster(all_images$imagen_matrix_RGB[[330]]))
plot(as.raster(all_images$imagen_matrix_RGB[[481]])) #se llaman igual pero son distintas curvas, no se descarta ninguna de las dos
plot(as.raster(all_images$imagen_matrix_RGB[[579]]))
plot(as.raster(all_images$imagen_matrix_RGB[[757]])) #se llaman igual pero son distintas curvas, no se descarta ninguna de las dos

# Guardar el objeto all_images en un archivo RData
save(all_images, file = "all_images.RData")

# crear y Guardar el conjunto de prueba (imagen más pequeña y grande de cada carpeta) en un archivo RData

test_images_t<- all_images %>%
  group_by(carpeta) %>%
  summarise(
    smallest_image = nombre_archivo [which.min(pixeles_totales)],
    largest_image = nombre_archivo [which.max(pixeles_totales)]
  ) %>%
  pivot_longer(cols = c(smallest_image, largest_image), 
               names_to = "extreme_type", 
               values_to = "nombre_archivo")

summary(test_images_t)
test_images_t$extreme_type =as.factor(test_images_t$extreme_type)
test_images_t$carpeta
test_images_t$extreme_type

test_images <- all_images %>%
  semi_join(test_images_t, by = c("nombre_archivo", "carpeta"))
test_images <- test_images %>%
  arrange(carpeta, pixeles_totales)
test_images$carpeta
test_images$pixeles_totales

# Guardar el dataset resultante como un archivo RData
save(test_images, file = "test_images.RData")

