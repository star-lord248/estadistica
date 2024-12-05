# Cargar librerías
library(modeest)  # Para la moda

# Datos
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 
            19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 
            21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 20, 18, 23, 
            19, 22, 19, 22, 21, 21, 19, 19, 19, 19, 19, 19, 22, 21, 20, 21, 19, 19, 22, 19, 19, 19, 19, 19, 19, 19, 21, 
            22, 20, 20, 20)

alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 
             167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 
             180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 
             185, 181, 178, 182, 180, 179, 170, 195, 171, 183, 153, 169, 168, 172, 170, 172, 172, 165, 175, 173, 170, 182, 
             165, 173, 175, 180, 179, 181, 176, 180, 175, 178, 183, 165, 172, 175, 178, 180, 164, 169, 178, 184, 180, 183, 184, 188)

pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52,
           58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65,
           75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 56, 53, 65, 67, 56, 67, 55, 70,
           60, 62, 60, 66, 70, 68, 75, 67, 72, 60, 83, 61, 60, 64, 56, 88, 80, 70, 54, 55, 82, 82, 64, 70, 58, 78, 69, 80, 
           70, 90, 80)

# Función para calcular las medidas de centralización
calcular_medidas <- function(datos) {
  media <- mean(datos)              # Media
  mediana <- median(datos)          # Mediana
  moda <- mfv(datos)                # Moda
  return(list(media = media, mediana = mediana, moda = moda))
}

# Cálculo de las medidas de centralización para edades, alturas y pesos
medidas_edades <- calcular_medidas(edades)
medidas_alturas <- calcular_medidas(alturas)
medidas_pesos <- calcular_medidas(pesos)

# Mostrar los resultados
cat("Medidas de centralización para Edades:\n")
print(medidas_edades)

cat("\nMedidas de centralización para Alturas:\n")
print(medidas_alturas)

cat("\nMedidas de centralización para Pesos:\n")
print(medidas_pesos)

# Agrupar por intervalos (opcional)
# Histograma con intervalos definidos
hist(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), main = "Histograma de Edades por Intervalos",xlab = "Edades")
hist(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), main = "Histograma de Alturas por Intervalos", xlab = "Alturas")
hist(pesos,  breaks = seq(min(pesos), max(pesos) + 10, by = 10), main = "Histograma de Pesos por Intervalos", xlab = "Pesos")


# Datos
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 
            19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 
            21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 20, 18, 23, 
            19, 22, 19, 22, 21, 21, 19, 19, 19, 19, 19, 19, 22, 21, 20, 21, 19, 19, 22, 19, 19, 19, 19, 19, 19, 19, 21, 
            22, 20, 20, 20)

alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 
             167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 
             180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 
             185, 181, 178, 182, 180, 179, 170, 195, 171, 183, 153, 169, 168, 172, 170, 172, 172, 165, 175, 173, 170, 182, 
             165, 173, 175, 180, 179, 181, 176, 180, 175, 178, 183, 165, 172, 175, 178, 180, 164, 169, 178, 184, 180, 183, 184, 188)

pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52,
           58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65,
           75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 56, 53, 65, 67, 56, 67, 55, 70,
           60, 62, 60, 66, 70, 68, 75, 67, 72, 60, 83, 61, 60, 64, 56, 88, 80, 70, 54, 55, 82, 82, 64, 70, 58, 78, 69, 80, 
           70, 90, 80)

# Función para calcular cuartiles, deciles y percentiles
calcular_posiciones <- function(datos) {
  cuartiles <- quantile(datos, probs = c(0.25, 0.50, 0.75))  # Cuartiles
  deciles <- quantile(datos, probs = c(0.1, 0.3, 0.6, 0.9))  # Deciles
  percentiles <- quantile(datos, probs = c(0.10, 0.25, 0.70, 0.90))  # Percentiles
  return(list(cuartiles = cuartiles, deciles = deciles, percentiles = percentiles))
}

# Cálculo de las medidas de posición para edades, alturas y pesos
posiciones_edades <- calcular_posiciones(edades)
posiciones_alturas <- calcular_posiciones(alturas)
posiciones_pesos <- calcular_posiciones(pesos)

# Mostrar los resultados
cat("Medidas de posición para Edades:\n")
print(posiciones_edades)

cat("\nMedidas de posición para Alturas:\n")
print(posiciones_alturas)

cat("\nMedidas de posición para Pesos:\n")
print(posiciones_pesos)

# Agrupar en intervalos (opcional)
# Histograma con intervalos definidos
hist(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), main = "Histograma de Edades por Intervalos",xlab = "Edades")
hist(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), main = "Histograma de Alturas por Intervalos", xlab = "Alturas")
hist(pesos,  breaks = seq(min(pesos), max(pesos) + 10, by = 10), main = "Histograma de Pesos por Intervalos", xlab = "Pesos")

# Datos
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 
            19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 
            21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 20, 18, 23, 
            19, 22, 19, 22, 21, 21, 19, 19, 19, 19, 19, 19, 22, 21, 20, 21, 19, 19, 22, 19, 19, 19, 19, 19, 19, 19, 21, 
            22, 20, 20, 20)

alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 
             167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 
             180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 
             185, 181, 178, 182, 180, 179, 170, 195, 171, 183, 153, 169, 168, 172, 170, 172, 172, 165, 175, 173, 170, 182, 
             165, 173, 175, 180, 179, 181, 176, 180, 175, 178, 183, 165, 172, 175, 178, 180, 164, 169, 178, 184, 180, 183, 184, 188)

pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52,
           58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65,
           75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 56, 53, 65, 67, 56, 67, 55, 70,
           60, 62, 60, 66, 70, 68, 75, 67, 72, 60, 83, 61, 60, 64, 56, 88, 80, 70, 54, 55, 82, 82, 64, 70, 58, 78, 69, 80, 
           70, 90, 80)

# Función para calcular cuartiles, deciles y percentiles
calcular_posiciones <- function(datos) {
  cuartiles <- quantile(datos, probs = c(0.25, 0.50, 0.75))  # Cuartiles
  deciles <- quantile(datos, probs = c(0.1, 0.3, 0.6, 0.9))  # Deciles
  percentiles <- quantile(datos, probs = c(0.10, 0.25, 0.70, 0.90))  # Percentiles
  return(list(cuartiles = cuartiles, deciles = deciles, percentiles = percentiles))
}

# Cálculo de las medidas de posición para edades, alturas y pesos
posiciones_edades <- calcular_posiciones(edades)
posiciones_alturas <- calcular_posiciones(alturas)
posiciones_pesos <- calcular_posiciones(pesos)

# Mostrar los resultados
cat("Medidas de posición para Edades:\n")
print(posiciones_edades)

cat("\nMedidas de posición para Alturas:\n")
print(posiciones_alturas)

cat("\nMedidas de posición para Pesos:\n")
print(posiciones_pesos)

# Agrupar en intervalos (opcional)
# Histograma con intervalos definidos
hist(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), main = "Histograma de Edades por Intervalos",xlab = "Edades")
hist(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), main = "Histograma de Alturas por Intervalos", xlab = "Alturas")
hist(pesos,  breaks = seq(min(pesos), max(pesos) + 10, by = 10), main = "Histograma de Pesos por Intervalos", xlab = "Pesos")

# Ingresar los datos (como en la pregunta anterior)
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 19, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 19, 22, 19, 21, 20, 21, 19, 19, 19, 19, 19, 19, 22, 19, 19, 19, 22, 20, 22, 19, 20, 19, 19)
alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 185, 181, 178, 182, 180, 183, 171, 183, 153, 169, 168, 172, 170, 170, 177, 177, 165, 172, 165, 173, 175, 179, 181, 176, 180, 178, 183, 165, 172, 182, 165, 172, 178, 184, 174)
pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52, 58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65, 75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 53, 65, 67, 56, 74, 65, 70, 55, 70, 62, 60, 66, 60, 70, 80, 75, 85, 90, 80)

# Cargar la librería moments para calcular sesgo y curtosis
library(moments)

# Sesgo (asimetría)
sesgo_edades <- skewness(edades)
sesgo_alturas <- skewness(alturas)
sesgo_pesos <- skewness(pesos)

# Curtosis (apuntamiento)
curtosis_edades <- kurtosis(edades)
curtosis_alturas <- kurtosis(alturas)
curtosis_pesos <- kurtosis(pesos)

# Resultados
list(sesgo_edades=sesgo_edades, curtosis_edades=curtosis_edades,
     sesgo_alturas=sesgo_alturas, curtosis_alturas=curtosis_alturas,
     sesgo_pesos=sesgo_pesos, curtosis_pesos=curtosis_pesos)



# Ingresar los datos
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 19, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 19, 22, 19, 21, 20, 21, 19, 19, 19, 19, 19, 19, 22, 19, 19, 19, 22, 20, 22, 19, 20, 19, 19)
alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 185, 181, 178, 182, 180, 183, 171, 183, 153, 169, 168, 172, 170, 170, 177, 177, 165, 172, 165, 173, 175, 179, 181, 176, 180, 178, 183, 165, 172, 182, 165, 172, 178, 184, 174)
pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52, 58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65, 75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 53, 65, 67, 56, 74, 65, 70, 55, 70, 62, 60, 66, 60, 70, 80, 75, 85, 90, 80)

# Crear histogramas con los intervalos dados
# Histograma de Edades
hist(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), main = "Histograma de Edades por Intervalos",xlab = "Edades", col = "lightblue", border = "black")

# Histograma de Alturas
hist(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), main = "Histograma de Alturas por Intervalos", xlab = "Alturas", col = "lightgreen", border = "black")

# Histograma de Pesos
hist(pesos,  breaks = seq(min(pesos), max(pesos) + 10, by = 10), main = "Histograma de Pesos por Intervalos", xlab = "Pesos", col = "lightcoral", border = "black")

# Crear histogramas con los intervalos dados
# Histograma de Edades
hist(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), main = "Histograma de Edades por Intervalos",xlab = "Edades", col = "lightblue", border = "black")

# Histograma de Alturas
hist(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), main = "Histograma de Alturas por Intervalos", xlab = "Alturas", col = "lightgreen", border = "black")

# Histograma de Pesos
hist(pesos,  breaks = seq(min(pesos), max(pesos) + 10, by = 10), main = "Histograma de Pesos por Intervalos", xlab = "Pesos", col = "lightcoral", border = "black")

# Función para cortar los datos en intervalos
# Para Edades
intervalos_edades <- cut(edades, breaks = seq(min(edades), max(edades) + 8, by = 8), include.lowest = TRUE)
frecuencia_edades <- table(intervalos_edades)
pie(frecuencia_edades, main = "Diagrama de Sectores - Edades", col = rainbow(length(frecuencia_edades)))

# Para Alturas
intervalos_alturas <- cut(alturas, breaks = seq(min(alturas), max(alturas) + 6, by = 6), include.lowest = TRUE)
frecuencia_alturas <- table(intervalos_alturas)
pie(frecuencia_alturas, main = "Diagrama de Sectores - Alturas", col = rainbow(length(frecuencia_alturas)))

# Para Pesos
intervalos_pesos <- cut(pesos, breaks = seq(min(pesos), max(pesos) + 10, by = 10), include.lowest = TRUE)
frecuencia_pesos <- table(intervalos_pesos)
pie(frecuencia_pesos, main = "Diagrama de Sectores - Pesos", col = rainbow(length(frecuencia_pesos)))


# Ingresar los datos
edades <- c(19, 21, 20, 21, 19, 20, 22, 19, 19, 19, 18, 20, 19, 20, 20, 21, 21, 20, 20, 18, 19, 24, 31, 36, 20, 23, 20, 26, 19, 20, 20, 21, 19, 21, 19, 19, 23, 21, 22, 20, 20, 21, 20, 22, 24, 20, 20, 21, 21, 22, 24, 22, 20, 24, 20, 21, 27, 18, 21, 20, 19, 28, 20, 19, 21, 19, 24, 19, 27, 19, 37, 25, 23, 22, 20, 19, 21, 19, 21, 19, 19, 22, 19, 21, 20, 21, 19, 19, 19, 19, 19, 19, 22, 19, 19, 19, 22, 20, 22, 19, 20, 19, 19)
alturas <- c(174, 187, 162, 157, 169, 169, 174, 173, 174, 180, 184, 177, 184, 187, 173, 182, 185, 164, 156, 170, 165, 180, 167, 180, 170, 188, 180, 169, 176, 174, 183, 170, 165, 184, 170, 170, 172, 179, 187, 178, 168, 160, 180, 171, 180, 175, 172, 178, 170, 182, 170, 154, 160, 178, 159, 170, 162, 185, 175, 181, 178, 176, 178, 181, 170, 177, 185, 181, 178, 182, 180, 183, 171, 183, 153, 169, 168, 172, 170, 170, 177, 177, 165, 172, 165, 173, 175, 179, 181, 176, 180, 178, 183, 165, 172, 182, 165, 172, 178, 184, 174)
pesos <- c(67, 88, 68, 46, 52, 65, 70, 59, 67, 74, 72, 65, 66, 81, 65, 69, 91, 65, 58, 55, 52, 60, 56, 80, 58, 120, 70, 52, 58, 60, 77, 68, 58, 70, 60, 71, 70, 64, 80, 85, 62, 54, 80, 58, 70, 66, 80, 65, 60, 83, 110, 52, 60, 74, 55, 65, 75, 80, 68, 65, 76, 71, 57, 100, 58, 64, 57, 75, 70, 90, 96, 74, 55, 85, 55, 71, 53, 65, 67, 56, 74, 65, 70, 55, 70, 62, 60, 66, 60, 70, 80, 75, 85, 90, 80)


# Diagrama de caja para Edades
boxplot(edades, main = "Diagrama de Caja - Edades", ylab = "Edades", col = "lightblue")

# Diagrama de caja para Alturas
boxplot(alturas, main = "Diagrama de Caja - Alturas", ylab = "Alturas (cm)", col = "lightgreen")

# Diagrama de caja para Pesos
boxplot(pesos, main = "Diagrama de Caja - Pesos", ylab = "Pesos (kg)", col = "lightcoral")

# Agrupar Edades en intervalos de 8
intervalos_edades <- cut(edades, breaks = seq(min(edades), max(edades), by = 8), include.lowest = TRUE)

# Agrupar Alturas en intervalos de 6
intervalos_alturas <- cut(alturas, breaks = seq(min(alturas), max(alturas), by = 6), include.lowest = TRUE)

# Agrupar Pesos en intervalos de 10
intervalos_pesos <- cut(pesos, breaks = seq(min(pesos), max(pesos), by = 10), include.lowest = TRUE)

# Crear los boxplots para los datos agrupados
# Edades agrupadas
boxplot(edades ~ intervalos_edades, main = "Diagrama de Caja - Edades Agrupadas", xlab = "Intervalos de Edades", ylab = "Edades", col = "lightblue")

# Alturas agrupadas
boxplot(alturas ~ intervalos_alturas, main = "Diagrama de Caja - Alturas Agrupadas", xlab = "Intervalos de Alturas", ylab = "Alturas (cm)", col = "lightgreen")

# Pesos agrupados
boxplot(pesos ~ intervalos_pesos, main = "Diagrama de Caja - Pesos Agrupados", xlab = "Intervalos de Pesos", ylab = "Pesos (kg)", col = "lightcoral")

