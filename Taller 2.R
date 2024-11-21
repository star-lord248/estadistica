# Datos de los 50 partidos de baloncesto
datos <- c(196, 171, 202, 178, 147, 102, 153, 197, 177, 192, 157, 185, 90, 116, 172, 
           191, 148, 200, 177, 165, 141, 149, 210, 175, 193, 198, 144, 168, 209, 167, 
           195, 163, 210, 175, 130, 143, 187, 166, 139, 149, 108, 199, 150, 154, 194, 
           135, 191, 177, 129, 158)

# Crear una tabla de frecuencias con 10 intervalos
breaks <- seq(min(datos), max(datos), length.out = 11)
frecuencias <- hist(datos, breaks = breaks, plot = FALSE)

# Frecuencia absoluta
frecuencia_absoluta <- frecuencias$counts

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(frecuencia_absoluta)

# Frecuencia relativa
frecuencia_relativa <- frecuencia_absoluta / length(datos)

# Crear un data.frame con las frecuencias
tabla_frecuencias <- data.frame(
  Intervalo = cut(datos, breaks = breaks, include.lowest = TRUE),
  Frecuencia_Absoluta = frecuencia_absoluta,
  Frecuencia_Acumulada = frecuencia_acumulada,
  Frecuencia_Relativa = round(frecuencia_relativa, 4)
)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)

# Histograma de los datos
hist(datos, breaks = breaks, col = "lightblue", border = "black",
     main = "Histograma de los datos de baloncesto", xlab = "Puntos", ylab = "Frecuencia")

# Diagrama de sectores de las frecuencias absolutas
pie(frecuencia_absoluta, labels = tabla_frecuencias$Intervalo, 
    main = "Diagrama de sectores de frecuencias absolutas", col = rainbow(10))



# Datos de los 50 partidos de baloncesto
datos <- c(196, 171, 202, 178, 147, 102, 153, 197, 177, 192, 157, 185, 90, 116, 172, 
           191, 148, 200, 177, 165, 141, 149, 210, 175, 193, 198, 144, 168, 209, 167, 
           195, 163, 210, 175, 130, 143, 187, 166, 139, 149, 108, 199, 150, 154, 194, 
           135, 191, 177, 129, 158)

# Crear una tabla de frecuencias con 10 intervalos
breaks <- seq(min(datos), max(datos), length.out = 11)
frecuencias <- hist(datos, breaks = breaks, plot = FALSE)

# Frecuencia absoluta
frecuencia_absoluta <- frecuencias$counts

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(frecuencia_absoluta)

# Frecuencia relativa
frecuencia_relativa <- frecuencia_absoluta / length(datos)

# Crear un data.frame con las frecuencias
tabla_frecuencias <- data.frame(
  Intervalo = cut(datos, breaks = breaks, include.lowest = TRUE),
  Frecuencia_Absoluta = frecuencia_absoluta,
  Frecuencia_Acumulada = frecuencia_acumulada,
  Frecuencia_Relativa = round(frecuencia_relativa, 4)
)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)

# Cálculo del promedio (media)
marca_clase <- (head(breaks, -1) + tail(breaks, -1)) / 2
promedio <- sum(marca_clase * frecuencia_absoluta) / length(datos)
cat("Promedio:", promedio, "\n")

# Cálculo de la moda (intervalo con mayor frecuencia)
moda_intervalo <- tabla_frecuencias$Intervalo[which.max(tabla_frecuencias$Frecuencia_Absoluta)]
cat("Moda (intervalo):", moda_intervalo, "\n")

# Cálculo de la mediana
n <- length(datos)
frecuencia_acumulada_mitad <- frecuencia_acumulada / n
mediana_intervalo <- tabla_frecuencias$Intervalo[which(frecuencia_acumulada >= n / 2)][1]
cat("Mediana (intervalo):", mediana_intervalo, "\n")

# Cálculo del rango
rango <- max(datos) - min(datos)
cat("Rango:", rango, "\n")

# Cálculo de la varianza y desviación estándar
varianza <- sum(frecuencia_absoluta * (marca_clase - promedio)^2) / (n - 1)
desviacion_estandar <- sqrt(varianza)
cat("Varianza:", varianza, "\n")
cat("Desviación estándar:", desviacion_estandar, "\n")

# Cálculo del rango intercuartílico
q1 <- quantile(datos, 0.25)
q3 <- quantile(datos, 0.75)
rango_intercuartilico <- q3 - q1
cat("Rango intercuartílico:", rango_intercuartilico, "\n")

# Histograma de los datos
hist(datos, breaks = breaks, col = "lightblue", border = "black",
     main = "Histograma de los datos de baloncesto", xlab = "Puntos", ylab = "Frecuencia")

# Diagrama de sectores de las frecuencias absolutas
pie(frecuencia_absoluta, labels = tabla_frecuencias$Intervalo, 
    main = "Diagrama de sectores de frecuencias absolutas", col = rainbow(10))



# Crear una tabla de frecuencias con 10 intervalos
breaks <- seq(min(datos), max(datos), length.out = 11)
frecuencias <- hist(datos, breaks = breaks, plot = FALSE)

# Frecuencia absoluta
frecuencia_absoluta <- frecuencias$counts

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(frecuencia_absoluta)

# Frecuencia relativa
frecuencia_relativa <- frecuencia_absoluta / length(datos)

# Crear un data.frame con las frecuencias
tabla_frecuencias <- data.frame(
  Intervalo = cut(datos, breaks = breaks, include.lowest = TRUE),
  Frecuencia_Absoluta = frecuencia_absoluta,
  Frecuencia_Acumulada = frecuencia_acumulada,
  Frecuencia_Relativa = round(frecuencia_relativa, 4)
)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)


# Datos de los 50 partidos de baloncesto
datos <- c(196, 171, 202, 178, 147, 102, 153, 197, 177, 192, 157, 185, 90, 116, 172, 
           191, 148, 200, 177, 165, 141, 149, 210, 175, 193, 198, 144, 168, 209, 167, 
           195, 163, 210, 175, 130, 143, 187, 166, 139, 149, 108, 199, 150, 154, 194, 
           135, 191, 177, 129, 158)

# Promedio
promedio <- mean(datos)
cat("Promedio:", promedio, "\n")

# Moda (número que más se repite)
tabla_frecuencias <- table(datos)
moda <- as.numeric(names(tabla_frecuencias)[which.max(tabla_frecuencias)])
cat("Moda:", moda, "\n")

# Mediana
mediana <- median(datos)
cat("Mediana:", mediana, "\n")

# Rango
rango <- max(datos) - min(datos)
cat("Rango:", rango, "\n")

# Varianza
varianza <- var(datos)
cat("Varianza:", varianza, "\n")

# Desviación estándar
desviacion_estandar <- sd(datos)
cat("Desviación estándar:", desviacion_estandar, "\n")

# Rango intercuartílico
rango_intercuartilico <- IQR(datos)
cat("Rango intercuartílico:", rango_intercuartilico, "\n")

# Sesgo y kurtosis (curtosis) usando librería "moments"
# install.packages("moments")  # Instalar si es necesario
library(moments)
sesgo <- skewness(datos)
curtosis <- kurtosis(datos)
cat("Sesgo:", sesgo, "\n")
cat("Curtosis:", curtosis, "\n")


# Datos de los 50 partidos de baloncesto
datos <- c(196, 171, 202, 178, 147, 102, 153, 197, 177, 192, 157, 185, 90, 116, 172, 
           191, 148, 200, 177, 165, 141, 149, 210, 175, 193, 198, 144, 168, 209, 167, 
           195, 163, 210, 175, 130, 143, 187, 166, 139, 149, 108, 199, 150, 154, 194, 
           135, 191, 177, 129, 158)

# Crear una tabla de frecuencias con 10 intervalos
breaks <- seq(min(datos), max(datos), length.out = 11)
frecuencias <- hist(datos, breaks = breaks, plot = FALSE)

# Frecuencia absoluta
frecuencia_absoluta <- frecuencias$counts

# Frecuencia acumulada
frecuencia_acumulada <- cumsum(frecuencia_absoluta)

# Frecuencia relativa
frecuencia_relativa <- frecuencia_absoluta / length(datos)

# Crear un data.frame con las frecuencias
tabla_frecuencias <- data.frame(
  Intervalo = cut(datos, breaks = breaks, include.lowest = TRUE),
  Frecuencia_Absoluta = frecuencia_absoluta,
  Frecuencia_Acumulada = frecuencia_acumulada,
  Frecuencia_Relativa = round(frecuencia_relativa, 4)
)

# Mostrar la tabla de frecuencias
print(tabla_frecuencias)

# Paso 2: Calcular el promedio, moda, mediana, rango, varianza y desviación estándar
# Calcular la marca de clase de cada intervalo
marca_clase <- (head(breaks, -1) + tail(breaks, -1)) / 2

# Promedio
promedio <- sum(marca_clase * frecuencia_absoluta) / sum(frecuencia_absoluta)
print(paste("Promedio:", promedio))

# Moda (Marca de clase del intervalo con mayor frecuencia)
moda <- marca_clase[which.max(frecuencia_absoluta)]
print(paste("Moda:", moda))

# Mediana (el valor que cae en la mitad de los datos)
mediana <- marca_clase[which.min(abs(frecuencia_acumulada - length(datos) / 2))]
print(paste("Mediana:", mediana))

# Rango
rango <- max(datos) - min(datos)
print(paste("Rango:", rango))

# Varianza
varianza <- sum(frecuencia_absoluta * (marca_clase - promedio)^2) / (length(datos) - 1)
print(paste("Varianza:", varianza))

# Desviación estándar
desviacion_estandar <- sqrt(varianza)
print(paste("Desviación estándar:", desviacion_estandar))

# Paso 3: Calcular el rango intercuartílico
# Q1 y Q3
q1 <- marca_clase[which.min(abs(frecuencia_acumulada - length(datos) * 0.25))]
q3 <- marca_clase[which.min(abs(frecuencia_acumulada - length(datos) * 0.75))]
rango_intercuartilico <- q3 - q1
print(paste("Rango intercuartílico:", rango_intercuartilico))


# Datos de los 50 partidos de baloncesto
datos <- c(196, 171, 202, 178, 147, 102, 153, 197, 177, 192, 157, 185, 90, 116, 172, 
           191, 148, 200, 177, 165, 141, 149, 210, 175, 193, 198, 144, 168, 209, 167, 
           195, 163, 210, 175, 130, 143, 187, 166, 139, 149, 108, 199, 150, 154, 194, 
           135, 191, 177, 129, 158)

# Paso 1: Cálculo de las estadísticas para datos sin agrupar

# Promedio
promedio_sin_agrup <- mean(datos)
print(paste("Promedio (sin agrupar):", promedio_sin_agrup))

# Moda (valor que más se repite)
moda_sin_agrup <- as.numeric(names(sort(table(datos), decreasing=TRUE)[1]))
print(paste("Moda (sin agrupar):", moda_sin_agrup))

# Mediana
mediana_sin_agrup <- median(datos)
print(paste("Mediana (sin agrupar):", mediana_sin_agrup))

# Rango
rango_sin_agrup <- max(datos) - min(datos)
print(paste("Rango (sin agrupar):", rango_sin_agrup))

# Varianza
varianza_sin_agrup <- var(datos)
print(paste("Varianza (sin agrupar):", varianza_sin_agrup))

# Desviación estándar
desviacion_estandar_sin_agrup <- sd(datos)
print(paste("Desviación estándar (sin agrupar):", desviacion_estandar_sin_agrup))

# Paso 2: Calcular el rango intercuartílico
q1_sin_agrup <- quantile(datos, 0.25)
q3_sin_agrup <- quantile(datos, 0.75)
rango_intercuartilico_sin_agrup <- q3_sin_agrup - q1_sin_agrup
print(paste("Rango intercuartílico (sin agrupar):", rango_intercuartilico_sin_agrup))

# Paso 3: Evaluar el sesgo y la curtosis (asimetría y apuntamiento)
# Sesgo
sesgo_sin_agrup <- mean((datos - promedio_sin_agrup)^3) / (desviacion_estandar_sin_agrup^3)
print(paste("Sesgo (sin agrupar):", sesgo_sin_agrup))

# Curtosis
curtosis_sin_agrup <- mean((datos - promedio_sin_agrup)^4) / (desviacion_estandar_sin_agrup^4) - 3
print(paste("Curtosis (sin agrupar):", curtosis_sin_agrup))






