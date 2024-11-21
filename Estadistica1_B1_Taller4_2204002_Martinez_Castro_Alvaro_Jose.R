# Número de experimentos
num_experimentos <- 10000

# Función para calcular lanzamientos necesarios para un epsilon dado
calcular_lanzamientos <- function(epsilon) {
  resultados <- numeric(num_experimentos)
  
  for (i in 1:num_experimentos) {
    suma_acumulada <- 0
    n <- 0
    
    while (TRUE) {
      n <- n + 1
      lanzamiento <- sample(1:6, size = 1) # Lanzamiento del dado (valores 1 a 6)
      suma_acumulada <- suma_acumulada + lanzamiento
      promedio <- suma_acumulada / n
      
      # Verificar si el promedio está dentro del rango de epsilon
      if (abs(promedio - 3.5) < epsilon) {
        resultados[i] <- n
        break
      }
    }
  }
  
  return(resultados)
}

# a) Epsilon = 0.01 (1%)
epsilon_1 <- 0.01
resultados_1 <- calcular_lanzamientos(epsilon_1)
promedio_1 <- mean(resultados_1)
cat("Promedio de lanzamientos necesarios para epsilon = 0.01 (1%):", promedio_1, "\n")

# Graficar histograma para epsilon = 0.01
hist(resultados_1, breaks = 50, col = "skyblue", main = "Epsilon = 0.01 (1%)",
     xlab = "Número de lanzamientos", ylab = "Frecuencia")

# b) Epsilon = 0.001 (0.1%)
epsilon_2 <- 0.001
resultados_2 <- calcular_lanzamientos(epsilon_2)
promedio_2 <- mean(resultados_2)
cat("Promedio de lanzamientos necesarios para epsilon = 0.001 (0.1%):", promedio_2, "\n")

# Graficar histograma para epsilon = 0.001
hist(resultados_2, breaks = 50, col = "lightgreen", main = "Epsilon = 0.001 (0.1%)",
     xlab = "Número de lanzamientos", ylab = "Frecuencia")

# c) Epsilon = 0.0001 (0.01%)
epsilon_3 <- 0.0001
resultados_3 <- calcular_lanzamientos(epsilon_3)
promedio_3 <- mean(resultados_3)
cat("Promedio de lanzamientos necesarios para epsilon = 0.0001 (0.01%):", promedio_3, "\n")

# Graficar histograma para epsilon = 0.0001
hist(resultados_3, breaks = 50, col = "pink", main = "Epsilon = 0.0001 (0.01%)",
     xlab = "Número de lanzamientos", ylab = "Frecuencia")

