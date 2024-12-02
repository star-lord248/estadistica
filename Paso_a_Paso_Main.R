# =============================================
# LEER DATOS DESDE UNA URL (SIN ARCHIVO LOCAL)
# =============================================

# Instalar y cargar librerías necesarias
if (!require(httr)) install.packages("httr", dependencies = TRUE)
if (!require(readr)) install.packages("readr", dependencies = TRUE)

library(httr)
library(readr)

# URL del archivo CSV en línea
url <- 'https://www.datos.gov.co/resource/4zwu-ra3f.csv'

# Solicitud GET para verificar la conexión
api_response <- GET(url)

# Si la conexión es exitosa, leer los datos
if (api_response$status_code == 200) {
  print("Conexión exitosa. Cargando los datos desde la URL...")
  
  # Leer el archivo CSV directamente desde la URL
  aux <- read_csv(url, show_col_types = FALSE)
  
  # Mostrar los primeros registros para verificar la lectura
  print(head(aux))
} else {
  print(paste("Error al conectar con la URL:", api_response$status_code))
}

# =============================================
# ANÁLISIS DE DATOS (Solo si se cargan correctamente)
# =============================================

# Verificar si los datos fueron cargados
if (exists("aux")) {
  print("Datos cargados correctamente desde la URL.")
  
  # Mostrar primeros y últimos registros
  print(head(aux))  # Primeros registros
  print(tail(aux))  # Últimos registros
  View(aux)         # Visualizar en el visor de datos (solo funciona en RStudio)
  print(names(aux)) # Nombres de las columnas
  
  # Verificar tipos de datos de las columnas
  print(class(aux$PREMIO))
  print(class(aux$SORTEO))
  print(class(aux$NUMERO))
  print(class(aux$SERIE))
  
  # Verificar valores únicos
  print(unique(aux$SORTEO))
  print(unique(aux$SERIE))
  
  # Verificar valores faltantes
  print(sum(is.na(aux)))        # Total de valores faltantes
  print(colSums(is.na(aux)))    # Valores faltantes por columna
  
  # Resumen estadístico de los datos
  print(summary(aux))
  print(summary(aux$PREMIO))    # Estadísticas descriptivas de la columna PREMIO
  print(summary(aux$NUMERO))    # Estadísticas descriptivas de la columna NUMERO
  
  # =============================================
  # VISUALIZACIONES BÁSICAS
  # =============================================
  
  # Histograma de la columna PREMIO
  hist(aux$PREMIO, breaks = 20, main = "Distribución de Premios", 
       xlab = "Premios", col = "skyblue")
  
  # Diagrama de dispersión (scatter plot) de PREMIO vs. NUMERO
  plot(aux$NUMERO, aux$PREMIO, xlab = "Número", ylab = "Premio", 
       main = "Premio vs Número", pch = 19, col = "blue")
  
  # Boxplot de PREMIO por SERIE
  boxplot(aux$PREMIO ~ aux$SERIE, main = "Boxplot de Premios por Serie",
          xlab = "Serie", ylab = "Premio", col = "lightgreen")
  
  # Tabla de frecuencia de SORTEO y gráfico de barras
  tabledata <- table(aux$SORTEO)
  barplot(tabledata, horiz = FALSE, las = 2, main = "Frecuencia por Sorteo")
  
  # Gráfico de pastel (pie chart) de la distribución de sorteos
  pie(tabledata, main = "Distribución de Sorteos")
} else {
  print("No se encontraron datos para analizar.")
}

# =============================================
# ENLACE AL REPOSITORIO DE GITHUB
# =============================================
# Para más información y actualizaciones sobre el proyecto, visita:
# GitHub: https://github.com/star-lord248/estadistica

