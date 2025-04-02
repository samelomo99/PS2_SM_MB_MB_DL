

#-------------------------------------------------------------------#
## --------------- Problem Set 2: Predicting Poverty ------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#


# ---------- LIBRERÍAS ---------- #

if (!require("dplyr")) install.packages("dplyr")
if (!require("skimr")) install.packages("skimr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("DataExplorer")) install.packages("DataExplorer")

library(dplyr)
library(skimr)
library(ggplot2)
library(DataExplorer)



# ----------------------DESCRIPTIVAS-------------------------------- # ----

# ----------------INSPECCIÓN DE LOS DATOS------------------ # ----

# Crear una lista con los datasets para facilitar el análisis
datasets <- list(
  "train_hogares" = train_hogares,
  "train_personas" = train_personas,
  "test_hogares" = test_hogares,
  "test_personas" = test_personas
)

# Función para explorar cada dataset
explorar_dataset <- function(data, nombre) {
  cat("\n==========================\n")
  cat("Explorando dataset:", nombre, "\n")
  cat("==========================\n")
  
  # Estructura de los datos
  cat("\nEstructura (str):\n")
  str(data)
  
  # Resumen estadístico base
  cat("\nResumen estadístico (summary):\n")
  print(summary(data))
  
  # Estadísticas descriptivas completas con skimr
  cat("\nAnálisis descriptivo (skim):\n")
  print(skim(data))
  
  # Generar reporte exploratorio completo con DataExplorer
  cat("\nGenerando reporte exploratorio (HTML)...\n")
  create_report(data, output_file = paste0(nombre, "_reporte.html"), report_title = paste("Reporte Exploratorio:", nombre))
  
  # Visualizaciones: Histogramas y boxplots para variables numéricas
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  if (length(num_vars) > 0) {
    for (var in num_vars) {
      # Histograma
      p_hist <- ggplot(data, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "blue", color = "black") +
        labs(title = paste("Histograma de", var, "en", nombre), x = var, y = "Frecuencia")
      print(p_hist)
      
      # Boxplot
      p_box <- ggplot(data, aes_string(y = var)) +
        geom_boxplot(fill = "orange", color = "black") +
        labs(title = paste("Boxplot de", var, "en", nombre), y = var)
      print(p_box)
    }
  } else {
    cat("No se encontraron variables numéricas para visualizar.\n")
  }
  
  cat("\nFin de la exploración de", nombre, "\n\n")
}

# Aplicar la función a cada dataset
for(nombre in names(datasets)) {
  explorar_dataset(datasets[[nombre]], nombre)
}



# ----------------MANEJO DE DATOS Y CREACIÓN DE VARIABLES RELEVANTAS------------------ # ----





# ----------------------MODELOS------------------------------------- # ----