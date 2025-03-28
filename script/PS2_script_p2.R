

#-------------------------------------------------------------------# ----
## --------------- Problem Set 2: Predicting Poverty -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#


# ----------------------LIBRERIAS----------------------------------- # ----

if (!require("dplyr")) install.packages("dplyr")
if (!require("skimr")) install.packages("skimr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("DataExplorer")) install.packages("DataExplorer")

library(dplyr)
library(skimr)
library(ggplot2)
library(DataExplorer)

# ----------------------DATOS--------------------------------------- # ----

#Los archivos de datos son demasiado grandes para GitHub. Lee los datos del siguiente enlace:

# URLs de los datasets
train_hogares_url <- "https://cgiar-my.sharepoint.com/personal/d_c_lopera_cgiar_org/_layouts/15/download.aspx?share=ERoWsIAssWZFlt2ffTOvAkMBptv6eu7iCYmDmiDCKAPeuw"
train_personas_url <- "https://cgiar-my.sharepoint.com/personal/d_c_lopera_cgiar_org/_layouts/15/download.aspx?share=EdAlNQlv7TNBkaV3ZSFIrLEBS4O1JH6ph7gD2dBUWy4z8w"
test_hogares_url <- "https://cgiar-my.sharepoint.com/personal/d_c_lopera_cgiar_org/_layouts/15/download.aspx?share=ERh0vDharuJGniJ2ClmQ9TMBYoP7WLeMgWtWTmHWpEnh7A"
test_personas_url <- "https://cgiar-my.sharepoint.com/personal/d_c_lopera_cgiar_org/_layouts/15/download.aspx?share=Ec1ococzdcVNgHfZziXibXwBCQFDPh2MdoDi_syhIg5LYw"

# Descargar archivos temporales
temp_train_hogares <- tempfile(fileext = ".csv")
temp_train_personas <- tempfile(fileext = ".csv")
temp_test_hogares <- tempfile(fileext = ".csv")
temp_test_personas <- tempfile(fileext = ".csv")

download.file(train_hogares_url, destfile = temp_train_hogares, mode = "wb")
download.file(train_personas_url, destfile = temp_train_personas, mode = "wb")
download.file(test_hogares_url, destfile = temp_test_hogares, mode = "wb")
download.file(test_personas_url, destfile = temp_test_personas, mode = "wb")

# Cargar los datos en R (asumiendo que son archivos CSV)
train_hogares <- read.csv(temp_train_hogares)
train_personas <- read.csv(temp_train_personas)
test_hogares <- read.csv(temp_test_hogares)
test_personas <- read.csv(temp_test_personas)

# Limpiar archivos temporales
unlink(temp_train_hogares)
unlink(temp_train_personas)
unlink(temp_test_hogares)
unlink(temp_test_personas)


# ----------------------DESCRIPTIVAS-------------------------------- # ----



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









# ----------------------MODELOS------------------------------------- # ----