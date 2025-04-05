

#-------------------------------------------------------------------#
## --------------- Problem Set 2: Predicting Poverty ------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#


# ---------- LIBRERÍAS ---------- # ----

if (!require("pacman")) install.packages("pacman")
library(pacman)

pacman::p_load(
  readr,        # Importar datos (ya incluido en tidyverse)
  labelled,     # Manejo de etiquetas
  naniar,       # Visualizar datos faltantes
  DataExplorer, # Gráficos de missing values
  psych,        # Estadísticas descriptivas
  rvest,        # Web scraping
  rio,          # Importar/exportar datos
  tidyverse,    # Conjunto de paquetes para tidy data (incluye dplyr, ggplot2, etc.)
  skimr,        # Resumen de datos
  visdat,       # Visualizar datos faltantes
  corrplot,     # Gráficos de correlación
  gridExtra,    # Organización de gráficos
  MASS,         # Funciones estadísticas diversas
  stargazer,    # Tablas para salida a TEX
  chromote,     # Automatización de navegador (útil para scraping avanzado)
  ggplot2,      # Gráficos (ya incluido en tidyverse)
  boot,         # Funciones de bootstrap
  patchwork,    # Combinación de gráficos
  caret,         # For predictive model assessment
  purrr,
  kableExtra,   # Opciones adicionales para kable()
  dplyr,          # Manipulación de datos
  summarytools,
  knitr,          # kable() para generar tablas en LaTeX
  xtable,
  tidyr,
  gmodels,
  glmnet
)

# ---------- BASE DE DATOS ---------- # ----

train <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS2_SM_MB_MB_DL/refs/heads/main/stores/train_completo_hogares.csv"
  )

#- Quito pension_jefe, y dropeo los miss de salud_jefe, oc_jefe y t_dependencia
train <- train %>% 
  mutate(Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes"))) %>% 
  dplyr::select(P5010, P5090, Nper, Depto, Pobre, P6040_prom, 
                P6050_jefe, P6210_moda, sexo_jefe, salud_jefe, 
                edad_jefe, oc_jefe, t_dependencia) %>% 
  filter(!is.na(salud_jefe), !is.na(oc_jefe), !is.na(t_dependencia))

skim(train)

test <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS2_SM_MB_MB_DL/refs/heads/main/stores/test_completo_hogares.csv"
  )
test <- test %>% dplyr::select(id, P5010, P5090, Nper, Depto, P6040_prom, 
                                 P6050_jefe, P6210_moda, sexo_jefe, salud_jefe, 
                                 edad_jefe, oc_jefe, t_dependencia)


# ---------- DESCRIPCIÓN DATOS ---------- # ----

#- Inspección de los datos creados en las nuevas bases de datos, no de los datos
#- que teníamos desde el principio, es mucho.

#- Crear una lista con los datasets para facilitar el análisis
datasets <- list(
  "train_hogares" = train,
  "test_hogares" = test
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


# ---------- MODELOS ---------- # ----

# OLS ----

# ELASTIC NET ----

set.seed(10101)
ctrl <- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

# Usando todas las variables del data frame como predictoras
model_en1 <- train(Pobre~.,
  data = train,
  metric = "Accuracy",
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = expand.grid(
    alpha = seq(0,1,by=.2), #- se prueban valores de alpha desde 0 a 1 en pasos de 0,2 
    lambda = 10^seq(10, -2, length = 10) #- parámetro de penalización
  )
)

model_en1

#- Hacemos la predicción
predictSample <- test   %>% 
  mutate(pobre_lab = predict(model_en1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% dplyr::select(id,pobre_lab)

head(predictSample)

#- Transformamos variable pobre para que cumpla con la especificación de la competencia
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  dplyr::select(id,pobre)
head(predictSample)

#- Formato específico Kaggle 
lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model_en1$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model_en1$bestTune$alpha))

name<- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample,name, row.names = FALSE)

# RANDOM FOREST ----













