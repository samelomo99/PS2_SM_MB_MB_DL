

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

# ---------- DATOS ---------- #

#- Los archivos de datos son demasiado grandes para GitHub. Lee los datos del siguiente enlace:

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



# ---------- MERGE Y CREACIÓN DE VARIABLES ---------- #

# TRAIN DATA

## Uniendo a nivel individuo
train_completo_personas <- left_join(train_personas, train_hogares, by = "id")
head(train_completo_personas)

#- Generamos variables de ocupados, distinguiendo entre desocupados e inactivos
train_completo_personas <- train_completo_personas %>%
  mutate(oc_total = case_when(
    Oc == 1 ~ 1,
    Des == 1 ~ 2,
    Ina == 1 ~ 3,
    TRUE ~ NA_real_
  ))

skim(train_completo_personas)

#- Cambiamos NA por 0 para variable de oc, des e ina
train_completo_personas$Oc[is.na(train_completo_personas$Oc)] <- 0
train_completo_personas$Des[is.na(train_completo_personas$Des)] <- 0
train_completo_personas$Ina[is.na(train_completo_personas$Ina)] <- 0

#- Para las variables P6100, P6210, P6210s1 y oficio, cambiamos NA por el valor
#- correspondiente a no sabe / no responde 
train_completo_personas$P6100[is.na(train_completo_personas$P6100)] <- 9
train_completo_personas$P6210[is.na(train_completo_personas$P6210)] <- 9
# train_completo_personas$Oficio[is.na(train_completo_personas$Oficio)] <- mean(train_completo_personas$Oficio)
#- Para oficio o cambiaría por mean o directamente elimino los NA


## A nivel hogar
#- Creamos resumen por hogar
resumen_familias <- train_completo_personas %>%
  group_by(id) %>%
  summarise(
    # Variables compartidas (colapsadas a nivel hogar)
    P6020_moda = names(sort(table(P6020), decreasing = TRUE))[1],  # Moda del género
    P6040_prom = mean(P6040, na.rm = TRUE),  # Promedio de edad del hogar
    P6050_jefe = sum(P6050 == 1, na.rm = TRUE),  # Cuántos jefes hay (debería ser 1)
    P6100_moda = names(sort(table(P6100), decreasing = TRUE))[1],
    P6210_moda = names(sort(table(P6210), decreasing = TRUE))[1],
    #oficio_moda = names(sort(table(oficio), decreasing = TRUE))[1],
      
    # Sexo, salud y pensión del jefe del hogar
    sexo_jefe = P6020[P6050 == 1][1],
    salud_jefe = P6090[P6050 == 1][1],
    pension_jefe = P6920[P6050 == 1][1],
    edad_jefe = P6040[P6050 == 1][1],
    oc_jefe = oc_total[P6050 == 1][1],

    # Edad promedio del hogar
    edad_promedio = mean(P6040, na.rm = TRUE),
    
    # Moda del estrato
    estrato_moda = names(sort(table(Estrato1), decreasing = TRUE))[1],
    
    # Pet: al menos un miembro con Pet == 1
    hogar_pet = as.integer(any(Pet == 1)),
    
    # Tasa de dependencia
    t_dependencia = ifelse(sum(Pet, na.rm = TRUE) > 0, sum(Ina == 1, na.rm = TRUE) / sum(Pet, na.rm = TRUE), NA),
    
    # Ingresos (sumados individualmente)
    ingreso_impa = sum(Impa, na.rm = TRUE),
    ingreso_isa = sum(Isa, na.rm = TRUE),
    ingreso_ie = sum(Ie, na.rm = TRUE),
    ingreso_imdi = sum(Imdi, na.rm = TRUE),
    ingreso_iof1es = sum(Iof1es, na.rm = TRUE),
    ingreso_iof2es = sum(Iof2es, na.rm = TRUE),
    ingreso_iof3hes = sum(Iof3hes, na.rm = TRUE),
    ingreso_iof3ies = sum(Iof3ies, na.rm = TRUE),
    ingreso_p6545s1 = sum(P6545s1, na.rm = TRUE),
    ingreso_p6610s1 = sum(P6610s1, na.rm = TRUE),
    ingreso_p6620 = sum(P6620, na.rm = TRUE),
    ingreso_p7510s1 = sum(P7510s1, na.rm = TRUE),
    ingreso_p7510s2 = sum(P7510s2, na.rm = TRUE),
    ingreso_p7510s3 = sum(P7510s3, na.rm = TRUE)
  ) %>%
  ungroup()

#- Unimos la base de hogares con el resumen de personas a nivel hogar
train_completo_hogares <- left_join(train_hogares, resumen_familias, by = "id")

glimpse(train_completo_hogares)  # Estructura general
head(train_completo_hogares)     # Primeras filas
summary(train_completo_hogares)  # Estadísticas básicas

# Finalmente guardamos
# Guardar en formato R
saveRDS(train_completo_personas, file = "train_completo_personas.rds")

# Guardar en formato CSV
write.csv(train_completo_personas, file = "train_completo_personas.csv", row.names = FALSE)

# Guardar en formato R
saveRDS(train_completo_hogares, file = "train_completo_hogares.rds")

# Guardar en formato CSV
write.csv(train_completo_hogares, file = "train_completo_hogares.csv", row.names = FALSE)



########################Para las variables de testeo #####################

#importar las bases de datos 

test_hogares <- read.csv("C:/Users/Miguel Blanco/OneDrive/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/Taller 2/Bases de Datos/test_hogares.csv", header=TRUE, row.names=NULL)
View(test_hogares)
colnames(test_hogares)

test_personas <- read.csv("C:/Users/Miguel Blanco/OneDrive/Materias Uniandes/2025 10/Big Data y Maching Learning para Economia Aplicada/Nueva carpeta/Taller 2/Bases de Datos/test_personas.csv", header=TRUE, row.names=NULL)
View(test_personas)
colnames(test_personas)


#### Uniendo a nivel individuo####################
test_completo_personas <- left_join(test_personas, test_hogares, by = "id")
head(test_completo_personas)

#\a nivel hogar\#

# Crear resumen por familia
resumen_familias <- test_personas %>%
  group_by(id) %>%
  summarise(
    # Sexo, salud y pensión del jefe del hogar
    sexo_jefe = P6020[P6050 == 1][1],
    salud_jefe = P6090[P6050 == 1][1],
    pension_jefe = P6920[P6050 == 1][1],
    oc_jefe = Oc[P6050 == 1][1],
    des_jefe = Des[P6050 == 1][1],
    ina_jefe = Ina[P6050 == 1][1],
    
    # Edad promedio del hogar
    edad_promedio = mean(P6040, na.rm = TRUE),
    
    # Pet: al menos un miembro con Pet == 1
    hogar_pet = as.integer(any(Pet == 1)),
    
    # Ingresos (sumados individualmente)
    ingreso_p6620 = sum(P6620, na.rm = TRUE),
    ingreso_p7510s1 = sum(P7510s1, na.rm = TRUE),
    ingreso_p7510s2 = sum(P7510s2, na.rm = TRUE),
    ingreso_p7510s3 = sum(P7510s3, na.rm = TRUE)
  ) %>%
  ungroup()

# Unir la base de hogares con el resumen de personas a nivel hogar
test_completo_hogares <- left_join(test_hogares, resumen_familias, by = "id")

glimpse(test_completo_hogares)  # Estructura general
head(test_completo_hogares)     # Primeras filas
summary(test_completo_hogares)  # Estadísticas básicas


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