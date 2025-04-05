

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
download.file(train_personas_url, destfile = temp_train_personas, mode = "wb", 
              method = "curl", extra = "-m 120")
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

# ----- TRAIN DATA ----- #

## Uniendo a nivel individuo
train_completo_personas <- left_join(train_personas, train_hogares, by = "id")
head(train_completo_personas)

# ----------------------------------------------------
## Corrección NA

#- Generamos variables de ocupados, distinguiendo entre desocupados e inactivos
train_completo_personas <- train_completo_personas %>%
  mutate(oc_total = case_when(
    Oc == 1 ~ 1,
    Des == 1 ~ 2,
    Ina == 1 ~ 3,
    TRUE ~ NA_real_
  ))

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
# ----------------------------------------------------

## A nivel hogar
#- Creamos resumen por hogar
resumen_familias_train <- train_completo_personas %>%
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
    ocupacion_jefe = P6430[P6050 == 1][1],
    
    # Edad promedio del hogar
    edad_promedio = mean(P6040, na.rm = TRUE),
    
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

## Cálculo de la variable 'num_age_educ' a nivel individual
train_completo_personas <- train_completo_personas %>%
  mutate(num_age_educ = case_when(
    P6210 == 1 ~ 0,                           # Ninguno: 0 años
    P6210 == 2 ~ as.numeric(P6210s1),           # Preescolar: se toma P6210s1
    P6210 == 3 ~ as.numeric(P6210s1),           # Básica primaria: se toma P6210s1
    P6210 == 4 ~ as.numeric(P6210s1),           # Básica secundaria: se toma P6210s1
    P6210 == 5 ~ as.numeric(P6210s1),           # Media: se toma P6210s1
    P6210 == 6 ~ 13 + as.numeric(P6210s1),      # Superior: 13 años (básica y media) + años aprobados en superior
    P6210 == 9 ~ NA_real_,                      # No sabe, no informa: NA
    TRUE ~ NA_real_
  ))

# Estadística descriptiva de años de educacion  'num_age_educ'
summary(train_completo_personas$num_age_educ)
# Contar la cantidad de missing
sum(is.na(train_completo_personas$num_age_educ))

## Cálculo del clima educativo del hogar

# Se calcula como el promedio de 'num_age_educ' de las personas adultas (edad >= 18) por hogar
clima_edu <- train_completo_personas %>%
  filter(P6040 >= 18) %>%   # Solo personas adultas
  group_by(id) %>%
  summarise(clima_eduactivo = mean(num_age_educ, na.rm = TRUE)) %>%
  ungroup()

## Cálculo de los años de educación del jefe del hogar
# Se extrae 'num_age_educ' del jefe (P6050 == 1)
edu_jefe <- train_completo_personas %>%
  filter(P6050 == 1) %>%
  group_by(id) %>%
  summarise(edu_jefe = first(num_age_educ)) %>%
  ungroup()

## Unir los resúmenes al nivel hogar

# Se unen 'clima_eduactivo' y 'edu_jefe' al resumen general por hogar
resumen_familias_train <- resumen_familias_train %>%
  left_join(clima_edu, by = "id") %>%
  left_join(edu_jefe, by = "id")

#- Unimos la base de hogares con el resumen de personas a nivel hogar
train_completo_hogares <- left_join(train_hogares, resumen_familias_train, by = "id")

glimpse(train_completo_hogares)  # Estructura general
summary(train_completo_hogares)  # Estadísticas básicas

# Finalmente guardamos la base de hogares

# Guardar en formato R
saveRDS(train_completo_hogares, file = "train_completo_hogares.rds")

# Guardar en formato CSV
write.csv(train_completo_hogares, file = "train_completo_hogares.csv", row.names = FALSE)


# ----- TEST DATA ----- #

## Uniendo a nivel individuo

test_completo_personas <- left_join(test_personas, test_hogares, by = "id")
head(test_completo_personas)

# ----------------------------------------------------
## Corrección NA

#- Generamos variables de ocupados, distinguiendo entre desocupados e inactivos
test_completo_personas <- test_completo_personas %>%
  mutate(oc_total = case_when(
    Oc == 1 ~ 1,
    Des == 1 ~ 2,
    Ina == 1 ~ 3,
    TRUE ~ NA_real_
  ))

#- Cambiamos NA por 0 para variable de oc, des e ina
test_completo_personas$Oc[is.na(test_completo_personas$Oc)] <- 0
test_completo_personas$Des[is.na(test_completo_personas$Des)] <- 0
test_completo_personas$Ina[is.na(test_completo_personas$Ina)] <- 0

#- Para las variables P6100, P6210, P6210s1 y oficio, cambiamos NA por el valor
#- correspondiente a no sabe / no responde 
test_completo_personas$P6100[is.na(test_completo_personas$P6100)] <- 9
test_completo_personas$P6210[is.na(test_completo_personas$P6210)] <- 9
# test_completo_personas$Oficio[is.na(test_completo_personas$Oficio)] <- mean(test_completo_personas$Oficio)
#- Para oficio o cambiaría por mean o directamente elimino los NA
# ----------------------------------------------------



## A nivel hogar
#- Creamos resumen por hogar
resumen_familias_test <- test_completo_personas %>%
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
    ocupacion_jefe = P6430[P6050 == 1][1],
    
    # Edad promedio del hogar
    edad_promedio = mean(P6040, na.rm = TRUE),
    
    # Pet: al menos un miembro con Pet == 1
    hogar_pet = as.integer(any(Pet == 1)),
    
    # Tasa de dependencia
    t_dependencia = ifelse(sum(Pet, na.rm = TRUE) > 0, sum(Ina == 1, na.rm = TRUE) / sum(Pet, na.rm = TRUE), NA),
    
    # Ingresos (sumados individualmente)
    ingreso_p6620 = sum(P6620, na.rm = TRUE),
    ingreso_p7510s1 = sum(P7510s1, na.rm = TRUE),
    ingreso_p7510s2 = sum(P7510s2, na.rm = TRUE),
    ingreso_p7510s3 = sum(P7510s3, na.rm = TRUE)
  ) %>%
  ungroup()

## Cálculo de la variable 'num_age_educ' a nivel individual
train_completo_personas <- train_completo_personas %>%
  mutate(num_age_educ = case_when(
    P6210 == 1 ~ 0,                           # Ninguno: 0 años
    P6210 == 2 ~ as.numeric(P6210s1),           # Preescolar: se toma P6210s1
    P6210 == 3 ~ as.numeric(P6210s1),           # Básica primaria: se toma P6210s1
    P6210 == 4 ~ as.numeric(P6210s1),           # Básica secundaria: se toma P6210s1
    P6210 == 5 ~ as.numeric(P6210s1),           # Media: se toma P6210s1
    P6210 == 6 ~ 13 + as.numeric(P6210s1),      # Superior: 13 años (básica y media) + años aprobados en superior
    P6210 == 9 ~ NA_real_,                      # No sabe, no informa: NA
    TRUE ~ NA_real_
  ))

# Estadística descriptiva de años de educacion  'num_age_educ'
summary(train_completo_personas$num_age_educ)
# Contar la cantidad de missing
sum(is.na(train_completo_personas$num_age_educ))

## Cálculo del clima educativo del hogar

# Se calcula como el promedio de 'num_age_educ' de las personas adultas (edad >= 18) por hogar
clima_edu <- train_completo_personas %>%
  filter(P6040 >= 18) %>%   # Solo personas adultas
  group_by(id) %>%
  summarise(clima_eduactivo = mean(num_age_educ, na.rm = TRUE)) %>%
  ungroup()

## Cálculo de los años de educación del jefe del hogar
# Se extrae 'num_age_educ' del jefe (P6050 == 1)
edu_jefe <- train_completo_personas %>%
  filter(P6050 == 1) %>%
  group_by(id) %>%
  summarise(edu_jefe = first(num_age_educ)) %>%
  ungroup()

## Unir los resúmenes al nivel hogar

# Se unen 'clima_eduactivo' y 'edu_jefe' al resumen general por hogar
resumen_familias_test <- resumen_familias_test %>%
  left_join(clima_edu, by = "id") %>%
  left_join(edu_jefe, by = "id")


#- Unimos la base de hogares con el resumen de personas a nivel hogar
test_completo_hogares <- left_join(test_hogares, resumen_familias_test, by = "id")

glimpse(test_completo_hogares)  # Estructura general
summary(test_completo_hogares)  # Estadísticas básicas

# Finalmente guardamos la base de hogares

# Guardar en formato R
saveRDS(test_completo_hogares, file = "test_completo_hogares.rds")

# Guardar en formato CSV
write.csv(test_completo_hogares, file = "test_completo_hogares.csv", row.names = FALSE)