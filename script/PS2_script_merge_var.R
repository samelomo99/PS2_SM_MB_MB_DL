

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

install.packages("tidyr")
# Luego, carga el paquete:
library(tidyr)
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

###ajuste en la variables educacion 

table(train_completo_personas$P6210, useNA = "ifany")
table(train_completo_personas$P6210s1, useNA = "ifany")

train_completo_personas <- train_completo_personas %>%
  mutate(P6210s1 = case_when(
    P6210 == 1 & is.na(P6210s1) ~ 0,
    TRUE ~ P6210s1
  ))

train_completo_personas <- train_completo_personas %>%
  mutate(P6210s1 = case_when(
    P6210 == 2 & is.na(P6210s1) ~ 0,
    TRUE ~ P6210s1
  ))

train_completo_personas <- train_completo_personas %>%
  mutate(P6210 = if_else(P6210s1 == 0, 1, P6210))

train_completo_personas <- train_completo_personas %>%
  mutate(P6210 = case_when(
    P6210s1 >= 1 & P6210s1 <= 5  ~ 3,
    P6210s1 >= 6 & P6210s1 <= 9  ~ 4,
    P6210s1 >= 10 & P6210s1 <= 13 ~ 5,
    TRUE ~ P6210  # En otros casos se mantiene el valor original
  ))

train_completo_personas <- train_completo_personas %>%
  mutate(P6210 = if_else(P6210 == 9, 1, P6210))

table(is.na(train_completo_personas$P6210), is.na(train_completo_personas$P6210s1))

# Identificar los índices donde P6210 y P6210s1 son NA
idx <- which(is.na(train_completo_personas$P6210) & is.na(train_completo_personas$P6210s1))

# Reemplazar para esos índices:
train_completo_personas$P6210[idx]   <- 1
train_completo_personas$P6210s1[idx] <- 0

table(train_completo_personas$P6210[train_completo_personas$P6210s1 == 99], useNA = "ifany")

train_completo_personas <- train_completo_personas %>%
  mutate(P6210s1 = if_else(P6210s1 == 99 & P6210 == 1, 0, P6210s1))

#Por los valores que toma la variable p6210s1,se intuye que esta se puede
#tomar directmente como los años de educacion


train_completo_personas <- train_completo_personas %>%
  mutate(
    mujer          = ifelse(P6020 == 2, 1, 0),
    H_Head         = ifelse(P6050 == 1, 1, 0),
    menor          = ifelse(P6040 <= 6, 1, 0),
    ocupado        = ifelse(is.na(Oc), 0, 1),
    sin_educacion  = ifelse(P6210 == 1, 1, 0),  # 1 si P6210 es 1 (ninguno)
    recibe_ayuda   = case_when(
      P7510s3 == 1 ~ 1,         # Sí recibió ayuda
      P7510s3 %in% c(2, 9) ~ 0,   # No recibió ayuda o no sabe
      TRUE ~ NA_real_            # Otros casos, si los hubiera
    )
  )


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
    ingreso_p7510s3 = sum(P7510s3, na.rm = TRUE),
    
    #NUEVAS 
    nmujeres=sum(mujer,na.rm=TRUE),
    nmenores=sum(menor,na.rm=TRUE),
    maxEducLevel=max(P6210,na.rm=TRUE),
    nocupados=sum(ocupado,na.rm=TRUE),
    n_sin_educacion = sum(sin_educacion, na.rm = TRUE),    # Número de personas sin educación (P6210 == 1)
    
    # Variables específicas del jefe del hogar
    H_Head_mujer       = mujer[P6050 == 1][1],
    H_Head_Educ_level  = P6210[P6050 == 1][1],
    H_Head_ocupado     = ocupado[P6050 == 1][1],
    
    # Nueva variable: número de personas en el hogar con recibe_ayuda == 1
    n_recibe_ayuda = sum(recibe_ayuda == 1, na.rm = TRUE),
    
    #Promedio de años de educación (P6210s1) de los miembros con edad >= 18
    clima_educ = mean(P6210s1[P6040 >= 18], na.rm = TRUE),
  ) %>%
  ungroup()

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

###ajuste en la variables educacion 

table(test_completo_personas$P6210, useNA = "ifany")
table(test_completo_personas$P6210s1, useNA = "ifany")

test_completo_personas <- test_completo_personas %>%
  mutate(P6210s1 = case_when(
    P6210 == 1 & is.na(P6210s1) ~ 0,
    TRUE ~ P6210s1
  ))

test_completo_personas <- test_completo_personas %>%
  mutate(P6210s1 = case_when(
    P6210 == 2 & is.na(P6210s1) ~ 0,
    TRUE ~ P6210s1
  ))

test_completo_personas <- test_completo_personas %>%
  mutate(P6210 = if_else(P6210s1 == 0, 1, P6210))

test_completo_personas <- test_completo_personas %>%
  mutate(P6210 = case_when(
    P6210s1 >= 1 & P6210s1 <= 5  ~ 3,
    P6210s1 >= 6 & P6210s1 <= 9  ~ 4,
    P6210s1 >= 10 & P6210s1 <= 13 ~ 5,
    TRUE ~ P6210  # En otros casos se mantiene el valor original
  ))

test_completo_personas <- test_completo_personas %>%
  mutate(P6210 = if_else(P6210 == 9, 1, P6210))

table(is.na(test_completo_personas$P6210), is.na(test_completo_personas$P6210s1))

# Identificar los índices donde P6210 y P6210s1 son NA
idx <- which(is.na(test_completo_personas$P6210) & is.na(test_completo_personas$P6210s1))

# Reemplazar para esos índices:
test_completo_personas$P6210[idx]   <- 1
test_completo_personas$P6210s1[idx] <- 0

table(test_completo_personas$P6210[test_completo_personas$P6210s1 == 99], useNA = "ifany")

test_completo_personas <- test_completo_personas %>%
  mutate(P6210s1 = if_else(P6210s1 == 99 & P6210 == 1, 0, P6210s1))

#Por los valores que toma la variable p6210s1,se intuye que esta se puede
#tomar directmente como los años de educacion


test_completo_personas <- test_completo_personas %>%
  mutate(
    mujer          = ifelse(P6020 == 2, 1, 0),
    H_Head         = ifelse(P6050 == 1, 1, 0),
    menor          = ifelse(P6040 <= 6, 1, 0),
    ocupado        = ifelse(is.na(Oc), 0, 1),
    sin_educacion  = ifelse(P6210 == 1, 1, 0),  # 1 si P6210 es 1 (ninguno)
    recibe_ayuda   = case_when(
      P7510s3 == 1 ~ 1,         # Sí recibió ayuda
      P7510s3 %in% c(2, 9) ~ 0,   # No recibió ayuda o no sabe
      TRUE ~ NA_real_            # Otros casos, si los hubiera
    )
  )

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
    ingreso_p7510s3 = sum(P7510s3, na.rm = TRUE),
    
    #NUEVAS 
    nmujeres=sum(mujer,na.rm=TRUE),
    nmenores=sum(menor,na.rm=TRUE),
    maxEducLevel=max(P6210,na.rm=TRUE),
    nocupados=sum(ocupado,na.rm=TRUE),
    n_sin_educacion = sum(sin_educacion, na.rm = TRUE),    # Número de personas sin educación (P6210 == 1)
    
    # Variables específicas del jefe del hogar
    H_Head_mujer       = mujer[P6050 == 1][1],
    H_Head_Educ_level  = P6210[P6050 == 1][1],
    H_Head_ocupado     = ocupado[P6050 == 1][1],
    
    # Nueva variable: número de personas en el hogar con recibe_ayuda == 1
    n_recibe_ayuda = sum(recibe_ayuda == 1, na.rm = TRUE),
    
    #Promedio de años de educación (P6210s1) de los miembros con edad >= 18
    clima_educ = mean(P6210s1[P6040 >= 18], na.rm = TRUE),
  ) %>%
  ungroup()


## Unir los resúmenes al nivel hogar

#- Unimos la base de hogares con el resumen de personas a nivel hogar
test_completo_hogares <- left_join(test_hogares, resumen_familias_test, by = "id")

glimpse(test_completo_hogares)  # Estructura general
summary(test_completo_hogares)  # Estadísticas básicas

# Finalmente guardamos la base de hogares

# Guardar en formato R
saveRDS(test_completo_hogares, file = "test_completo_hogares.rds")

# Guardar en formato CSV
write.csv(test_completo_hogares, file = "test_completo_hogares.csv", row.names = FALSE)