
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

train_completo_hogares <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS2_SM_MB_MB_DL/refs/heads/main/stores/train.csv"
)

test_completo_hogares <- read_csv(
  "https://raw.githubusercontent.com/samelomo99/PS2_SM_MB_MB_DL/refs/heads/main/stores/test.csv"
)

#En la base de datos a nivel hogar generaremos la variable de arriendo y seleccionaremos las variables que usaremos posteriormente, 
#entre ellas nuestra variable dependiente (que no se encuentra en la base de test):

train <- train_completo_hogares %>% 
  dplyr::mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  dplyr::select(id,Dominio,arrienda,Pobre,P5010, Nper, salud_jefe, edad_jefe, ocupacion_jefe, t_dependencia, nmujeres, nmenores, nocupados, n_sin_educacion,n_recibe_ayuda, H_Head_mujer, H_Head_Educ_level, clima_educ)

# ---------- PROCESAMIENTO ADICIONAL BASE TRAIN---------- # ---- 

#descripcion rápida de los datos para verificar ajustes necesarios 

skim(train)
table(train$ocupacion_jefe, useNA = "ifany")

#En la variable clima_edu, hay unos missing, que corresponde a hogares donde no hay personas mayores de 18. Se reemplaza por 0, porque originalmente estos casos tenia cero 
train <- train %>%
  replace_na(list(clima_educ = 0))
#para recategorizar la variable ocupacion del jefe, generamos una variable que toma el valor de 1 si el jefe tiene una ocupacion informal y 0 en otro caso

train <- train %>%
  mutate(ocup_jefe_informal = if_else(ocupacion_jefe %in% c(3, 4, 5, 6, 7, 8, 9) | is.na(ocupacion_jefe),
                                      1,
                                      0))
#quitamos las variable ocupacion_jefe de la base porque ya se genero otra variable a partir de ella
train <- train %>% select(-ocupacion_jefe)

#antes de convertir a factor, revisamos los valores de las categorias de la variables categoricas
table(train$arrienda, useNA = "ifany")
table(train$Pobre, useNA = "ifany")
table(train$salud_jefe, useNA = "ifany")
table(train$ocup_jefe_informal, useNA = "ifany")
table(train$H_Head_mujer, useNA = "ifany")
table(train$H_Head_Educ_level, useNA = "ifany")
table(train$n_recibe_ayuda, useNA = "ifany")

# recodificamos algunas variables para mayor claridad, la variable originalmente toma 1 sí, 2 no, 9 no sabe, no informa
# por lo que solo tomamos los casos donde el jefe es cotizante o es beneficiario de alguna entidad de 
#seguridad social en salud
train <- train %>%
  mutate(salud_jefe = if_else(salud_jefe == 1, 1, 0, missing = 0))

table(train$salud_jefe, useNA = "ifany")

#ahora generamos otra dicotomica, que toma el valor de 1 si al menos un miembro del hogar 
# ayudas en dinero de instituciones del país
train <- train %>%
  mutate(recibe_ayuda_ = if_else(n_recibe_ayuda > 0, 1, 0))

table(train$recibe_ayuda_, useNA = "ifany")

#se reemplaza el missing t_dependencia con el promedio 
# Calcular el promedio de t_dependencia ignorando los NA
promedio_t <- mean(train$t_dependencia, na.rm = TRUE)

# Reemplazar NA con el promedio utilizando if_else()
train <- train %>% 
  mutate(t_dependencia = if_else(is.na(t_dependencia), promedio_t, t_dependencia))

#Ahora, vamos a convertir las variables categoricas a factores.Importante: 
#los factores tienen que tener nombres validos sino se tiene el siguiente error:

#variables continuas 
#Nper            = Número de personas en el hogar 
#edad_jefe       = edad jefe de hogar
#nocupados       = Número de personas ocupadas dentro del hogar
#nmujeres        = Número de mujeres dentro del hogar
#nmenores        = Número de menores de 6 años
#n_sin_educacion = Número de personas sin eduacion
#t_dependencia   = Tasa de dependencia 
#clima_educ      = Clima educativo (numero de años promedio de educacion de los jefes de hogar)
#n_recibe_ayuda  = Número de personas que reciben ayuda de institucion del gobierno

#categoricas
#arrienda            1=arrienda                0=otro caso
#Pobre               1=Pobre                   0=No
#salud_jefe          1=si tiene salud          0=otro caso
#ocup_jefe_informal  1=ocupacion informal jefe 0=otro caso
#H_Head_mujer        1=jefe mujer              0=otro caso
#H_Head_Educ_level   1= Ninguno 2=Preescolar 3=Primaria 4= secundaria 5=media 6=superior
#recibe_ayuda_       1= al menos un miembro del hogar recibio ayuda de insticuion del gobierno 0=otro caso

train <- train %>% 
  mutate(
    Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Si")),
    arrienda = factor(arrienda, levels = c(0, 1), labels = c("No", "Si")),
    salud_jefe = factor(salud_jefe, levels = c(0, 1), labels = c("No", "Si")),
    ocup_jefe_informal = factor(ocup_jefe_informal, levels = c(0, 1), labels = c("No", "Si")),
    H_Head_mujer = factor(H_Head_mujer, levels = c(0, 1), labels = c("No", "Si")),
    recibe_ayuda_ = factor(recibe_ayuda_, levels = c(0, 1), labels = c("No", "Si")),
    Dominio = factor(Dominio),
    H_Head_Educ_level = factor(H_Head_Educ_level,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria"))
  )


# ---------- PROCESAMIENTO ADICIONAL BASE TEST---------- # ----       


#En la base de datos a nivel hogar generaremos la variable de arriendo y seleccionaremos las variables que usaremos posteriormente, 
#entre ellas nuestra variable dependiente (que no se encuentra en la base de test):

test<-test_completo_hogares %>% 
  mutate(arrienda=ifelse(P5090==3,1,0)) %>% 
  select(id,Dominio,arrienda,P5010, Nper, salud_jefe, edad_jefe, ocupacion_jefe, t_dependencia, nmujeres, nmenores, nocupados, n_sin_educacion,n_recibe_ayuda, H_Head_mujer, H_Head_Educ_level, clima_educ)

# ---------- PROCESAMIENTO ADICIONAL BASE TEST---------- # ---- 

#descripcion rápida de los datos para verificar ajustes necesarios 

skim(test)
table(train$ocupacion_jefe, useNA = "ifany")

#En la variable clima_edu, hay unos missing, que corresponde a hogares donde no hay personas mayores de 18. Se reemplaza por 0, porque originalmente estos casos tenia cero 
test <-test %>%
  replace_na(list(clima_educ = 0))
#para recategorizar la variable ocupacion del jefe, generamos una variable que toma el valor de 1 si el jefe tiene una ocupacion informal y 0 en otro caso

test <-test %>%
  mutate(ocup_jefe_informal = if_else(ocupacion_jefe %in% c(3, 4, 5, 6, 7, 8, 9) | is.na(ocupacion_jefe),
                                      1,
                                      0))
#quitamos las variable ocupacion_jefe de la base porque ya se genero otra variable a partir de ella
test <-test %>% select(-ocupacion_jefe)

#antes de convertir a factor, revisamos los valores de las categorias de la variables categoricas
table(train$arrienda, useNA = "ifany")
table(train$salud_jefe, useNA = "ifany")
table(train$ocup_jefe_informal, useNA = "ifany")
table(train$H_Head_mujer, useNA = "ifany")
table(train$H_Head_Educ_level, useNA = "ifany")
table(train$n_recibe_ayuda, useNA = "ifany")

# recodificamos algunas variables para mayor claridad, la variable originalmente toma 1 sí, 2 no, 9 no sabe, no informa
# por lo que solo tomamos los casos donde el jefe es cotizante o es beneficiario de alguna entidad de 
#seguridad social en salud
test <-test %>%
  mutate(salud_jefe = if_else(salud_jefe == 1, 1, 0, missing = 0))

table(train$salud_jefe, useNA = "ifany")

#ahora generamos otra dicotomica, que toma el valor de 1 si al menos un miembro del hogar 
# ayudas en dinero de instituciones del país
test <-test %>%
  mutate(recibe_ayuda_ = if_else(n_recibe_ayuda > 0, 1, 0))

table(train$recibe_ayuda_, useNA = "ifany")

# Calcular el promedio de t_dependencia ignorando los NA
promedio_t <- mean(test$t_dependencia, na.rm = TRUE)

# Reemplazar NA con el promedio utilizando if_else()
test <- test %>% 
  mutate(t_dependencia = if_else(is.na(t_dependencia), promedio_t, t_dependencia))
#Ahora, vamos a convertir las variables categoricas a factores.Importante: 
#los factores tienen que tener nombres validos sino se tiene el siguiente error:

#variables continuas 
#Nper            = Número de personas en el hogar 
#edad_jefe       = edad jefe de hogar
#nocupados       = Número de personas ocupadas dentro del hogar
#nmujeres        = Número de mujeres dentro del hogar
#nmenores        = Número de menores de 6 años
#n_sin_educacion = Número de personas sin eduacion
#t_dependencia   = Tasa de dependencia 
#clima_educ      = Clima educativo (numero de años promedio de educacion de los jefes de hogar)
#n_recibe_ayuda  = Número de personas que reciben ayuda de institucion del gobierno

#categoricas
#arrienda            1=arrienda                0=otro caso
#salud_jefe          1=si tiene salud          0=otro caso
#ocup_jefe_informal  1=ocupacion informal jefe 0=otro caso
#H_Head_mujer        1=jefe mujer              0=otro caso
#H_Head_Educ_level   1= Ninguno 2=Preescolar 3=Primaria 4= secundaria 5=media 6=superior
#recibe_ayuda_       1= al menos un miembro del hogar recibio ayuda de insticuion del gobierno 0=otro caso

test <-test %>% 
  mutate(
    arrienda = factor(arrienda, levels = c(0, 1), labels = c("No", "Si")),
    salud_jefe = factor(salud_jefe, levels = c(0, 1), labels = c("No", "Si")),
    ocup_jefe_informal = factor(ocup_jefe_informal, levels = c(0, 1), labels = c("No", "Si")),
    H_Head_mujer = factor(H_Head_mujer, levels = c(0, 1), labels = c("No", "Si")),
    recibe_ayuda_ = factor(recibe_ayuda_, levels = c(0, 1), labels = c("No", "Si")),
    Dominio = factor(Dominio),
    H_Head_Educ_level = factor(H_Head_Educ_level,
                               levels = c(1,2,3,4,5,6),
                               labels = c("Ninguno", "Preescolar", "Primaria", "Secundaria", "Media", "Universitaria"))
  )


# ---------- MODELOS ---------- # ----

# ---------- OLS ---------- # ----
#Montamos la validacion cruzada
set.seed(10101)
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = T)

#Usamos un modelo con todas las varibles

model_ols1 <- train(Pobre~.,
                    data = train,
                    metric = "Accuracy",
                    method = "glm",
                    trControl = ctrl) 

model_ols1
#Haciendo la prediccion 
predictSample <- test   %>% 
  mutate(pobre_lab = predict(model_ols1, newdata = test, type = "raw")    ## predicted class labels
  )  %>% dplyr::select(id,pobre_lab)

head(predictSample)

# Transformamos variable pobre para que cumpla con la especificación de la competencia
predictSample <- predictSample %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  dplyr::select(id,pobre)
head(predictSample)

# Formato específico Kaggle 

name<- paste0(
  "OLS.csv") #Dado que el modelo no tiene hiperparametros no es necesario usar algo mas sofisticado

write.csv(predictSample,name, row.names = FALSE)
# ---------- ELASTIC NET ---------- # ----
set.seed(1410)
library(caret)

# Creamos índices para dividir
index <- createDataPartition(train$Pobre, p = 0.7, list = FALSE)

train_split <- train[index, ]
test_split  <- train[-index, ]
ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = TRUE,
                     savePredictions = T)
# Grilla para glmnet
grid <- expand.grid(
  alpha = seq(0, 1, by = 0.1),
  lambda = 10^seq(10, -2, length = 10)
)

# Función F1 personalizada
f1_summary <- function(data, lev = NULL, model = NULL) {
  precision <- posPredValue(data$pred, data$obs, positive = lev[1])
  recall <- sensitivity(data$pred, data$obs, positive = lev[1])
  f1 <- ifelse((precision + recall) == 0, 0, 2 * precision * recall / (precision + recall))
  out <- c(F1 = f1)
  return(out)
}

# Modelo con Accuracy ----
ctrl_acc <- trainControl(method = "cv", number = 5)
model_acc <- train(Pobre ~ ., data = train_split, method = "glmnet",
                   metric = "Accuracy", trControl = ctrl_acc, tuneGrid = grid)

# Modelo con ROC ----
ctrl_roc <- trainControl(method = "cv", number = 5,
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE)
model_roc <- train(Pobre ~ ., data = train_split, method = "glmnet",
                   metric = "ROC", trControl = ctrl_roc, tuneGrid = grid)

# Modelo con F1 ----
ctrl_f1 <- trainControl(method = "cv", number = 5,
                        summaryFunction = f1_summary,
                        classProbs = TRUE)
model_f1 <- train(Pobre ~ ., data = train_split, method = "glmnet",
                  metric = "F1", trControl = ctrl_f1, tuneGrid = grid)


# ---------- Probamos el mejor modelo ---------- # ----
# Predicciones
pred_acc <- predict(model_acc, newdata = test_split)
pred_roc <- predict(model_roc, newdata = test_split)
pred_f1  <- predict(model_f1,  newdata = test_split)

# Evaluación
library(yardstick)
library(dplyr)

results <- bind_rows(
  data.frame(model = "Accuracy", pred = pred_acc),
  data.frame(model = "ROC", pred = pred_roc),
  data.frame(model = "F1", pred = pred_f1)
) %>%
  mutate(truth = rep(test_split$Pobre, 3)) %>%
  group_by(model) %>%
  summarise(
    Accuracy = accuracy_vec(truth, pred),
    F1       = f_meas_vec(truth, pred),
    Sens     = sens_vec(truth, pred),
    Spec     = spec_vec(truth, pred)
  )

print(results)


# ---------- ENVIO PARA KAGGLE ELASTIC NET ---------- # ---- 
#- Hacemos la predicción
predictSample_en2 <- test   %>% 
  mutate(pobre_lab = predict(model_en2, newdata = test, type = "raw")    ## predicted class labels
  )  %>% dplyr::select(id,pobre_lab)

head(predictSample_en2)

#- Transformamos variable pobre para que cumpla con la especificación de la competencia
predictSample_en2 <- predictSample_en2 %>% 
  mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) %>% 
  dplyr::select(id,pobre)
head(predictSample_en2)

lambda_str <- gsub(
  "\\.", "_", 
  as.character(round(model_en2$bestTune$lambda, 4)))
alpha_str <- gsub("\\.", "_", as.character(model_en2$bestTune$alpha))

name<- paste0(
  "EN_V3_lambda_", lambda_str,
  "_alpha_" , alpha_str, 
  ".csv") 

write.csv(predictSample_en2,name, row.names = FALSE)


# ---------- MODELO LOGIT---------- # ----   

# Instalar (si es necesario) y cargar los paquetes requeridos
# if(!require(caret)) install.packages("caret")
# if(!require(Metrics)) install.packages("Metrics")
# if(!require(pacman)) install.packages("pacman")
# library(caret)
# library(Metrics)
# library(pacman)

#install.packages("MLmetrics")
#require(MLmetrics)

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)


# Modelo 1 ---- 


set.seed(1410)
logit_m1 <- train(Pobre~Nper + edad_jefe + nocupados + nmujeres + nmenores + 
                    H_Head_mujer +  H_Head_Educ_level,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m1

# Extraer los resultados (métricas) en un data frame
df_logit_m1 <- logit_m1$results
# Agregar una columna para identificar el modelo
df_logit_m1$model <- "logit_m1"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m1)

# Modelo 2 ---- 

set.seed(1410)
logit_m2 <- train(Pobre~P5010 + arrienda + ocup_jefe_informal + t_dependencia + 
                    clima_educ + recibe_ayuda_ +  H_Head_mujer,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m2

# Extraer los resultados (métricas) en un data frame
df_logit_m2 <- logit_m2$results
# Agregar una columna para identificar el modelo
df_logit_m2$model <- "logit_m2"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m2)


# Modelo 3 ---- 


set.seed(1410)
logit_m3 <- train(Pobre~Nper + edad_jefe + nocupados + nmujeres + nmenores + 
                    n_sin_educacion + n_recibe_ayuda + arrienda + salud_jefe + ocup_jefe_informal +P5010 +H_Head_mujer + H_Head_Educ_level,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m3

# Extraer los resultados (métricas) en un data frame
df_logit_m3 <- logit_m3$results
# Agregar una columna para identificar el modelo
df_logit_m3$model <- "logit_m3"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m3)


# Modelo 4 ----      

set.seed(1410)
logit_m4 <- train(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                    recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal +P5010 +H_Head_mujer,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m4

# Extraer los resultados (métricas) en un data frame
df_logit_m4 <- logit_m4$results
# Agregar una columna para identificar el modelo
df_logit_m4$model <- "logit_m4"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m4)


# Modelo 5 ----  


X <- c("poly(Nper,2,raw=TRUE)",
       "poly(edad_jefe,2,raw=TRUE)",
       "poly(nocupados,2,raw=TRUE)",
       "poly(nmujeres,2,raw=TRUE)",
       "poly(nmenores,2,raw=TRUE)",
       "poly(clima_educ,2,raw=TRUE)",
       "ocup_jefe_informal*H_Head_mujer",
       "H_Head_mujer*H_Head_Educ_level",
       "t_dependencia",
       "P5010",
       "arrienda")

# Crear la fórmula del modelo concatenando los términos
formula_modelo <- as.formula(paste("Pobre ~", paste(X, collapse = " + ")))
print(formula_modelo)   # Para revisar la fórmula generada

set.seed(1410)
logit_m5 <- train(formula_modelo,
                  data = train, 
                  method = "glm",
                  family = "binomial",
                  metric = "F",
                  trControl = ctrl)

# Visualización del modelo y extracción de resultados
print(logit_m5)
df_logit_m5 <- logit_m5$results
df_logit_m5$model <- "logit_m5"
print(df_logit_m5)

# Modelo 6 ----   

set.seed(1410)
logit_m6 <- train(Pobre ~ edad_jefe +
                    H_Head_mujer +
                    Nper +
                    ocup_jefe_informal * H_Head_mujer +
                    H_Head_Educ_level * H_Head_mujer +
                    clima_educ +
                    arrienda +
                    P5010,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m6

# Extraer los resultados (métricas) en un data frame
df_logit_m6 <- logit_m6$results
# Agregar una columna para identificar el modelo
df_logit_m6$model <- "logit_m6"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m6)


# Modelo 7 ----  

set.seed(1410)
logit_m7 <- train(Pobre~arrienda + ocup_jefe_informal +H_Head_mujer+n_sin_educacion+nmenores+Nper,
                  data = train, 
                  metric = "F",
                  method = "glm",
                  trControl = ctrl,
                  family = "binomial")

# Visualizar los resultados del modelo
logit_m7

# Extraer los resultados (métricas) en un data frame
df_logit_m7 <- logit_m7$results
# Agregar una columna para identificar el modelo
df_logit_m7$model <- "logit_m7"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m7)


#Combinar los resultados de todos modelos para compararlos
df_comparacion <- rbind(df_logit_m1, df_logit_m2, df_logit_m3,df_logit_m4, df_logit_m5,df_logit_m6,df_logit_m7)
print(df_comparacion)

write.csv(df_comparacion, file = "df_comparacionv2.csv", row.names = FALSE)

# ---------- VERIFICACION DE LA ESPECIFICACION DEL MODELO--------- # ---- 

###Prueba para verificar si mi mejor modelo realmente predice bien 

#partimos de la mejor especificacion encontrada en logit, sin embargo para asegurar que nuestro 
# modelo predice bien y no gastar una submission en Kaggle partimos la base de train
# y luego si entrenamos con todos los datos para kaggle

train_2<-train   #generamos un copia de nuestra base train

inTrain <- createDataPartition(
  y = train_2$Pobre,## La variable dependiente u objetivo 
  p = .7, ## Usamos 70%  de los datos en el conjunto de entrenamiento 
  list = FALSE)


train_70 <- train_2[ inTrain,]   #base para entrenamiento proveniente de mi train
test_30  <- train_2[-inTrain,]   #base para test proveniente de mi train

table(train_70$Pobre)
table(test_30$Pobre)

#entreno el modelo con el 70% de los datos

p_load(Metrics)
fiveStats <- function(...)  c(prSummary(...))  


ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = fiveStats,
                    savePredictions = T)
set.seed(1410)
logit_m4_prueba <- train(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                           recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal +P5010 +H_Head_mujer,
                         data = train_70, 
                         metric = "F",
                         method = "glm",
                         trControl = ctrl,
                         family = "binomial")

# Visualizar los resultados del modelo
logit_m4_prueba

# Extraer los resultados (métricas) en un data frame
df_logit_m4_prueba <- logit_m4_prueba$results
# Agregar una columna para identificar el modelo
df_logit_m4_prueba$model <- "logit_m4_prueba"

# Visualiza el data frame con los resultados del MODELO 1
print(df_logit_m4_prueba)

### Predicción en el conjunto de test (30% de los datos)

# Copiamos y preparamos el dataset de prueba
test_30_copy <- test_30
test_30_copy <- dplyr::select(test_30_copy, -Pobre)

# Realizamos las predicciones utilizando test_30_copy
predictSample_logit_prueba <- test_30_copy %>% 
  mutate(pobre_lab = predict(logit_m4_prueba, newdata = test_30_copy, type = "raw")) %>% 
  select(id, pobre_lab)

head(predictSample_logit_prueba)

# Convertimos la etiqueta a formato binario
predictSample_logit_prueba <- predictSample_logit_prueba %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  select(id, pobre)

head(predictSample_logit_prueba)

# Comparación: unimos las predicciones con los valores reales del conjunto test_30
# Asumimos que test_30 tiene las columnas 'id' y 'Pobre'
resultados_comparacion <- test_30 %>% 
  select(id, Pobre) %>%
  inner_join(predictSample_logit_prueba, by = "id")

# Visualizamos el resultado de la unión (predicción vs. etiqueta real)
head(resultados_comparacion)

#compracion de resultados 

# Convertimos la columna de predicción (pobre, numérica) a factor con niveles "Si" y "No"
resultados_comparacion <- resultados_comparacion %>%
  mutate(
    # Si pobre es 1, la predicción es "Si"; si es 0, la predicción es "No"
    pred = factor(ifelse(pobre == 1, "Si", "No"), levels = c("Si", "No")),
    Pobre = factor(Pobre, levels = c("Si", "No"))
  )

# Ahora creamos la matriz de confusión utilizando la columna 'pred' para las predicciones
#library(caret)
conf_mat <- confusionMatrix(data = resultados_comparacion$pred,
                            reference = resultados_comparacion$Pobre)
print(conf_mat)

# calculo manualmente el número de predicciones correctas y la precisión
total_obs <- nrow(resultados_comparacion)
pred_correctas <- sum(resultados_comparacion$pred == resultados_comparacion$Pobre)
precision <- pred_correctas / total_obs

cat("Número de predicciones correctas:", pred_correctas, "\n")
cat("Precisión del modelo:", round(precision, 4), "\n")

###RESULTADO: Debido al desbalance de clase el resultado no es muy bueno

# ---------- ENVIO PARA KAGGLE LOGIT --------- # ----

#En este caso, logit_m4 destaca al combinar un rendimiento excelente (AUC de 0.93768 y F de 0.89689) con una menor complejidad (10 predictores) y menor variabilidad (FSD muy baja).
#Por lo tanto, basándonos en estos resultados, logit_m4 es una opción sólida para realizar predicciones en datos nuevos.

predictSample_logit <- test   %>% 
  mutate(pobre_lab = predict(logit_m4, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predictSample_logit)

predictSample_logit<- predictSample_logit %>% 
  mutate(pobre=ifelse(pobre_lab=="Si",1,0)) %>% 
  select(id,pobre)
head(predictSample_logit)    

setwd("D:/OneDrive - CGIAR/Pictures/Diplomado_BigData/Problem_set/ProblemSet2/uniandes-bdml-202510-ps-2")
template<-read.csv("sample_submission.csv")

head(template)

name <- "logit.csv"
write.csv(predictSample_logit, name, row.names = FALSE)



#Usamos nuestra base train dividida entre 70% train y 30% test
#usamos el paquete rpart, que implementa el algoritmo de árboles de decisión para regresión y clasificación.

p_load("rpart")
p_load("modeldata")


# ---------- MODELO DE ÁRBOL -------- # ---- 

#Cargar librerías 
require("pacman")
p_load(tidyverse, # tidy-data
       rpart, # Recursive Partition and Regression Trees (To run Trees)
       caret ,  # for model training and tunning
       rpart.plot, ## for trees graphs
       Metrics, ## Evaluation Metrics for ML
       ipred,  # For Bagging 
       ranger #For random Forest
)   

# ---------- ejercicio 1: usando especificacion del modelo 4------- # ---- 

#Partimos de la especificacion usada en el modelo 4 del logit

complex_tree_1 <- rpart(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                          recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
                        data    = train_70,
                        method = "class",
                        cp = 0  # complexity parameter, nuestro alpha
)

prp(complex_tree_1)

# Utilizamos la función prp del paquete rpart.plot para graficar el árbol de decisión
rpart.plot::prp(
  complex_tree_1,      
  under = TRUE,      # Mostrar la información debajo de cada nodo
  branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
  yesno = 2,         # Mostrar indicadores de "sí"/"no"
  faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
  varlen = 15,       # Longitud máxima para abreviar los nombres de variables
  box.palette = "-RdYlGn"  # Paleta de colores para las hojas 
)


minbucket_tree <- rpart::rpart(
  Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
    recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
  data = train_70, 
  method = "class", 
  minbucket = 15, # Numero minimo de obs en hojas
  cp = 0 # alpha para podar el arbol
) 

minbucket_tree 

prp(minbucket_tree, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")

# ---------- Poda del arbol ejercicio 1-------- # ----  

#usando alpha=0.01

arbol_1 <- rpart(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                   recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
                 data    = train_70,
                 method = "class" ## define tree for classification
)
arbol_1$control$cp # alpha

prp(arbol_1, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn") 


#Vamos a guardar la AUC para nuestro árbol:

pobreza<- ifelse(test_30$Pobre=="Si",1,0) #Volver pobre en test  numérico
pred_prob_1 <- predict(arbol_1, newdata = test_30, type = "prob")    ## Predecir la probabilidad (en lugar de la clase)
aucval_arbol_1 <- Metrics::auc(actual = pobreza,predicted = pred_prob_1[,2]) #calcular el AUC

aucval_arbol_1

#usaremos caret maximizando el AUC.

fiveStats <- function(...) {
  c(
    twoClassSummary(...),
    defaultSummary(...)
  )
}
## Para usar ROC) (u otras más) para tuning
#

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, # nuestra función 
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)


# especificamos la grilla de los alphas
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

cv_tree_1 <- train(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                     recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer,
                   data = train_70,
                   method = "rpart", 
                   trControl = ctrl, 
                   tuneGrid = grid, 
                   metric= "ROC"
)
cv_tree_1

#Veamos el valor del α que maximiza el AUC

cv_tree_1$bestTune$cp

# ---------- arbol final del ejercicio 1-------- # ---- 

prp(cv_tree_1$finalModel, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")


# calculemos el AUC sobre los datos de prueba:  

pred_prob <- predict(cv_tree_1, newdata = test_30, type = "prob")   


aucval_cvtree_1 <- Metrics::auc(actual = pobreza,predicted = pred_prob[,2])
aucval_cvtree_1

# Eliminamos la variable Pobre ya que el modelo se entrena sin ella.
test_30_copy <- dplyr::select(test_30, -Pobre)

# Aquí usamos type = "prob" para obtener la probabilidad de cada clase y 
# convertimos a etiqueta "Si"/"No" usando 0.5 como umbral.
predictSample_CART <- test_30_copy %>% 
  mutate(pobre_lab = ifelse(
    predict(cv_tree_1, newdata = test_30_copy, type = "prob")[,2] >= 0.5,
    "Si",
    "No"
  )) %>% 
  dplyr::select(id, pobre_lab)

# Verificamos las primeras predicciones
head(predictSample_CART)

# Convertimos la etiqueta (pobre_lab) a formato binario (1 para "Si", 0 para "No"),
predictSample_CART <- predictSample_CART %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  select(id, pobre)

head(predictSample_CART)

# Convertimos la etiqueta (pobre_lab) a formato binario (1 para "Si", 0 para "No"),
# igual a como lo hiciste en el ejercicio con logit.
predictSample_CART <- predictSample_CART %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  dplyr::select(id, pobre)

head(predictSample_CART)


# Comparamos las predicciones obtenidas con los datos reales.
resultados_comparacion_CART <- test_30 %>% 
  dplyr::select(id, Pobre) %>%
  inner_join(predictSample_CART, by = "id")

head(resultados_comparacion_CART)

# Para evaluar, convertimos la variable predicha (pobre, numérica) a factor con niveles "Si" y "No"
resultados_comparacion_CART <- resultados_comparacion_CART %>%
  mutate(
    pred = factor(ifelse(pobre == 1, "Si", "No"), levels = c("Si", "No")),
    Pobre = factor(Pobre, levels = c("Si", "No"))
  )

# Calculamos la matriz de confusión utilizando el paquete caret
library(caret)
conf_mat_CART <- confusionMatrix(data = resultados_comparacion_CART$pred,
                                 reference = resultados_comparacion_CART$Pobre)
print(conf_mat_CART)


#calculo manual de las metricas de la matriz de confusion 

# Extraer la tabla de la matriz de confusión
cm <- conf_mat_CART$table

# Suponemos que:
#   - cm["Si", "Si"] representa los Verdaderos Positivos (TP)
#   - cm["Si", "No"] representa los Falsos Positivos (FP)
#   - cm["No", "Si"] representa los Falsos Negativos (FN)
#   - cm["No", "No"] representa los Verdaderos Negativos (TN)
TP <- cm["Si", "Si"]
FP <- cm["Si", "No"]
FN <- cm["No", "Si"]
TN <- cm["No", "No"]

total <- TP + FP + FN + TN  # Total de observaciones

# Calcular las métricas

# Accuracy: Proporción de predicciones correctas
Accuracy <- (TP + TN) / total

# Recall o Sensitivity: Proporción de verdaderos positivos correctamente identificados.
Recall <- TP / (TP + FN)
Sensitivity <- Recall  # Es lo mismo

# Specificity: Proporción de verdaderos negativos correctamente identificados.
Specificity <- TN / (TN + FP)

# Positive Predictive Value (PPV) y Precision: Proporción de predicciones positivas que son correctas.
PPV <- TP / (TP + FP)
Precision <- PPV

# Negative Predictive Value (NPV): Proporción de predicciones negativas que son correctas.
NPV <- TN / (TN + FN)

# F1 score: Media armónica de Precision y Recall.
F1 <- 2 * (Precision * Recall) / (Precision + Recall)

# Prevalence: Proporción de casos reales positivos en el conjunto.
Prevalence <- (TP + FN) / total

# Detection rate: Proporción de verdaderos positivos en relación al total.
Detection_rate <- TP / total

# Detection prevalence: Proporción de observaciones clasificadas como positivas (TP + FP) en relación al total.
Detection_prevalence <- (TP + FP) / total

# Balanced accuracy: Promedio de Sensitivity y Specificity.
Balanced_accuracy <- (Sensitivity + Specificity) / 2

# Crear un data frame con los resultados para visualizarlos mejor
metrics <- data.frame(
  Metric = c("Accuracy", "Recall/Sensitivity", "Specificity", 
             "Positive Predictive Value (Precision)",
             "Negative Predictive Value", "F1 score", "Prevalence",
             "Detection rate", "Detection prevalence", "Balanced accuracy"),
  Value = c(Accuracy, Recall, Specificity, PPV, NPV, F1, Prevalence, Detection_rate, Detection_prevalence, Balanced_accuracy)
)

print(metrics)


#Partimos de la especificacion usada en el modelo 4 del logit

complex_tree_kgg <- rpart(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                            recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
                          data    = train,
                          method = "class",
                          cp = 0  # complexity parameter, nuestro alpha
)

prp(complex_tree_kgg)

# Utilizamos la función prp del paquete rpart.plot para graficar el árbol de decisión
rpart.plot::prp(
  complex_tree_kgg,      
  under = TRUE,      # Mostrar la información debajo de cada nodo
  branch.lty = 2,    # Tipo de línea para las ramas (2 = línea punteada)
  yesno = 2,         # Mostrar indicadores de "sí"/"no"
  faclen = 0,        # Longitud de la abreviación para niveles de factores (0 = sin abreviación)
  varlen = 10,       # Longitud máxima para abreviar los nombres de variables
  box.palette = "-RdYlGn"  # Paleta de colores para las hojas 
)


minbucket_tree_kgg <- rpart::rpart(
  Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
    recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
  data = train, 
  method = "class", 
  minbucket = 15, # Numero minimo de obs en hojas
  cp = 0 # alpha para podar el arbol
) 

minbucket_tree_kgg 

prp(minbucket_tree_kgg, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")

# ---------- Poda del arbol para kaggle-------- # ----  

#usando alpha=0.01

arbol_kgg <- rpart(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                     recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer, 
                   data    = train,
                   method = "class" ## define tree for classification
)
arbol_kgg$control$cp # alpha

prp(arbol_kgg, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn") 


#usaremos caret maximizando el AUC.

fiveStats <- function(...) {
  c(
    twoClassSummary(...),
    defaultSummary(...)
  )
}
## Para usar ROC) (u otras más) para tuning
#

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, # nuestra función 
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)


# especificamos la grilla de los alphas
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

cv_tree_kgg <- train(Pobre~t_dependencia + clima_educ + edad_jefe + nocupados + 
                       recibe_ayuda_ + arrienda + salud_jefe + ocup_jefe_informal + P5010 + H_Head_mujer,
                     data = train,
                     method = "rpart", 
                     trControl = ctrl, 
                     tuneGrid = grid, 
                     metric= "ROC"
)
cv_tree_kgg

#Veamos el valor del α que maximiza el AUC

cv_tree_kgg$bestTune$cp

# ---------- arbol final del ejercicio para kaggle-------- # ---- 

prp(cv_tree_kgg$finalModel, under = TRUE, branch.lty = 2, yesno = 2, faclen = 0, varlen=15,box.palette = "-RdYlGn")


# Aquí usamos type = "prob" para obtener la probabilidad de cada clase y 
# convertimos a etiqueta "Si"/"No" usando 0.5 como umbral.
predictSample_CART_kgg <- test %>% 
  mutate(pobre_lab = ifelse(
    predict(cv_tree_kgg, newdata = test, type = "prob")[,2] >= 0.5,
    "Si",
    "No"
  )) %>% 
  dplyr::select(id, pobre_lab)

# Verificamos las primeras predicciones
head(predictSample_CART_kgg)

# Convertimos la etiqueta (pobre_lab) a formato binario (1 para "Si", 0 para "No"),
predictSample_CART_kgg <- predictSample_CART_kgg %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  dplyr::select(id, pobre_lab)

head(predictSample_CART_kgg)

# Convertimos la etiqueta (pobre_lab) a formato binario (1 para "Si", 0 para "No"),
# igual a como lo hiciste en el ejercicio con logit.
predictSample_CART_kgg <- predictSample_CART_kgg %>% 
  mutate(pobre = ifelse(pobre_lab == "Si", 1, 0)) %>% 
  dplyr::select(id, pobre)

head(predictSample_CART_kgg)

name <- "CART_alpha0.csv"
write.csv(predictSample_CART_kgg, name, row.names = FALSE)




# Pobre~arrienda + ocup_jefe_informal +H_Head_mujer+n_sin_educacion+nmenores+Nper
# Pobre~edad_jefe+H_Head_mujer+Nper+ ocup_jefe_informal*H_Head_mujer+ H_Head_Educ_level*H_Head_mujer + clima_educ + arrienda +p5010

