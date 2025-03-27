

#-------------------------------------------------------------------# ----
## --------------- Problem Set 2: Predicting Poverty -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#


# ----------------------LIBRERIAS----------------------------------- # ----


# ----------------------DATOS--------------------------------------- # ----


# Definir rutas

setwd("D:/OneDrive - CGIAR/Pictures/Diplomado_BigData/Repositorios/PS2_SM_MB_MB_DL")
repo_path <- "D:/OneDrive - CGIAR/Pictures/Diplomado_BigData/Repositorios/PS2_SM_MB_MB_DL"
datos_path <- file.path(repo_path, "datos")

# Crear carpeta "datos" si no existe
if (!dir.exists(datos_path)) {
  dir.create(datos_path, recursive = TRUE)
}


# ----------------------DESCRIPTIVAS-------------------------------- # ----



# ----------------------MODELOS------------------------------------- # ----