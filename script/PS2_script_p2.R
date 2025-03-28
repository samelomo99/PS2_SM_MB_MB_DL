

#-------------------------------------------------------------------# ----
## --------------- Problem Set 2: Predicting Poverty -------------- ##
## - Santiago Melo - Miguel Blanco - María Bernal - Diana Lopera - ##
#-------------------------------------------------------------------#


# ----------------------LIBRERIAS----------------------------------- # ----

library(tidyverse)
library(skimr)  # For quick summaries
library(naniar) # For missing data

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



# ----------------------MODELOS------------------------------------- # ----