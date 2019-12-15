####  LIBRERIAS ####
library(tidyverse)

### DATOS ####
setwd("~/Documentos/RgrAvn/ProyectoFinal/proyecto_final_mlg/datos/")
base <- read.csv("narco_mensual.csv")

### Procesamiento ####
vars <- c("estado", "AÑO", "tasa", "MES", "TAX", "m_arma_de_fuego", "m_decapitado", "m_mutiliado", 
          "mens_narco", "cartel_juarez", "cartel_beltranleyva", "cartel_sinaloa",
          "cartel_familia_michoacana", "cartel_zetas", "cartel_otro")

base <- base %>% select(vars)

base %>% write.csv("narco_modelo.csv")
