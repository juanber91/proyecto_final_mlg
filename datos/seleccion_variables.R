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

estados <- base %>% select(estado) %>% unique() %>% as.character()
mes <- base %>% select(MES) %>% unique() %>% as.character()

for (i in estados){
  for (j in mes){
    for (anio in seq(2007, 2011)){
      sub <- base %>% filter(MES == j & estado == i & AÑO == anio)
      if (dim(sub)[1] == 0) {
        reg <- base[1,]
        reg$AÑO <- anio
        reg$estado <- i
        reg$tasa <- 0
        reg$MES <- j
        reg[,5:15] <- 0
        base <- rbind(base, reg)
      }
    }
  }
}

base %>% write.csv("narco_modelo.csv")
