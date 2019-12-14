#### LIBRERÍAS ###
library(tidyverse)

#### DATOS #####
setwd("~/Documentos/RgrAvn/ProyectoFinal")
base <- read.csv("limpios.csv")
pob_2005<-read.csv("pob_municipios_2005.csv") %>%
  select(c('entidad', 'nom_ent', 'mun', 'nom_mun', 'p_total', 'prop_pob_mun'))
names(pob_2005) <- c('cve_ent', 'nom_ent', 'cve_mun', 'mun', 'pob_ent_2005', 'prop_pob_ent_2005')
  
pob_2010<-read.csv("pob_municipios_2010.csv") %>% 
  select(c('entidad', 'nom_ent', 'mun', 'nom_mun', 'pobtot', 'prop_pob_mun'))
names(pob_2010) <- c('cve_ent', 'nom_ent', 'cve_mun', 'mun', 'pob_ent_2010', 'prop_pob_ent_2010')

#### PROCESANDO POBLACIÍON MUNICIPAL ####
pob <- pob_2005 %>% merge(.,pob_2010, c("cve_ent", 'nom_ent', 'cve_mun', 'mun'))

### EVOLUCIÓN DE LA TASA DE POBLACIÓN POR MUNICIPIO ####
dif_perc <- pob$prop_pob_ent_2005 - pob$prop_pob_ent_2010
dif_pob <- pob$pob_ent_2010 * pob$prop_pob_ent_2010 - pob$pob_ent_2005 * pob$prop_pob_ent_2005
hist(dif_perc, breaks = 50, xlim = c(-0.01, 0.01))
hist(dif_pob, breaks = 100, xlim = c(-15000, 15000))
rm(pob_2005, pob_2010)

### BASE FINAL DE POBLACION POR MUNICIPIO POR AÑO ###
pob['prop_pob_mun'] <- (pob$prop_pob_ent_2005+pob$prop_pob_ent_2010)/2

slope <- (pob$pob_ent_2010 -pob$pob_ent_2005)/5
interc <- pob$pob_ent_2005 - slope * 2005

pob <- select(pob, c('cve_ent', 'nom_ent', 'cve_mun', 'mun', 'prop_pob_mun'))

for ( i in 0:6){
  pob[paste('pob_ent_', as.character(2005+i), by='')] <- round(interc + slope * (2005+i))
}

pob %>% write.csv('Población por Municipio 2005 - 2011.csv')
