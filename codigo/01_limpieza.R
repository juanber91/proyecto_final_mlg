library('tidyverse')
library('magrittr')

narco <- read.csv('datos/completa/EJECUCIONES (2006-2011).csv',
                  sep = ';', encoding = 'latin1') %>% 
  mutate(AÑO_ID = as.character(AÑO_ID))

narco %>% str

# narco %<>% mutate(AÑO_ID = paste0(AÑO_ID, '_', YEAR))

# narco[narco==9999] <- NA


# Tablas externas ----_------------------------------------------

tabla1 <- readxl::read_excel('datos/completa/tabla1-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'VEHICLE')) %>% 
  mutate(aux = 1) %>% 
  filter(VEHICLE != 8) %>% 
  spread(VEHICLE, aux, 0) %>% 
  setNames(c('AÑO_ID', 'v_automovil', 'v_camion', 'v_camioneta', 'v_jeep', 'v_moto', 'v_patrulla', 'v_tanque'))


tabla2 <- readxl::read_excel('datos/completa/tabla2-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'DRUGS')) %>% 
  mutate(DRUGS = ifelse(DRUGS %in% c(1,4), DRUGS, 2)) %>%
  count(AÑO_ID, DRUGS) %>% 
  mutate(n = 1) %>% 
  spread(DRUGS, n, 0) %>% 
  setNames(c('AÑO_ID', 'd_cocaina', 'd_otras_drogas', 'd_marihuana'))

tabla3 <- readxl::read_excel('datos/completa/tabla3-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'AGE_RANGE')) %>% 
  filter(AGE_RANGE != 9999) %>% 
  mutate(aux = 1) %>% 
  spread(AGE_RANGE, aux, 0) %>% 
  setNames(c('AÑO_ID', 'e_15_o_menos', 'e_15_a_20', 'e_20_a_30', 'e_30_a_50', 'e_50_o_mas'))


tabla42 <- readxl::read_excel('datos/completa/tabla4-EJECUCIONES.xlsx')  %>% 
  setNames(c('AÑO_ID', 'WEAPON')) %>% 
  count(AÑO_ID) %>% 
  mutate(n = 1) %>% 
  rename(WEAPON = n)

# tabla5 <- readxl::read_excel('datos/2010/table5-A-X10.xlsx') %>% 
#   setNames(c('AÑO_ID', 'CALIBER'))

tabla6 <- readxl::read_excel('datos/completa/tabla6-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'MODALITY')) %>% 
  mutate(MODALITY = ifelse(MODALITY %in% c(1,4,11,5,15,8,20,6,12,14),
                           MODALITY, 9999)) %>% 
  count(AÑO_ID, MODALITY) %>% 
  spread(MODALITY, n, 0) %>% 
  setNames(c('AÑO_ID', 'm_arma_de_fuego', 'm_decapitado',
             'm_mutiliado', 'm_encobijado', 'm_asfixiado', 'm_calcinado',
             'm_foza', 'm_golpes', 'm_tortura', 'm_vehiculo', 'm_otra'))

tabla7 <- readxl::read_excel('datos/completa/tabla7-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'MESSAGE_TYPE')) %>% 
  mutate(MESSAGE_TYPE = ifelse(MESSAGE_TYPE %in% 4:5, 'mens_narco',
                                ifelse(MESSAGE_TYPE == 1, 'mens_gobierno',
                                       ifelse(MESSAGE_TYPE %in% c(2,3,6,9998), 'mens_otro', NA)))) %>% 
  filter(!is.na(MESSAGE_TYPE)) %>% 
  count(AÑO_ID, MESSAGE_TYPE) %>% 
  spread(MESSAGE_TYPE, n, 0)

tabla8 <- readxl::read_excel('datos/completa/tabla8-EJECUCIONES.xlsx') %>% 
  setNames(c('AÑO_ID', 'CARTEL')) %>% 
  mutate(CARTEL = ifelse(CARTEL %in% c(20,14,190,18,86),
                           CARTEL, 9999)) %>% 
  count(AÑO_ID, CARTEL) %>% 
  spread(CARTEL, n, 0) %>% 
  setNames(c('AÑO_ID', 'cartel_juarez', 'cartel_beltranleyva',
             'cartel_sinaloa', 'cartel_familia_michoacana',
             'cartel_zetas', 'cartel_otro'))



narco_full <- narco %>% 
  left_join(tabla1) %>% 
  left_join(tabla2) %>% 
  left_join(tabla3) %>% 
  left_join(tabla4) %>% 
  left_join(tabla6) %>% 
  left_join(tabla7) %>% 
  left_join(tabla8)

narco_full[is.na(narco_full)] <- 0
narco_full[narco_full == 9999] <- NA

data.frame(colSums(is.na(narco_full)))
write.csv(narco_full, 'datos/limpios.csv')
