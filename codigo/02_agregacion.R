library(tidyverse)
library(magrittr)
library(R2jags)



# Población -----------------------------------------------------

pob <- read.csv('datos/Proyeccion de Poblacion/Población por Municipio 2005 - 2011.csv', )
pob %<>% 
  select(ESTADO = cve_ent, estado = nom_ent, cve_mun, contains('pob_ent')) %>% 
  mutate(ESTADO = as.character(ESTADO)) %>% 
  gather(AÑO, poblacion, contains('pob_ent')) %>% 
  mutate(AÑO = parse_number(str_remove(AÑO, '\\.'))) %>% 
  group_by(ESTADO, estado, AÑO) %>% 
  summarise(poblacion = sum(poblacion)) %>% 
  ungroup()

pob


# Narco ---------------------------------------------------------

narco <- read.csv('datos/limpios.csv')

narco %>% names
narco %>% str

narco %<>% mutate(FPE = is.na(FPE)) 

narco_mensual <- narco %>%
  filter(AÑO != 2006) %>% 
  group_by(ESTADO, AÑO, MES) %>% 
  summarise_at(vars(EJF:cartel_otro), ~sum(., na.rm = T)) %>% 
  ungroup() %>% 
  mutate(ESTADO = as.character(ESTADO))

narco_mensual %>% count(ESTADO) %>% print(n=Inf)
narco_mensual %>% head

narco_mensual %<>% left_join(pob) %>% unique()
narco_mensual %<>% mutate(tasa = EJ / poblacion * 1000000)
narco_mensual$tasa %>% summary

# EDA -----------------------------------------------------------

# conteos por estado 

estados <- narco_mensual %>% 
  select(-AÑO, -MES) %>% 
  group_by(ESTADO, estado) %>% 
  summarise_all(~sum(., na.rm = T)) %>% 
  ungroup() %>% 
  select(-c(CORR, DRO, DTRA, EJM, EJF, ELE, FCRU, PRE, TAX, VEH, tasa))

estados %>% summary

plot_estados <- function(df, variable) {
  
  df %>% 
    gather(var, val, -ESTADO, -estado) %>% 
    filter(var == variable) %>% 
    ggplot(aes(reorder(estado, val), val)) +
    geom_col(aes(fill = factor(estado))) +
    labs(title = variable, x='', y='') +
    coord_flip() +
    theme_light() +
    theme(legend.position = 'none')
  
  ggsave(paste0('plots/',
                variable, '.png'),
         width = 6, height = 4)
  
}

map(names(estados)[-c(1,2)], ~plot_estados(estados, .))

narco_mensual %>% 
  mutate(fecha = as.Date(paste(AÑO, str_pad(MES, 2, 'left', '0'), '01', sep = '-'))) %>% 
  gather(var, val, -estado_num, -estado, -AÑO, -MES, -fecha) %>% 
  filter(var == 'm_decapitado') %>% 
  ggplot(aes(fecha, val)) +
  geom_line(aes(color = factor(estado))) +
  # labs(title = variable, x='', y='') +
  theme_light() +
  theme(legend.position = 'none')

narco_mensual %<>% 
  select(estado_num = ESTADO, estado, AÑO, MES, poblacion, EJ, tasa,
         everything())


write.csv(narco_mensual, 'datos/narco_mensual.csv', row.names = F)
