library(tidyverse)
library(magrittr)
library(R2jags)

narco1 <- read.csv('datos/narco_modelo.csv') %>% 
  mutate(fecha = as.Date(paste(AÑO, 
                               str_pad(MES, 2, 'left', '0'), 
                               '01', sep = '-'))) %>% 
  select(-X) %>% 
  filter(fecha != '2011-12-01') %>%
  # filter(tasa > 0) %>% 
  # filter(estado %in% c('Chihuahua', 'Sinaloa', 'Guerrero',
  #                      'Durango', 'Baja California', 'México',
  #                      'Michoacán de Ocampo', 'Jalisco', 'Nuevo León',
  #                      'Tamaulipas', 'Sonora', 'Coahuila de Zaragoza')) %>% 
  # mutate(estado2 = as.numeric(as.factor(as.character(estado)))) %>% 
  mutate(estado2 = as.numeric(estado)) %>% 
  na.omit() 

narco1 %>% count(estado2)
narco1 %>% head

narco <- narco1 %>%
  mutate_at(vars(TAX:WEAPON), ~log(. + 1/1000)) %>% 
  mutate(tasa = log(tasa + 1 + 1/1000))

datos <- list(
  "n" = dim(narco)[1],
  'tasa' = narco$tasa+ 1/1000000,
  'TAX' = narco$TAX,
  'm_arma_de_fuego' = narco$m_arma_de_fuego,
  'm_decapitado' = narco$m_decapitado,
  'm_mutilado' = narco$m_mutiliado,
  'mens_narco' = narco$mens_narco,
  'cartel_juarez' = narco$cartel_juarez,
  'cartel_beltranleyva' = narco$cartel_beltranleyva,
  'cartel_sinaloa' = narco$cartel_sinaloa,
  'cartel_familia_michoacana' = narco$cartel_familia_michoacana,
  'cartel_zetas' = narco$cartel_zetas,
  'cartel_otro' = narco$cartel_otro,
  'estado' = narco$estado2,
  'anio' = narco$AÑO - 2006
)

inits <- function() {
  list(beta = rep(1,12),
       gamma = rep(1,32),
       delta = rep(1,5))
}

parameters <- c('beta', 'gamma', 'delta', 'yf')

# JAGS
mod_jags <- jags(datos, parameters.to.save = parameters, 
                 model.file = "codigo/02_modelo_juan_chido.jags",
                 n.iter = 1000, n.chains = 3, 
                 n.burnin = 100, n.thin = 1)

save(mod_jags, file = 'codigo/modelo_juan2.RData')
beepr::beep(2)

R2jags::traceplot(mod_jags, ask = F, mfrow = c(3,4), varname = 'beta')
par(mfrow = c(1,1))

out <- mod_jags$BUGSoutput$sims.list

# Resumen
# out.sum <- mod_jags$BUGSoutput$summary
# out.sum.t <- out.sum[grep("beta", rownames(out.sum)), ]
# out.sum.t <- out.sum[grep("gamma", rownames(out.sum)), ]
# out.sum.t <- out.sum[grep("delta", rownames(out.sum)), ]
print(out.sum.t)

# DIC
out.dic <- mod_jags$BUGSoutput$DIC
print(out.dic)

# Predicciones
out.yf <- out.sum[grep("yf", rownames(out.sum)), ]
# Psuedo R²
# R2 <- (cor(exp(narco$tasa), exp(out.yf[, 1]))) ^ 2
R2 <- (cor(narco$tasa, out.yf[, 1])) ^ 2
print(R2)


narco_preds <- narco1 %>%  
  mutate(pred = exp(out.yf[, 1])-1) %>% 
  mutate(fecha = as.Date(paste(AÑO, str_pad(MES, 2, 'left', '0'), '01', sep = '-'))) 

# data.frame(obs = exp(narco$tasa), pred = exp(out.yf[, 1])-1) 

narco_preds %>% 
  # filter(pred <= 2) %>%
  ggplot() +
  geom_abline() +
  geom_point(aes(tasa, pred)) +
  theme_light()

narco_preds %>% 
  # filter(estado == 'Nuevo León') %>% 
  filter(estado == 'Chihuahua') %>%
  ggplot() +
  geom_line(aes(fecha, tasa), color = 'black', size = 1) +
  # geom_line(aes(fecha, pred), color = 'red', size = 1) +
  theme_light()
