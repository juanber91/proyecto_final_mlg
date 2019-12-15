library(tidyverse)
library(R2jags)

narco <- read.csv('datos/narco_modelo.csv') %>% 
  select(-X) %>% 
  mutate(estado2 = as.numeric(estado)) %>% 
  na.omit()

narco %>% head

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
                 n.iter = 5000, n.chains = 3, 
                 n.burnin = 500, n.thin = 1)

R2jags::traceplot(mod_jags, ask = F, mfrow = c(3,4), varname = 'beta')

# saveRDS(mod_jags, 'Final/proyecto_final_mlg/codigo/modelo_juan1.RDS')
# save(mod_jags, file = 'Final/proyecto_final_mlg/codigo/modelo_juan1.RData')

out <- mod_jags$BUGSoutput$sims.list

# Resumen
out.sum <- mod_jags$BUGSoutput$summary
out.sum.t <- out.sum[grep("beta", rownames(out.sum)), ]
out.sum.t <- out.sum[grep("gamma", rownames(out.sum)), ]
out.sum.t <- out.sum[grep("delta", rownames(out.sum)), ]
print(out.sum.t)

# DIC
out.dic <- mod_jags$BUGSoutput$DIC
print(out.dic)

# Predicciones
out.yf <- out.sum[grep("yf", rownames(out.sum)), ]
or <- order(calif$MO)
ymin <- min(calif$SP, out.yf[, c(1, 3, 7)])
ymax <- max(calif$SP, out.yf[, c(1, 3, 7)])
par(mfrow = c(1, 1))
plot(calif$MO, calif$SP, ylim = c(ymin, ymax))
lines(calif$MO[or], out.yf[or, 1], lwd = 2, col = 2)
lines(calif$MO[or], out.yf[or, 3], lty = 2, col = 2)
lines(calif$MO[or], out.yf[or, 7], lty = 2, col = 2)

# Psuedo R²
R2 <- (cor(narco$tasa, out.yf[, 1])) ^ 2
print(R2)
