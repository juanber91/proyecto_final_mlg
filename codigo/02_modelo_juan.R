library(tidyverse)
library(R2jags)

narco <- read.csv('Final/proyecto_final_mlg/datos/limpios.csv')

narco_mod <- narco %>% 
  select(ejecutados = EJ,
         estado = ESTADO,
         anio = AÑO)

narco_mod %>% head

datos <- list(
  "n" = dim(narco_mod)[1],
  "ejecutados" = narco_mod$ejecutados,
  "estado" = narco_mod$estado,
  "anio" = narco_mod$anio - 2005
)

inits <- function() {
  list(beta = rep(0,32),
       gamma = rep(0,6))
}

parameters <- c('alpha.est', "beta.est", "gamma.est", "yf")

# JAGS
mod_jags <- jags(datos, inits, parameters, 
                 model.file = "Final/proyecto_final_mlg/codigo/modelo_juan.jags",
                 n.iter = 1000, n.chains = 2, 
                 n.burnin = 100, n.thin = 1)

saveRDS(mod_jags, 'Final/proyecto_final_mlg/codigo/modelo_juan1.RDS')
save(mod_jags, file = 'Final/proyecto_final_mlg/codigo/modelo_juan1.RData')

out <- mod_jags$BUGSoutput$sims.list

# Resumen
out.sum <- mod_jags$BUGSoutput$summary
# out.sum.t <- out.sum[grep("beta|deviance", rownames(out.sum)), c(1, 3, 7)]
print(out.sum)

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
R2 <- (cor(narco_mod$ejecutados, out.yf[, 1])) ^ 2
print(R2)