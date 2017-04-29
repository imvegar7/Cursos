
# En este script se va a replicar el ejemplo sobre entrega de refrescos

# -------------------------------------------------------------------------
# Lectura de la base de datos ---------------------------------------------
# -------------------------------------------------------------------------

# Los datos estan disponibles en el objeto softdrink del paquete MPV
require(MPV)
softdrink
datos <- softdrink
colnames(datos) <- c('tiempo', 'cantidad', 'distancia')
attach(datos)


# -------------------------------------------------------------------------
# Ajustando el modelo y agregando el plano --------------------------------
# -------------------------------------------------------------------------

# Ajustando el modelo de regresion
mod <- lm(tiempo ~ cantidad + distancia)
summary(mod)

# -------------------------------------------------------------------------
# Residuales --------------------------------------------------------------
# -------------------------------------------------------------------------

par(mfrow=c(2, 2))
plot(mod, pch=19, col='purple')
