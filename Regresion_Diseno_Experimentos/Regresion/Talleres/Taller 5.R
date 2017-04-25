# Para cargar los datos
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)
obs.rara <- which.max(datos$alcobas)
datos <- datos[-obs.rara, ]
attach(datos)

# Punto 1
library(scatterplot3d)
scatterplot3d(x=mt2, y=alcobas, z=precio, pch=16,
              cex.lab=1.5, highlight.3d=TRUE, type="h")

library(rgl)
plot3d(x=mt2, y=alcobas, z=precio,
       main='Mueva con el mouse la figura')


# Punto 2
require(gamlss)
mod1 <- gamlss(precio ~ mt2 + alcobas, sigma.fo= ~ mt2 + alcobas,
              data=datos)

summary(mod1)

mod2 <- gamlss(precio ~ mt2 + alcobas, sigma.fo= ~ mt2,
              data=datos)

summary(mod2)
