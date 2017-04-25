
# Leyendo la base
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)

# Punto 1
dim(datos)

# Punto 2
str(datos)

# Punto 3
with(datos,
     plot(x=mt2, y=precio, pch=20, col='green4',
          xlab='Área (m2)', ylab='Precio (millones $)'))

# Punto 4
with(datos, cor(mt2, precio))
text(x=100, y=1500, expression(rho==0.85), cex=2)

# Punto 5
mod <- lm(precio ~ mt2, data=datos)
summary(mod)

# Punto 6
# Se escribe a partir de la tabla de resumen

# Punto 7
coef(mod)[2]

# Punto 8
table(datos$ubicacion)

# Punto 9
subdatos <- subset(datos, ubicacion %in% c('poblado', 'laureles'),
                   drop=TRUE)
with(subdatos, plot(x=mt2, y=precio, type='n',
                    xlab='Área (m2)', ylab='Precio (millones $)'))
with(subset(subdatos, ubicacion == 'laureles'),
            points(x=mt2, y=precio, pch=20, col='red'))
with(subset(subdatos, ubicacion == 'poblado'),
     points(x=mt2, y=precio, col='blue'))
legend('topleft', legend=c('Laureles', 'Poblado'),
       col=c('red', 'blue'), pch=c(20, 1))

# Punto 10
with(subset(subdatos, ubicacion == 'laureles'), cor(mt2, precio))
with(subset(subdatos, ubicacion == 'poblado'), cor(mt2, precio))

text(x=200, y=1200, expression(rho==0.79), col='blue')
text(x=300, y=200, expression(rho==0.82), col='red')

# Punto 11
modlau <- lm(precio ~ mt2, data=datos, subset=ubicacion == 'laureles')
modpob <- lm(precio ~ mt2, data=datos, subset=ubicacion == 'poblado')
summary(modlau)
summary(modpob)


