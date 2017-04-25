# Para cargar los datos
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)

# Punto 1
require(gamlss)
mod <- gamlss(precio ~ mt2, sigma.fo= ~ mt2,
              family=NO2, data=datos)

summary(mod)

# Punto 2
# Si, el area es importante.

# Punto 3
mu.est <- function(x) sum(coef(mod, 'mu') * c(1, x))
mu.est <- Vectorize(mu.est) # Esto para vectorizar la funcion

si.est <- function(x) exp(sum(coef(mod, 'sigma') * c(1, x)))
si.est <- Vectorize(si.est) # Esto para vectorizar la funcion

par(mfrow=c(1, 2))
curve(expr=mu.est, from=26, to=500, lwd=4, col='gold',
      ylab=expression(hat(mu)), xlab='Area')
grid()
curve(expr=si.est, from=26, to=500, lwd=4, col='tomato',
      ylab=expression(hat(sigma)), xlab='Area')
grid()

# Punto 4
newdata <- data.frame(mt2=250)
predict(object=mod, what='mu', newdata=newdata)

# Punto 5
predict(object=mod, what='sigma', newdata=newdata)



