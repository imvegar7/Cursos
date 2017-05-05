
# Diferencia entre polinomios no ortogonales y ortogonales ----------------
x <- c(5, 3, 6, 1, 5, 8, 5, 6, 5)  # Una variable cualquiera

mat1 <- model.matrix( ~ x + I(x^2) + I(x^3))
mat2 <- model.matrix( ~ poly(x, degree=3))

# Matrices de diseño
mat1
mat2

# Matriz de correlaciones
round(cor(mat1[, -1]), digits=2)
round(cor(mat2[, -1]), digits=2)


# Dibujando los dos tipos de polinomios -----------------------------------
poten <- function(x, k) x ^ k

par(mfrow=c(1, 2))
curve(poten(x, k=1), from=-3, to=3, ylim=c(-10, 15),
      main='Polinomios usuales', col=1, ylab='', lwd=2)
for (i in 2:5) curve(poten(x, k=i), from=-3, to=3, add=T, col=i, lwd=2)

legend('bottomright', legend=1:5, col=1:5, lwd=2, bty='n')

xx <- seq(from=-3, to=3, by=0.1)
dt <- poly(xx, degree=5)
plot(x=xx, y=dt[, 1], type='l', col=1, lwd=2,
     ylab='', xlab='x', main='Polinomios ortogonales')
for (i in 2:5) lines(x=xx, y=dt[, i], type='l', col=i, lwd=2)


# Ejemplo de regresión con polinomio ortogonal ----------------------------

n <- 100
x <- runif(n=n, min=0, max=30)
y <- rnorm(n=n, mean=-3-2*x+1*x^2, sd=60)


# Ajustando los modelos
mod1 <- lm(y ~ x + I(x^2))         # Polinomio NO ortogonal
mod2 <- lm(y ~ poly(x, degree=2))  # Polinomio si ortogonal

# comparando los resultados
summary(mod1)
summary(mod2)

# Agregando las líneas ajustadas
par(mfrow=c(1, 1))
plot(x, y)
lines(x=sort(x), y=fitted.values(mod1)[order(x)], col='blue', lwd=6)
lines(x=sort(x), y=fitted.values(mod2)[order(x)], col='red3', lwd=2)
legend('topleft', legend=c('Ajuste polinomial', 'Ajuste ortogonal'),
       lwd=c(6, 2), col=c('blue', 'red3'))

# Estimando y para x = 20
c(1, 20, 400) %*% coef(mod1) # usando mod1
c(1, 20, 400) %*% coef(mod2) # usando mod2. Error!!!

new.data <- data.frame(x=20)
predict(mod2, new.data)      # Ahora si el valor correcto



