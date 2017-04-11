
# Problema ----------------------------------------------------------------

# Simular un conjunto de datos que tengan la siguiente estructura:
# yi ~ N(mui, sigma2)
# mui = -4 + 5 x
# sigma2 = 9
# x ~ Poisson(lambda=5)


# Simulacion de los datos -------------------------------------------------
n <- 4  # Aqui se coloca el numero de observaciones deseadas
x <- rpois(n=n, lambda=5)
media <- -4 + 5 * x
varia <- 9
y <- rnorm(n=n, mean=media, sd=sqrt(varia))

# Diagrama de dispersion --------------------------------------------------
plot(x=x, y=y, pch=19)

