
# Punto 1
n <- 100
x <- rpois(n=n, lambda=5)
media <- 36 - 12 * x + 1 * x ^ 2
desvi <- 3
y <- rnorm(n=n, mean=media, sd=desvi)

plot(x=x, y=y)

# Punto 2
mod1 <- lm(y ~ x)

# PUnto 3
par(mfrow=c(1, 3))
plot(mod1, which=1:3)

# Punto 4
mod2 <- lm(y ~ x + I(x^2))
summary(mod2)

par(mfrow=c(1, 3))
plot(mod2, which=1:3)

# Punto 5
media.est <- function(x) sum(coef(mod2) * c(1, x, x^2))
media.est <- Vectorize(media.est)
plot(x=x, y=y)
curve(expr=media.est, from=0, to=10, lwd=3, col='red', add=T)
  
  
  
  
