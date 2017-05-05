
# Simulaci?n de un modelo no lineal ---------------------------------------
theta1 <- 440
theta2 <- -43
theta3 <- 0.02
sigma <- 0.1

x <- seq(from=1790, to=2000, by=10)
media <- theta1 / (1 + exp(-(theta2 + theta3 * x)))
set.seed(12345678)
y <- rnorm(n=length(x), mean=media, sd=sigma)

plot(x, y)


# Residual sum of squares -------------------------------------------------

S <- function(theta) {
  media <- theta[1] / (1 + exp(-(theta[2] + theta[3] * x)))
  sum((y - media)^2)
}

S(c(400, -40, 0))


# Valores iniciales -------------------------------------------------------
require(car)
t1 <- 50
aux <- lm(logit(y / t1) ~ x)
initial <- c(t1, coef(aux))
initial

S(initial)

# Minimizando S -----------------------------------------------------------

nlminb(start=initial, objective=S)
optim(par=initial, fn=S)

# Usando nls --------------------------------------------------------------

mod <- NULL
mod <- nls(y ~ theta1 / (1 + exp(-(theta2 + theta3 * x))),
           start=list(theta1=initial[1], theta2=initial[2], theta3=initial[3]),
           trace=T, control=nls.control(maxiter=1000))

summary(mod)

datos <- data.frame(y=y, x=x)
plot(y ~ x, xlim=c(1790, 2016), ylim=c(0, 70))
with(datos, lines(seq(1790, 2016, by=10),
                  predict(mod, data.frame(x=seq(1790, 2016, by=10))), lwd=2))



# Self-starting values ----------------------------------------------------

mod.self <- nls(y ~ SSlogis(x, phi1, phi2, phi3))
summary(mod.self)






