# Ejemplo superficie respuesta

temp <- c(200, 250, 200, 250, 189.65, 260.35, 
          225, 225, 225, 225, 225, 225)

conc <- c(15, 15, 25, 25, 20, 20, 12.93, 27.07,
          20, 20, 20, 20)

rend <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)

# Dibujo en 3d
require(rgl)

plot3d(x=temp, y=conc, z=rend, col='pink',
       xlab='Temperatura', ylab='Concentracion', 
       zlab='Rendimiento', type='s')

# Modelo polinomial ajustado
mod <- lm(rend ~ temp * conc + I(temp^2) + I(conc^2))
summary(mod)

# Funci?n que recibe temp y conc para calcular el rend
rend.fit <- function(temp, conc) {
  a <- c(1, temp, conc, temp^2, conc^2, temp * conc)
  sum(a * coef(mod))
}

rend.fit <- Vectorize(rend.fit)  # Para vectorizar la funci?n

# Construyendo la superficie de respuesta

k <- 10 # Tama?o de la rejilla, usar un numero grande
Temp <- seq(from=min(temp), to=max(temp), length.out=k)
Conc <- seq(from=min(conc), to=max(conc), length.out=k)
Rend <- outer(X=Temp, Y=Conc, FUN=rend.fit)

contour(x=Temp, y=Conc, z=Rend, nlevels=15,
        xlab='Temperatura', ylab='Concentraci?n')

image(x=Temp, y=Conc, z=Rend, col=rainbow(50),
      xlab='Temperatura', ylab='Concentraci?n')

persp3d(x=Temp, y=Conc, z=Rend, col="lightblue", back = "lines",
        xlab='Temperatura', ylab='Concentracion')


# Encontrando las coordenadas de temp y conc que maximizan el rend ---------

# Funci?n que entrega rendimiento segun un valor de temp y conc
minus_rend <- function(x) {
  temp <- x[1]
  conc <- x[2]
  a <- c(1, temp, conc, temp^2, conc^2, temp * conc)
  -sum(a * coef(mod))
}

inicio <- c(192, 15)  # valor inicial de b?squeda
names(inicio) <- c('Temperatura', 'Concentracion') # Nombres

res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(190, 12), upper=c(260, 28),
              control=list(trace=1))

res$par  # Valores ?ptimos
-res$objective  # Valor del objetivo

contour(x=Temp, y=Conc, z=Rend, nlevels=20,
        xlab='Temperatura', ylab='Concentracion')
points(res$par[1], res$par[2], pch=19, cex=2, col='red')
abline(v=res$par[1], lty="dotted", col='tomato')
abline(h=res$par[2], lty="dotted", col='tomato')



# ?C?mo evoluciona la b?squeda? -------------------------------------------
camino <- capture.output(a <- nlminb(start=inicio, objective=minus_rend,
                                     lower=c(180, 10), upper=c(260, 30),
                                     control=list(trace=1)))
camino

steps <- length(camino)
Trace <- matrix(0, ncol=2, nrow=steps)
for (i in 1:steps) {
  val <- c(as.numeric(substr(camino[i], 22, 28)),
           as.numeric(substr(camino[i], 31, 37)))
  Trace[i, ] <- val
}

Trace

contour(x=Temp, y=Conc, z=Rend, nlevels=40,
        xlab='Temperatura', ylab='Concentraci?n')
lines(Trace, pch=19, col='red', type='b')
points(inicio[1], inicio[2], col='blue', pch=19)


