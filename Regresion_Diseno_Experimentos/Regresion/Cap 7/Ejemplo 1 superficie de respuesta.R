# Ejemplo superficie respuesta

temp <- c(200, 250, 200, 250, 189.65, 260.35, 
          225, 225, 225, 225, 225, 225)

conc <- c(15, 15, 25, 25, 20, 20, 12.93, 27.07,
          20, 20, 20, 20)

rend <- c(43, 78, 69, 73, 48, 76, 65, 74, 76, 79, 83, 81)

# Dibujo en 3d
library(scatterplot3d)
scatterplot3d(x=temp, y=conc, z=rend, pch=16, cex.lab=1.5,
              highlight.3d=TRUE, type="h")

# Dibujo en 3d
require(rgl)

plot3d(x=temp, y=conc, z=rend, lwd=2, col='orange',
       xlab='Temperatura', ylab='Concentracion', type='s',
       zlab='Rendimiento')

# Modelo polinomial ajustado
mod <- lm(rend ~ poly(temp, conc, degree=2))
summary(mod)

# Construyendo la superficie de respuesta
library(rsm)
image(mod, conc ~ temp)
contour(mod, conc ~ temp)
persp(mod, conc ~ temp, zlab = "Rendimiento")

persp(mod, temp ~ conc, col = "blue",
      bounds = list(temp=c(150, 300), conc=c(10, 30)),
      zlab = "Rendimiento", 
      contours = list(z="bottom", col="blue"),
      theta = -145, phi = 35)


# ncontrando las coordenadas de temp y conc que maximizan el rend ---------

# Funci?n que entrega -rendimiento segun un valor de temp y conc
minus_rend <- function(x) {
  temp <- x[1]
  conc <- x[2]
  new.data <- data.frame(temp=c(1, temp), conc=c(1, conc))
  -predict(mod, new.data)[2]
}

inicio <- c(192, 15)  # valor inicial de b?squeda
names(inicio) <- c('Temperatura', 'Concentracion') # Nombres

res <- nlminb(start=inicio, objective=minus_rend,
              lower=c(190, 12), upper=c(260, 28),
              control=list(trace=1))

res$par  # Valores ?ptimos
-res$objective  # Valor del objetivo

contour(mod, conc ~ temp)
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

contour(mod, conc ~ temp)
lines(Trace, pch=19, col='red', type='b')
points(inicio[1], inicio[2], col='blue', pch=19)


