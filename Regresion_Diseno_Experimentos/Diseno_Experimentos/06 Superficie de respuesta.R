
# For example 1 -----------------------------------------------------------
fun <- function(x1, x2) 35.5 + (21/2) * x1 + (11*2) * x2 + (1/2) * x1 * x2
fun <- Vectorize(fun)
x1 <- seq(from=-1, to=1, length.out=7)
x2 <- seq(from=-1, to=1, length.out=7)
y <- outer(x1, x2, fun)

# Superficie en 3d
persp(x1, x2, y, theta=30, phi=30,
      ticktype = "detailed", nticks=3,
      col='springgreen1', border='blue',
      xlab='Factor A', ylab='Factor B',
      zlab='Response')

# Grafico de contornos
contour(x=x1, y=x2, z=y, nlevels=50,
        col='salmon', lwd=1, lty='solid',
        xlab=expression(x[1]), ylab=expression(x[2]))

# Grafico de niveles
filled.contour(x=x1, y=x2, z=y, nlevels=20,
               xlab=expression(x[1]), ylab=expression(x[2]),
               color=topo.colors)


# For example 2 -----------------------------------------------------------
fun <- function(x1, x2) 30.5 + (1/2) * x1 - (9/2) * x2 - (29/2)*x1*x2
fun <- Vectorize(fun)
x1 <- seq(from=-1, to=1, length.out=15)
x2 <- seq(from=-1, to=1, length.out=15)
y <- outer(x1, x2, fun)

persp(x1, x2, y, theta=20, phi=30,
      ticktype = "detailed", nticks=3,
      col='pink', border='black',
      xlab='Factor A', ylab='Factor B',
      zlab='Response')

# Grafico de contornos
contour(x=x1, y=x2, z=y, nlevels=50,
        col='blue', lwd=3, lty='solid',
        xlab=expression(x[1]), ylab=expression(x[2]))

# Grafico de niveles
filled.contour(x=x1, y=x2, z=y, nlevels=20,
               xlab=expression(x[1]), ylab=expression(x[2]),
               color=heat.colors)


# Pretty response surface using rgl package -------------------------------
library(rgl)

fun <- function(x1, x2) 30.5 + 1*x1 - 9*x2 - (29/2)*x1*x2
fun <- Vectorize(fun)
x1 <- seq(from=-1, to=1, length.out=30)
x2 <- seq(from=-1, to=1, length.out=30)
y <- outer(x1, x2, fun)

bg3d("lavenderblush1")
persp3d(x1, x2, y, col="lightskyblue",
        xlab="Factor A", ylab="Factor B", zlab="Response")




