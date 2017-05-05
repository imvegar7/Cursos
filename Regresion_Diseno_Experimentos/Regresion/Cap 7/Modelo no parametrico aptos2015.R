
file <- 'https://raw.githubusercontent.com/fhernanb/datos/master/aptos2015'
datos <- read.table(file=file, header=TRUE)
head(datos)


require(rgl)

plot3d(x=datos$mt2, y=datos$avaluo, z=datos$precio, col='pink',
       xlab="Area", ylab="Avaluo", zlab="Precio", type='s', cex=0.3)


mod <- loess(precio ~ mt2 + avaluo, data=datos, degree=1, span=0.5)
summary(mod)

Mt2 <- with(datos, seq(min(mt2), max(mt2), len=25))
Avaluo <- with(datos, seq(min(avaluo), max(avaluo), len=25))
newdata <- expand.grid(mt2=Mt2, avaluo=Avaluo)
Precio <- matrix(predict(mod, newdata), 25, 25)
myplot3d <- persp(Mt2, Avaluo, Precio, theta=45, phi=30, ticktype="detailed",
                  xlab="Area", ylab="Avaluo", zlab="Precio", expand=2/3,
                  shade=0.5, col="lightblue")


persp3d(Mt2, Avaluo, Precio, col="lightblue", back = "lines",
        xlab="Area", ylab="Avaluo", zlab="Precio")



