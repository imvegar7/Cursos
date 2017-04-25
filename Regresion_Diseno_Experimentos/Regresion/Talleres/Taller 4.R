# Para cargar los datos
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)

# Punto 1
summary(datos[, c('mt2', 'alcobas')])

# Punto 2
with(datos, symbols(x=mt2, y=precio,
                    squares=alcobas,
                    las=1, inches=0.4,
                    fg='dodgerblue4', bg='pink',
                    main='Lado = N° alcobas'))
# Punto 3
obs.rara <- which.max(datos$alcobas)
obs.rara
datos[obs.rara, ]

# Punto 4
datos2 <- datos[-obs.rara, ]
with(datos2, symbols(x=mt2, y=precio,
                     squares=alcobas,
                     las=1, inches=0.2,
                     fg='dodgerblue4',
                     main='Lado = N° alcobas'))

# Punto 5
mod <- lm(precio ~ mt2 + alcobas, data=datos2)
summary(mod)

# Punto 6
mu.est <- function(area, habit) sum(coef(mod) * c(1, area, habit))
mu.est <- Vectorize(mu.est) # Esto para vectorizar la funcion

Areas <- seq(from=26, to=500, length.out=100)
Habit <- 1:5
y <- outer(Areas, Habit, mu.est)

# Grafico de contornos
contour(x=Areas, y=Habit, z=y, nlevels=20,
        lwd=1, lty='solid',
        xlab='Area', ylab='No alcobas')



