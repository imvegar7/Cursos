# Para cargar los datos
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)

# Punto 1
with(datos, symbols(x=mt2, y=precio,
                    squares=alcobas,
                    las=1, inches=0.4,
                    fg='dodgerblue4', bg='pink',
                    main='Lado = N° alcobas'))

# Punto 2
mod <- lm(precio ~ mt2 + alcobas, data=datos)
summary(mod)

# Punto 3
# Se escribe usando los resultados del summary

