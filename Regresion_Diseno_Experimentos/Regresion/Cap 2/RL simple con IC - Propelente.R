
# Edad del propelente -----------------------------------------------------

# En este script se va a replicar el ejemplo 2.1 del texto de
# Montgomery, Peck y Vining.


# Lectura de la base de datos ---------------------------------------------

# Los datos estan disponibles en la url siguiente
file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"

# Para cargar los datos se usa
datos <- read.table(file=file, header=T)

# para disponer de las variables de datos como vectores hacemos:
attach(datos)


# Diagrama de dispersion --------------------------------------------------

# Para construir un diagrama de dispersion hacemos lo siguiente:
plot(x=Edad,y=Resistencia,pch=19, cex=1.5, las=1,
     xlab="Edad (semanas)", ylab="Resistencia (psi)")
# Para calcular la correlaci?n lineal entre las variables
cor(Edad, Resistencia)


# Para ajustar el modelo --------------------------------------------------

# Modelo ajustado usando la funcion lm( )
mod <- lm(Resistencia ~ Edad)

# para obtener la tabla de resultados se hace:
summary(mod)

# para agregar la linea de tendencia al diagrama de dispersion se hace:
abline(mod, col="blue", lwd=2)

# Diagrama de dispersion con linea de tendencia y errores
segments(x0=Edad, y0=Resistencia,
         x1=Edad, y1=mod$fitted.values, lty=2, lwd=2, col='red')


# Intervalos de confianza -------------------------------------------------

confint(object=mod, parm='(Intercept)', level=0.95)
confint(object=mod, parm='Edad', level=0.95)


# Para hacer predicciones -------------------------------------------------

# Para predecir la resistencia de dos elementos con edades de
# 13 semanas y 20 semanas

nuevos_datos <- data.frame(Edad=c(13, 20))
predict(object=mod, newdata=nuevos_datos)





