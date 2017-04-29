#----------------------------------------------------------------------------
#-------------------- Ejemplo con pastilas de circuitos ---------------------
#----------------------------------------------------------------------------

# Los datos están en un archivo de Excel con extensión .csv
# La ubicación de los datos está en la variable enlace y podemos ver la base
# con las siguientes dos instrucciones:
enlace <- "http://dl.dropboxusercontent.com/u/9602072/Datos%20sobre%20pastil
las%20de%20circuitos.csv"
# Par cargar los datos en R, se debe usar sep=";" para que R entienda
# cómo están separadas las filas del archivo.csv
datos <- read.table(enlace, header=T, sep=";")
#----------------------------------------------------------------------------
# Disponibilizando las columnas como vectores
attach(datos)


#----------------------------------------------------------------------------
#------------------------- Ajustando el modelo       ------------------------
#----------------------------------------------------------------------------

# Ajustando el modelo de regresión
mod <- lm(Stregth ~ Length + Height)


#----------------------------------------------------------------------------
#----------------------     Análisis de residuos    -------------------------
#----------------------------------------------------------------------------
# Recordemos que en mod está el modelo ajustado
# La desviación estándar de los errores se extrae de mod así:
sd_e <- summary(mod)$sigma
sd_e

# Los residuales se pueden obtener así:
ei <- residuals(mod)
ei
# Los residuales estandarizados se obtienen así:
ei_stand <- (ei-mean(ei)) / sd_e
ei_stand
# Los valores ajustados del modelo se obtienen así:
y.ajus <- mod$fitted.values
y.ajus

# Para ver una tabla con los elementos anteriores útiles para el análisis
# usamos la fución cbind, bind=pegar y la "c" quiere decir que vamos a
# pegar por columnas los vectores
cbind(Stregth, ei, ei_stand, y.ajus)

# Para obtener los valores hii de cada uno de los puntos usamos la función
# lm.influence y solicitamos el valor hat colocando al final $hat así:
hii <- lm.influence(mod)$hat
# para ver los valores hii hacemos
hii

#-------------- Gráfico de normalidad de los errores ---------------------
# Vamos a colorear el fondo del gráfico para mejorar la estética
# para ver las opciones de colores consulte el siguiente pdf
shell.exec("http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")
# Ahora si el grafico
par(bg='salmon')
qqnorm(ei_stand, pch=19, main='Gráfico de normalidad',
       xlab='Cuantiles teoricos', 
       ylab='Residuales estandarizados')
qqline(ei_stand, lty='longdash', col='yellow2', lwd=2)

#----------------- Gráfico de residuales vs valores ajustados ------------
par(bg='greenyellow')
plot(x=y.ajus, y=ei_stand, pch=19, col='coral3', 
     main='Residuales estandarizados vs valores ajustados',
     ylab='Residuales estandarizados',
     xlab='Valores ajustados')

#-- Gráfico de raiz cuadradada de residuales vs valores ajustados --------
par(bg='plum3')
plot(x=y.ajus, y=sqrt(abs(ei_stand)), pch=19, col='blue3', 
     ylab=expression(sqrt(lR.E.l)), 
     xlab='Valores ajustados')

#------------ Gráfico de residuales estandarizados vs hii  ----------------
par(bg='gray45')
plot(x=hii, y=ei_stand, pch=19, col='gold', 
     ylab='Residuales estandarizados', 
     xlab=expression(h[ii]))

#-------------------- Gráficos de residuales con R ------------------------
# R nos entrega 4 gráficos para analizar los residuales
# Vamos a dibujar los 4 gráficos en la misma ventana gráfica
# para esto vamos a dividirla en dos filas y dos columnas
par(mfrow=c(2,2))
# Ahora solicitamos los 4 gráficos
plot(mod, pch=19)

#----------------------------------------------------------------------------
