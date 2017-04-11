
# Pastilla electrónica ----------------------------------------------------

# En este script se va a replicar el ejemplo sobre pastillas electronicas

# -------------------------------------------------------------------------
# Lectura de la base de datos ---------------------------------------------
# -------------------------------------------------------------------------

# Los datos estan disponibles en la url siguiente
enlace <- "https://raw.githubusercontent.com/fhernanb/datos/master/pastillas"

# Para cargar los datos se usa
datos <- read.table(file=enlace, header=T)

# para disponer de las variables de datos como vectores hacemos:
attach(datos)

# A continuacion una foto del objeto de estudio
shell.exec("http://www.dscuento.com/circuitos-integrados/")

# -------------------------------------------------------------------------
# Matriz de correlaciones entre las variables -----------------------------
# -------------------------------------------------------------------------
cor(datos)

# -------------------------------------------------------------------------
# Diagramas de dispersion en 2d -------------------------------------------
# -------------------------------------------------------------------------
plot(datos, pch=19, col='blue', cex=1.3, las=1)

# -------------------------------------------------------------------------
# Diagramas de dispersion en 3d -------------------------------------------
# -------------------------------------------------------------------------

# FORMA 1 con el paquete scatterplot3d
install.packages("scatterplot3d", dependencies=TRUE)
library(scatterplot3d)
scatterplot3d(x=Length, y=Height, z=Stregth)

# Podemos mejorar la apariencia del diagrama anterior asi:
scatterplot3d(x=Length, y=Height, z=Stregth, pch=16, cex.lab=1.5,
              highlight.3d=TRUE, type="h")

# FORMA 2 con el paquete rgl
install.packages("rgl", dependencies = TRUE)
library(rgl)
plot3d(x=Length, y=Height, z=Stregth,
       main='Mueva con el mouse la figura')


# -------------------------------------------------------------------------
# Ajustando el modelo y agregando el plano --------------------------------
# -------------------------------------------------------------------------

# Ajustando el modelo de regresion
mod <- lm(Stregth ~ Length + Height)
summary(mod)

# Vamos a agregar el plano de regresion al diagrama de la forma 1
# Para repetir el diagrama forma 1
mi_grafico_3d <- scatterplot3d(x=Length, y=Height, z=Stregth, 
                               pch=16, cex.lab=1.5,highlight.3d=TRUE, 
                               type="h", main='Diagrama en 3d')
# Para agregar el plano usamos la funcion s3d$plane3d( )
# con argumento modelo ajustado. El resto de argumentos son opcionales
mi_grafico_3d$plane3d(mod, lty.box="solid", col='blue', cex=2)



# -------------------------------------------------------------------------
# Intervalos de confianza para betas --------------------------------------
# -------------------------------------------------------------------------

# Como obtener los IC en un modelo de regresion lineal multiple?
# Primero necesitamos los nombres de los coeficientes que 
# podemos extraer as:
betas <- names(coef(mod))
# Para ver los nombres que estan en el vector betas
betas

# Ahora usamos la funcion confint(object, parm, level = 0.95)
confint(object=mod, parm=betas, level=0.95)

# Es posible obtener IC con niveles de confianza diferentes
# modificando el parametro level
confint(object=mod, parm=betas, level=0.97)


# -------------------------------------------------------------------------
# Pruebas de hipotesis ----------------------------------------------------
# -------------------------------------------------------------------------

# Para pruebas sobre la significancia de la regresion 
# y prueba sobre coeficientes individuales se usa la salida del resumen
summary(mod)


# Usando la prueba F parcial para probar
# Ho: beta_2 = 0 vs Ha: beta_2 dif 0
mod.redu <- lm(Stregth ~ Length)

# SSt para el modelo completo
SSt <- sum(Stregth^2) - sum(Stregth)^2 / length(Stregth)
SSt <- var(Stregth) * (length(Stregth)-1) # otra forma
# SSres para el modelo completo
SSres <- sum(residuals(mod)^2)
  SSR <- SSt - SSres
# MSRes para el modelo completo
MSRes <- SSres / (length(Stregth)-2-1)

# SSres para el modelo reducido
SSres.redu <- sum(residuals(mod.redu)^2)
  SSR.redu <- SSt - SSres.redu

# Estadistico
F0 <- ( (SSR - SSR.redu) / 1 ) / MSRes
# Valor de referencia para comparar
qf(p=0.05, df1=1, df2=length(Stregth)-3)


# -------------------------------------------------------------------------
# Obtencion de la matriz H ------------------------------------------------
# -------------------------------------------------------------------------

# Para obtener los valores hii de cada uno de los puntos usamos la funcion
# lm.influence y solicitamos el valor hat colocando al final $hat asi:
hii <- lm.influence(mod)$hat

# para ver los valores hii hacemos
hii

# El valor hmax de referencia se obtiene como el maximo del vector hii
hmax <- max(hii)

# para ver hmax
hmax

# Para detectar la posicion del m?ximo de hii se usa la funcion which.max
which.max(hii)

# Para crear una tabla que muestre los regresores y su hii hacemos
cbind(Length, Height, hii=round(hii, 2))
# La funcion round se usa para redondear con los decimales deseados

# Como obtener la matriz X?
# Se puede usar la funcion model.matrix aplicada al modelo ajustado
X <- model.matrix(mod)
X

# Tambien se puede obtener asi:
X <- model.matrix( ~ Length + Height)
X

# Como se calcula el hoo para una nueva observacion?
# Supongamos que la nueva observaci?n es
x0 <- c(1, 20, 150)
h00 <- x0 %*% solve(t(X)%*%X) %*% x0
h00

# Cual es el centroide de los datos?
centro <- c(mean(Length), mean(Height))
centro

# Donde se encuentra el centroide?
plot(Length, Height, pch=19,cex.lab=1.5)
points(x=mean(Length), y=mean(Height), pch=20, cex=3, col='blue')

# Donde se encuentra la nueva observacion?
points(x=20, y=150, pch=20, cex=3, col='red')

# Donde se encuentra la observacion con hmax?
points(x=Length[18], y=Height[18], pch=20, cex=3, col='purple')


# -------------------------------------------------------------------------
# How can I estimate response values --------------------------------------
# -------------------------------------------------------------------------

# If you want to obtain the estimated betas you can use
coef(mod)

# To obtain fitted values for y given length and height
y.ajustados <- coef(mod)[1] + coef(mod)[2] * Length + coef(mod)[3] * Height

# If you want to obtain fitted values for y you can use 
# fitted() or predict() function
fitted(mod)   
predict(mod) # The same results

# Compare
cbind(Stregth, y.ajustados, fitted(mod), predict(mod))

# How can I obtain interval estimates for confidence and prediction?
confi <- predict(object=mod, interval = "confidence")
predi <- predict(object=mod, interval = "prediction")
cbind(confi, predi)

# How can I estimate y values for a new set of covariates?
# Suposse you want to estimate y for lengths of 5, 12, 17, 20
# and heighs of 50, 155, 371, 150
new.data.set <- data.frame(Length=c(5,12,17,20),
                           Height=c(50,155,371,150))

predict(object=mod, new=new.data.set)

predict(object=mod, new=new.data.set, se.fit=TRUE)

