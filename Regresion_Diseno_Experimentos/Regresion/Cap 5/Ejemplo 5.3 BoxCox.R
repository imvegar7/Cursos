#---------------------------------------------------------------------------------
#---------------------------------------------------------------------------------
#--------------------------- Transformaci?n Box-Cox ------------------------------
#------------------- para el ejemplo 5.3 de Montgomery et al.---------------------
#---------------------------------------------------------------------------------
# Para cargar y disponer de los datos usamos
ruta <- "https://tinyurl.com/k3quqpt"
datos <- read.table(file=ruta, header=T, sep="\t")
head(datos)
attach(datos)
#---------------------------------------------------------------------------------
# En forma manual

transf <- function(y,lambda) {
  y.dot <- exp(mean(log(y)))
  if (lambda!=0) y.transf <- (y^lambda-1)/(lambda*y.dot^(lambda-1))
  else y.transf <- y.dot * log(y)
  y.transf
}

transf(kWh, 2)


BoxCox <- function(lambda) {
  mod <- lm(transf(kW, lambda) ~ kWh)
  logLik(mod)
}

lambdas <- seq(-2, 2, by=0.01)
ll <- NULL
for (i in 1:length(lambdas)) ll[i] <- BoxCox(lambdas[i])
plot(x=lambdas, y=ll)
lambdas[which.max(ll)]

#---------------------------------------------------------------------------------
# Diagrama de dispersi?n
plot(x=kW, y=kWh, pch=19)
# Ajustamos el modelo 1
mod1 <- lm(kWh~kW)
# Tabla de resultados
summary(mod1)
# Para obtener el gr?fico de residuales #1, se puede elegir entre 4.
plot(mod1, which=1)
#---------------------------------------------------------------------------------
# Cargamos la librer?a MASS para poder usar la funci?n boxcox
library(MASS)
# Usando la funci?n boxcox() sobre el modelo ajustado tenemos:
bc <- boxcox(mod1)
# Para encontrar el valor de lambda que maximiza log-likelihood hacemos
lambda <- bc$x[which.max(bc$y)]
lambda
#---------------------------------------------------------------------------------
# Modelo transformado
mod2 <- lm(kWh^lambda~kW)
summary(mod2)

par(mfrow=c(2,2))
plot(mod2)

