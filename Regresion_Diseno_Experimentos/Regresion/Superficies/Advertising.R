# Cargando los datos
url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/Advertising'
datos <- read.table(file=url, header=T, sep='\t')
head(datos)

# Analisis descriptivo
pairs(datos)

library(rgl)
with(datos, plot3d(x=TV, y=Radio, z=Sales, type='s', col='pink'))
with(datos, plot3d(x=TV, y=Newspaper, z=Sales, type='s', col='blue'))
with(datos, plot3d(x=Radio, y=Newspaper, z=Sales, type='s', col='green'))


# Modelos iniciales -------------------------------------------------------

mod1 <- lm(Sales ~ ., data=datos)
summary(mod1)

mod2 <- lm(Sales ~ TV + Radio, data=datos)
summary(mod2)

mod3 <- lm(Sales ~ TV * Radio, data=datos)
summary(mod3)


# Seleccion de variables --------------------------------------------------

# forward
empty.model <- lm(Sales ~ 1, data = datos)
horizonte <- formula(lm(Sales ~ TV + Radio + Newspaper + 
                          TV * Radio * Newspaper + 
                          I(TV^2) + I(Radio^2) + I(Newspaper^2), data=datos))
require(MASS)
modforw <- stepAIC(empty.model, trace=FALSE,
                   direction="forward", scope=horizonte)
modforw$anova
summary(modforw)

# backward
full.model <- lm(horizonte, data = datos)
modback <- stepAIC(full.model, trace=FALSE, direction="backward")
modback$anova

# both
modboth <- stepAIC(empty.model, trace=FALSE, direction="both", scope=horizonte)
modboth$anova
summary(modboth)

coef(modforw)
coef(modback)
coef(modboth)


# R2 ----------------------------------------------------------------------
summary(mod3)$adj.r.squared
summary(modback)$adj.r.squared
summary(modforw)$adj.r.squared
summary(modboth)$adj.r.squared

# Seleccion de modelos con BAIC -------------------------------------------
k <- nrow(datos)
AIC(mod3, k=k)
AIC(modforw, k=k)
AIC(modback, k=k)
AIC(modboth, k=k)


# Residuales --------------------------------------------------------------

par(mfrow=c(2, 2))
plot(mod3, pch=19)
plot(modforw, pch=19)
plot(modback, pch=19)
plot(modboth, pch=19)


# Modelo final ------------------------------------------------------------

mod <- lm(Sales ~ TV * Radio, data=datos[-c(79, 156, 131), ])
summary(mod)
plot(mod, pch=19)

fun <- function(x1, x2) sum(coef(mod) * c(1, x1, x2, x1 * x2))
fun <- Vectorize(fun)
x1 <- seq(from=0.7, to=300, length.out=10)
x2 <- seq(from=0, to=50, length.out=10)
y <- outer(x1, x2, fun)

# contornos
contour(x1, x2, y, xlab='TV', ylab='Radio')

# contornos
filled.contour(x1, x2, y, xlab='TV', ylab='Radio',
               color = terrain.colors)

# Superficie en 3d
persp(x1, x2, y, theta=30, phi=30,
      ticktype = "detailed", nticks=3,
      col='lightblue', border='blue',
      xlab='TV', ylab='Radio',
      zlab='Sales')


# Superficies con rsm -----------------------------------------------------
require(rsm)

rsm1 <- rsm(Sales ~ FO(TV) + Radio + TV * Radio,
            data=datos[-c(79, 156, 131), ])
summary(rsm1)

contour(rsm1, ~ TV + Radio)









