# Para cargar los datos

url <- 'https://raw.githubusercontent.com/fhernanb/datos/master/lifespan'
datos <- read.table(url, header=T, sep='\t')
dim(datos)


# Punto 1
datos[datos == 0] <- NA

# Punto 2
length(table(datos$year))
range(datos$year)

table(datos$region)

# Punto 3
data2000 <- subset(datos, year == 2000)
reg <- names(table(data2000$region))
with(data2000, symbols(x=income, y=life, circles=pop,
                       inches=0.6))

# Punto 4
require(gamlss)
mod1 <- gamlss(life ~ income + pop,
                  sigma.fo= ~ income + pop,
                  family=NO2, data=na.omit(data2000))
summary(mod1)

mod2 <- gamlss(life ~ income + pop,
               sigma.fo= ~ income,
               family=NO2, data=na.omit(data2000))
summary(mod2)

mod3 <- gamlss(life ~ income,
               sigma.fo= ~ income,
               family=NO2, data=na.omit(data2000))
summary(mod3)

mod4 <- gamlss(life ~ income,
               sigma.fo= ~ 1,
               family=NO2, data=na.omit(data2000))
summary(mod4)

# Punto 7
AIC(mod1, k=3)
AIC(mod2, k=3)
AIC(mod3, k=3)
AIC(mod4, k=3) # El mejor

