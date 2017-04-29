# Ejemplo 4.2 de Gutierrez y de la Vara
#----------------------------------------------------------------------
# Para ingresar los datos
Desgaste <- c(12, 11, 13, 8, 
              14, 12, 11, 13, 
              17, 14, 10, 9, 
              13, 14, 13, 9)

Auto <- c(1, 2, 3, 4, 1, 2, 3, 4, 
          1, 2, 3, 4, 1, 2, 3, 4)
Auto <- as.factor(Auto)

Posicion <- c(1, 1, 1, 1, 2, 2, 2, 2, 
              3, 3, 3, 3, 4, 4, 4, 4)
Posicion <- as.factor(Posicion)

Marca <- c("C", "D", "A", "B", 
           "B", "C", "D", "A", 
           "A", "B", "C", "D", 
           "D", "A", "B", "C")
#----------------------------------------------------------------------
# Boxplot para el desgaste dados los factores

boxplot(Desgaste~Marca, ylab="Desgaste (pulgadas/1000)", 
        xlab="Marca", col='lightblue')

boxplot(Desgaste~Auto, ylab="Desgaste (pulgadas/1000)", 
        xlab="Auto", col='lightblue')

boxplot(Desgaste~Posicion, ylab="Desgaste (pulgadas/1000)", 
        xlab="Posicion", col='lightblue')
#----------------------------------------------------------------------
# Analisis de varianza
mod <- aov(Desgaste ~ Marca + Auto + Posicion)
summary(mod)

#----------------------------------------------------------------------
# Analisis de residuales
eij <- residuals(mod)

# Boxplot for eij vs treatment
boxplot(eij ~ Marca, xlab='Marca', col='pink',
        ylab='Residuales eij')

# Standarized residual vs fitted values
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(x=mod$fitted.values, y=sqrt(abs(eij)),
     xlab='Fitted values', pch=19,
     ylab=expression(sqrt(abs(e[ij]))))

# QQplot for eij
qqnorm(eij, pch=19, col='dodgerblue')
qqline(eij)

# Are the residuals normally distributed??????

# Shapiro Wilk test
shapiro.test(eij)

# Anderson Darling test from nortest package
require(nortest)
ad.test(eij)

# Para obtener todos los graficos
par(mfrow=c(2, 2))
plot(mod, pch=19)

#----------------------------------------------------------------------
# Para aplicar LSD automaticamente
require(agricolae)
LSD.test(mod, "Marca", alpha = 0.05, console=TRUE)
#----------------------------------------------------------------------
