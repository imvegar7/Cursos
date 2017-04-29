# Problem -----------------------------------------------------------------

# Se desea determinar qué antídoto es apropiado para cada veneno.

# Data --------------------------------------------------------------------
tiempo <- c(3.10,	4.50,	8.20,	11.0,	4.30,	4.50,	4.50,	7.10,
            4.60,	4.30,	8.80,	7.20,	6.30,	7.60,	6.60,	6.20,
            3.60,	2.90,	9.20,	6.10,	4.40,	3.50,	5.60,	10.2,
            4.00,	2.30,	4.90,	12.4,	3.10,	4.00,	7.10,	3.80,
            2.20,	2.10,	3.00,	3.70,	2.30,	2.50,	3.00,	3.60,
            1.80,	2.30,	3.80,	2.90,	2.40,	2.20,	3.10,	3.30)

veneno   <- factor(rep(1:3, each=16))

antidoto <- factor(rep(letters[1:4], each=2, times=6))

# Boxplot -----------------------------------------------------------------
par(mfrow=c(1, 2))
boxplot(tiempo ~ veneno, col='lightblue', las=1,
        xlab='Veneno', ylab='Tiempo (min)')
boxplot(tiempo ~ antidoto, col='pink', las=1,
        xlab='Antídoto', ylab='Tiempo (min)')

# Interaction plot --------------------------------------------------------

interaction.plot(x.factor=antidoto,
                 trace.factor=veneno,
                 response=tiempo,
                 xlab='Antídoto',
                 ylab='Tiempo promedio (min)',
                 col=c('green', 'black', 'red'),
                 fun=mean, lwd=3, las=1, fixed=T)

interaction.plot(x.factor=veneno,
                 trace.factor=antidoto,
                 response=tiempo,
                 xlab='Veneno',
                 ylab='Tiempo promedio (min)',
                 col=c('green', 'black', 'red', 'blue'),
                 fun=mean, lwd=3, las=1, fixed=T)


# Anova -------------------------------------------------------------------
mod <- aov(tiempo ~ antidoto * veneno)
summary(mod)


# Residual analysis -------------------------------------------------------
par(mfrow=c(2, 2))
plot(mod, pch=19)

# Normality
eij <- residuals(mod)
shapiro.test(eij)

# Homocedasticity
bartlett.test(x=eij, g=antidoto)
bartlett.test(x=eij, g=veneno)

require(car)
leveneTest(eij ~ antidoto)
leveneTest(eij ~ veneno)

fligner.test(eij ~ antidoto)
fligner.test(eij ~ veneno)

require(MASS)
transf <- boxcox(tiempo ~ veneno)
lambda <- transf$x[which.max(transf$y)]
lambda

y.t <- tiempo ^ lambda

# Anova -------------------------------------------------------------------
mod2 <- aov(y.t ~ antidoto * veneno)
summary(mod2)

par(mfrow=c(2, 2))
plot(mod2, pch=19)


