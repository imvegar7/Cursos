# Problem -----------------------------------------------------------------

# Se desea entender mejor las fuentes de variabilidad y disminuir Y.

# Data --------------------------------------------------------------------
y <- c(-3, -1, -1, 1,
       -1, 0, 0, 1,
       0, 2, 2, 6,
       1, 1, 3, 5,
       5, 7, 7, 10,
       4, 6, 9, 11)
presion <- rep(c(25, 30), each=2, times=6)
rapidez <- rep(c(200, 250), times=12)
carbo <- rep(c(10, 12, 14), each=8)

presion <- factor(presion)
rapidez <- factor(rapidez)
carbo <- factor(carbo)


# Boxplot -----------------------------------------------------------------
par(mfrow=c(1, 3))
boxplot(y ~ presion, las=1, xlab='Presión',
        ylab='Desviación llenado (ml)')

boxplot(y ~ rapidez, las=1, xlab='Rapidez',
        ylab='Desviación llenado (ml)')

boxplot(y ~ carbo, las=1, xlab='Carbonatación',
        ylab='Desviación llenado (ml)')

# Interaction plot --------------------------------------------------------

interaction.plot(x.factor=presion,
                 trace.factor=rapidez,
                 response=y,
                 xlab='Presión',
                 ylab='Desviación llenado (ml)',
                 col=c('black', 'red'),
                 fun=mean, lwd=3, las=1, fixed=T)
axis(4, las=1)

interaction.plot(x.factor=presion,
                 trace.factor=carbo,
                 response=y,
                 xlab='Presión',
                 ylab='Desviación llenado (ml)',
                 col=c('black', 'red', 'blue'),
                 fun=mean, lwd=3, las=1, fixed=T)
axis(4, las=1)

interaction.plot(x.factor=rapidez,
                 trace.factor=carbo,
                 response=y,
                 xlab='Rapidez',
                 ylab='Desviación llenado (ml)',
                 col=c('black', 'red', 'blue'),
                 fun=mean, lwd=3, las=1, fixed=T)
axis(4, las=1)

# Anova -------------------------------------------------------------------
mod1 <- aov(y ~ carbo * presion * rapidez)
summary(mod1)

mod2 <- aov(y ~ presion + rapidez + carbo * rapidez)
summary(mod2)

# Residual analysis -------------------------------------------------------
par(mfrow=c(2, 2))
plot(mod2, pch=19)


