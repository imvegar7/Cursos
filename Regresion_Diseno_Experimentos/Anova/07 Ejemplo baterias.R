# Problem -----------------------------------------------------------------

# What effects do material type & temperature have on life?

# Data --------------------------------------------------------------------
horas <- c(130, 155, 74, 180,   # by column
           150, 188, 159, 126, 
           138, 110, 168, 160,
           34, 40, 80, 75,
           136, 122, 106, 115,
           174, 120, 150, 139,
           20, 70, 82, 58,
           25, 70, 58, 45,
           96, 104, 82, 60)

temperatura <- rep(c(15, 70, 125), each=12)
material    <- rep(1:3, each=4, times=3)

temperatura <- factor(temperatura)
material    <- factor(material)


# Boxplot -----------------------------------------------------------------
par(mfrow=c(1, 2))
boxplot(horas~material, las=1, xlab='Material',
        ylab='Duración (horas)')
boxplot(horas~temperatura, las=1, xlab='Temperatura (°K)',
        ylab='Duración (horas)')


# Interaction plot --------------------------------------------------------

interaction.plot(x.factor=temperatura,
                 trace.factor=material,
                 response=horas,
                 xlab='Temperatura',
                 ylab='Duración promedio (horas)',
                 col=c('green', 'black', 'red'),
                 fun=mean, lwd=3, las=1, fixed=T)

interaction.plot(x.factor=material,
                 trace.factor=temperatura,
                 response=horas,
                 xlab='Material',
                 ylab='Duración promedio (horas)',
                 col=c('green', 'black', 'red'),
                 fun=mean, lwd=3, las=1, fixed=T)

# Anova -------------------------------------------------------------------
resultado <- aov(horas ~ temperatura * material)
summary(resultado)


# Residual analysis -------------------------------------------------------

par(mfrow=c(2, 2))
plot(resultado, pch=19)


