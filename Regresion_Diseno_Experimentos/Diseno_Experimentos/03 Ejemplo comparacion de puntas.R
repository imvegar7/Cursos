# Problem -----------------------------------------------------------------

# Suppose we wish to determine whether or not four different tips produce 
# different readings on hardness testing machine.

# Data --------------------------------------------------------------------
y <- c(9.3, 9.4, 9.2, 9.7, 
       9.4, 9.3, 9.4, 9.6,  
       9.6, 9.8, 9.5, 10 , 
       10 , 9.9, 9.7, 10.2)

punta <- c(1, 2, 3, 4,
           1, 2, 3, 4,
           1, 2, 3, 4,
           1, 2, 3, 4)
punta <- as.factor(punta)

lamina <- c(1, 1, 1, 1,
            2, 2, 2, 2,
            3, 3, 3, 3,
            4, 4, 4, 4)
lamina <- as.factor(lamina)

# Anova -------------------------------------------------------------------
mod <- aov(y ~ punta + lamina)
summary(mod)

#----------------------------------------------------------------------
# Analisis de residuales
eij <- residuals(mod)

# Boxplot for eij vs treatment
boxplot(eij ~ punta, xlab='Punta', col='pink',
        ylab='Residuales eij')

# Standarized residual vs fitted values
par(mar=c(5.1, 5.1, 4.1, 2.1))
plot(x=mod$fitted.values, y=sqrt(abs(eij)),
     xlab='Fitted values', pch=19,
     ylab=expression(sqrt(abs(e[ij]))))

# QQplot for eij
qqnorm(eij, pch=19, col='dodgerblue')
qqline(eij)

# To plot the first, second and third graphs
par(mfrow=c(2, 2))
plot(mod, 1, pch=19)
plot(mod, 2, pch=19)
plot(mod, 3, pch=19)

# What happens if we do not consider the blocking factor? -----------------
mod2 <- aov(y ~ punta)
summary(mod2)
