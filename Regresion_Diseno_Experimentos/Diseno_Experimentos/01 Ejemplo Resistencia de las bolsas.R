
# Problem -----------------------------------------------------------------
# A manufacturer of paper grocery bags is interested in improving the 
# tensile strength of the product. The process engineer decides to 
# investigate 4 levels of hardwood concentration: 5%, 10%, 15%, and 20%.
# She takes six specimens (replicates) at each level.

# Dataset -----------------------------------------------------------------
strength  <- c(7, 8, 15, 11, 9, 10,
               12, 17, 13, 18, 19, 15,
               14, 18, 19, 17, 16, 18,
               19, 25, 22, 23, 18, 20)

treatment <- c(5, 5, 5, 5, 5, 5,
               10, 10, 10, 10, 10, 10,
               15, 15, 15, 15, 15, 15,
               20, 20, 20, 20, 20, 20)

treatment <- factor(treatment)

# Boxplot -----------------------------------------------------------------
boxplot(strength ~ treatment, las=1, col='lightblue',
        xlab='Porcentaje de madera dura',
        ylab='Resistencia (psi)')

# Exploring means ---------------------------------------------------------
sapply(X=split(x=strength, f=treatment), FUN=mean)

# Using anova -------------------------------------------------------------
mymodel <- aov(strength ~ treatment)
summary(mymodel)

# Residual analysis -------------------------------------------------------
eij <- residuals(mymodel)

# Boxplot for eij vs treatment
boxplot(eij ~ treatment,
        xlab='Porcentaje de madera dura',
        ylab='Residuales eij')

# Standarized residual vs fitted values
plot(x=mymodel$fitted.values, y=sqrt(abs(eij)),
     xlab='Fitted values',
     ylab='sqrt abs eij', pch=19)

# Bartlett test
bartlett.test(x=eij, g=treatment)

# Levene test
require(car)
leveneTest(eij ~ treatment)

# Fligner-Killeen test
fligner.test(eij ~ treatment)

# QQplot for eij
qqnorm(eij, pch=19, col='dodgerblue')
qqline(eij)

# Shapiro Wilk test
shapiro.test(eij)

# Using the standard residual analysis
par(mfrow=c(2, 2))
plot(mymodel, pch=19, col='dodgerblue')

