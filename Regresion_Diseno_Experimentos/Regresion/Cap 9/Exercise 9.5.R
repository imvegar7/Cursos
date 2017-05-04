require(MASS) # to load the stepAIC function
require(MPV) # to load the data

# Excersice 9.5 from MPV

# Usando los datos de la tabla b3 aplicar el proceso de selecci?n de variables
# para obtener el mejor modelo.

data(table.b3)
table.b3[22:26, ] # Can you see the missing values?
datis <- table.b3[-c(23,25), ]

# The full model ------------------------------------------------------------
full.model <- lm(y ~ ., data = datis)
summary(full.model)

# logLik and AIC for full.model
length(coef(full.model))            # Number of betas
logLik(full.model)                  # logLik with 13 df
-2 * logLik(full.model) + 2 * 13    # AIC manually
AIC(full.model, k=2)                # AIC automatically
AIC(full.model, k=log(30))               # BIC automatically

# backward selection ------------------------------------------------------
modback <- stepAIC(full.model, trace=TRUE, direction="backward")
modback$anova
summary(modback)

# forward selection ------------------------------------------------------
empty.model <- lm(y ~ 1, data = datis)
horizonte <- formula(lm(y ~ ., data = datis))
horizonte
modforw <- stepAIC(empty.model, trace=FALSE, direction="forward",
                   scope=horizonte)
modforw$anova
summary(modforw)

# Comparing --------------------------------------------------------------
coef(modback)
AIC(modback)

coef(modforw)
AIC(modforw)

# In a graphical way -----------------------------------------------------
par(mfrow=c(1, 2))
require(car)
qqPlot(modback, main="Backward", pch=19)
qqPlot(modforw, main="Forward", pch=19)


#-----------------------------------------------------------------------------
#------------------------ All Subsets Regression -----------------------------
#-----------------------------------------------------------------------------
 
library(leaps)
prueba <- regsubsets(y ~ ., data = datis, nbest=2, intercept=T)
summary(prueba)
summary(prueba)

do.call(cbind,(summary(prueba)[c("rsq","rss","adjr2","cp","bic")]))


# To have x8 and x3 in the model
prueba <- regsubsets(y ~ ., data = datis, nbest=1, intercept=T, 
                     force.in=c("x8", "x3"))
summary(prueba)

do.call(cbind,(summary(prueba)[c("rsq","rss","adjr2","cp","bic")]))

# Coefficients for the first 3 models
coef(prueba, 1:3)

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
par(mfrow=c(1,1))
plot(prueba, scale="bic")
plot(prueba, scale="adjr2")
plot(prueba, scale="r2")
plot(prueba, scale="Cp")

  