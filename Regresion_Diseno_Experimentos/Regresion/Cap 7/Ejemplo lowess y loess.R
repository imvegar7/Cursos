# Ejemplo lowess

library(car)

plot(prestige ~ income, xlab='Average income',
     ylab='Prestige', data=Prestige, pch=19)

with(Prestige, lines(lowess(income, prestige, f=0.5, iter=0), 
                     lwd=3, col='tomato'))


# Ejemplo loess

library(rgl)
with(Prestige, plot3d(education, income, prestige, type='s',
                      col="blue", size=2))


mod.lo <- loess(prestige ~ income + education, span=.5, 
                degree=1, data=Prestige)
summary(mod.lo)

inc <- with(Prestige, seq(min(income), max(income), len=25))
ed <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(mod.lo, newdata), 25, 25)
persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
      xlab="Income", ylab="Education", zlab="Prestige", expand=2/3,
      shade=0.5)



