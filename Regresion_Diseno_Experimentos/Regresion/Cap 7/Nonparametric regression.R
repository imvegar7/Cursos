
# Ejemplo 1 ---------------------------------------------------------------

library(car) # for data sets

# Example: impact of span (f) parameter
fs <- c(0.001, 0.1, 0.3, 0.5, 2/3, 0.999)
for (i in 1:length(fs)) {
  plot(prestige ~ income, xlab="Average Income", ylab="Prestige", data=Prestige,
       main=paste("f=", fs[i]), pch=19)
  with(Prestige, lines(lowess(income, prestige, f=fs[i], iter=0), lwd=4, col=i+1))
}

# Example with lowess
plot(prestige ~ income, xlab="Average Income", ylab="Prestige", data=Prestige)
mod1 <- lowess(x=Prestige$income, y=Prestige$prestige, f=2/3, iter=0)
lines(mod1, lwd=4, col=2)


# Ejemplo 2 ---------------------------------------------------------------


# Example with loess for the same example
mod2 <- loess(prestige ~ income + education, data=Prestige, degree=1, span=0.5)
summary(mod2)

inc <- with(Prestige, seq(min(income), max(income), len=25))
ed <- with(Prestige, seq(min(education), max(education), len=25))
newdata <- expand.grid(income=inc, education=ed)
fit.prestige <- matrix(predict(mod2, newdata), 25, 25)
myplot3d <- persp(inc, ed, fit.prestige, theta=45, phi=30, ticktype="detailed",
                  xlab="Income", ylab="Education", zlab="Prestige", expand=2/3,
                  shade=0.5, col="lightblue")

# How to predict automatically
newdata <- data.frame(   income=c(5635, 12563, 18965),
                      education=c(8.5 ,  12.3,  14.8))
predict(mod2, newdata)
mypoints <- trans3d(x=newdata$income, 
                    y=newdata$education, 
                    z=predict(mod2, newdata), pmat=myplot3d)
points(mypoints, col="yellow", pch=19)


