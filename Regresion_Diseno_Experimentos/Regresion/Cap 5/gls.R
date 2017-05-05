
https://socserv.socsci.mcmaster.ca/jfox/Books/Companion/appendix/Appendix-Timeseries-Regression.pdf

library(car)
Hartnagel

plot(fconvict ~ year, type="o", pch=16, data=Hartnagel,
     ylab="Convictions per 100,000 Women")

mod.ols <- lm(fconvict ~ tfr + partic + degrees + mconvict, data=Hartnagel)
summary(mod.ols)

plot(Hartnagel$year, residuals(mod.ols), type="o", pch=16,
     xlab="Year", ylab="OLS Residuals")
abline(h=0, lty=2)

par(mfrow=c(1, 2))
acf(residuals(mod.ols))
acf(residuals(mod.ols), type="partial")

require(forecast)
auto.arima(residuals(mod.ols))



library(nlme)
mod.gls <- gls(fconvict ~ tfr + partic +  mconvict,
               data=Hartnagel, correlation=corARMA(p=2), method="ML")
summary(mod.gls)

my.gls <- gls(fconvict ~ tfr + partic +  mconvict,
               data=Hartnagel, correlation=corARMA(p=1, q=1), method="ML")
summary(my.gls)

mod.gls.0 <- update(mod.gls, correlation=NULL)

anova(mod.gls, my.gls)
anova(mod.gls, mod.gls.0)

par(mfrow=c(1, 3))
plot(Hartnagel$year, residuals(mod.ols), type="o", pch=16,
     xlab="Year", ylab="OLS Residuals")
abline(h=0, lty=2)
plot(Hartnagel$year, residuals(mod.gls), type="o", pch=16,
     xlab="Year", ylab="gls Residuals")
abline(h=0, lty=2)
plot(Hartnagel$year, residuals(my.gls), type="o", pch=16,
     xlab="Year", ylab="gLS Residuals")
abline(h=0, lty=2)


plot(mod.gls)
plot(my.gls)

