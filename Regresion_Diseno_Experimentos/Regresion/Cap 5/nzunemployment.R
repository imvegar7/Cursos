# Loading packages
library(lattice)       # Fancy graphics
library(nlme)          # Generalized linear mixed models 

# Reading data

url <- "https://dl.dropboxusercontent.com/u/9602072/nzunemployment.csv"
un <- read.table(file=url, sep=",", header=TRUE)

# Plotting first youth data and then adding adults
# as time series
with(un,
     {
       plot(youth ~ q, type='l', ylim=c(0,30), col='red', lwd=3,
            xlab='Quarter', ylab='Percentage unemployment')
       lines(q, adult, lty=2, col='blue', lwd=3)
       legend('topleft', c('Youth', 'Adult'), lty=c(1, 2), 
              col=c('red', 'blue'), bty='n', lwd=3)
       abline(v=90)
     })


# Creating minimum wage policy factor
un$minwage <- ifelse(un$q < 90, 'Before90', 'After90')
un$minwage <- as.factor(un$minwage)
levels(un$minwage) <- c('Before90', 'After90')

# Scatterplot
plot(youth ~ adult, data=un, pch=19, las=1)

# And a scatterplot
xyplot(youth ~ adult, group=minwage, data=un, pch=19,
       type=c('p', 'r'), auto.key=TRUE)


# Linear regression accounting for change of policy
mod1 <- lm(youth ~ adult * minwage, data=un)
summary(mod1)

par(mfrow=c(2, 2))
plot(mod1)     # Plots residuals for the model fit

plot(residuals(mod1), type='l')

# Centering continuous predictor
un$cadult <- with(un, adult - mean(adult))
mod2 <- lm(youth ~ cadult * minwage, data=un)
summary(mod2)

par(mfrow=c(2, 2))
plot(mod2)     # Plots residuals for the model fit

par(mfrow=c(1, 2))
acf(mod2$res)  # Plots autocorrelation of the residuals
pacf(mod2$res) # Plots partial autocorrelation of the residuals

# Now we move to use nlme

# gls() is an nlme function when there are no random effects
mod3 <- gls(youth ~ cadult * minwage, data=un)
summary(mod3)
intervals(mod3, level=0.95)
plot(mod3, pch=19)

# Adding autocorrelation
mod4 <- gls(youth ~ cadult * minwage, 
            correlation=corAR1(form=~1), data=un)
summary(mod4)
intervals(mod4, level=0.95)
plot(mod4, pch=19)


anova(mod3, mod4)


