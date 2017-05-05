# Example 7.2 of MPV

# The data
drop <- c(8.33, 8.23, 7.17, 7.14, 7.31, 7.60, 7.94, 8.30, 8.76, 8.71, 9.71,
          10.26, 10.91, 11.67, 11.76, 12.81, 13.30, 13.88, 14.59,
          14.05, 14.48, 14.92, 14.37, 14.63, 15.18, 14.51, 14.34, 
          13.81, 13.79, 13.05, 13.04, 12.60, 12.05, 11.15, 11.15, 
          10.14, 10.08,9.78,9.80,9.95,9.51)
time <- seq(from=0, to=20, by=0.5)
dat <- data.frame(time=time, drop=drop)

# Scatterplot figure 7.6
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19, 
     ylim=c(0,15), main="Figure 7.6")

abline(v=6.5, lty="dotted", col='tomato')
abline(v=13, lty="dotted", col='tomato')

## First try cubic polynomials
g3 <- lm(drop ~ time + I(time^2) + I(time^3), data=dat)
summary(g3)

# Now try cubic spline fitting
xplus <- function(x) ifelse(x>=0, x, 0)  # Auxiliar function
time6.5 <- xplus(time-6.5)
time13 <- xplus(time-13)
gs <- lm(drop ~ time + I(time^2) + I(time^3) + I(time6.5^3) + I(time13^3), data=dat)
summary(gs)

# Adding fitted curves to the scatterplot
plot(dat, ylab="Voltage drop", xlab="Time (seconds)", pch=19,
     ylim=c(0,15), main="Scatterplot with fitted models")
i <- order(time)
lines(time[i], fitted(gs)[i], col=4, lty=1, lwd=3)
lines(time[i], fitted(g3)[i], col=2, lty=2, lwd=3)
legend("bottomright", lwd=3, lty=1:2, col=c(4,2),
       legend=c("Cubic spline model", "Cubic polynomial model"), bty="n")

# Residual plots
par(mfrow=c(1,2))
plot(g3, 1, main="Cubic polynomial model", pch=20)
plot(gs, 1, main="Cubic spline model", pch=20)

# Comparing the models
anova(g3, gs)


