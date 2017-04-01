
# Objective ---------------------------------------------------------------

# We are going to simulate a dataset with different variance 
# between treatments, the objective is to explore if residual
# analysis can identify departures of homogeniety

# Simulating dataset ------------------------------------------------------
a <- 10
n <- 20
N <- a * n

media <- 150
desvi <- rep(1:a, each=n)

# To explore the mean and standard deviation
cbind(media, desvi)

# Simulating response
y <- rnorm(n=N, mean=media, sd=desvi)

# Treatment indicator
treat <- rep(letters[1:a], each=n)

# Boxplot to explore the dataset
par(mfrow=c(2, 2))
boxplot(y ~ treat, xlab='Treatment', ylab='Response')

# Using anova -------------------------------------------------------------
mod <- aov(y ~ treat)

# Residual analysis -------------------------------------------------------
boxplot(residuals(mod) ~ treat,
        xlab='Treatment', ylab=expression(e[ij]))

plot(mod, 1)
plot(mod, 3)
