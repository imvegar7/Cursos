# Ejercicio 5.1

temp <- c(24.9, 35, 44.9,55.1,65.2,75.2,85.2,95.2)
visc <- c(1.133,0.9772,0.8532,0.7550,0.6723,0.6021,0.5420,0.5074)

# a)
plot(x=temp, y=visc, pch=19)

# b)
mod1 <- lm(visc ~ temp)
summary(mod1)

# c)
plot(mod1, which=1)

# d)
visc.t <- log(visc)
mod2 <- lm(visc.t ~ temp)
summary(mod2)
plot(mod2, which=1)

