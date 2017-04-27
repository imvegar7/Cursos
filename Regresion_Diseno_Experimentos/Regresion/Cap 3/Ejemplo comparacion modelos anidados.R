require(MPV)             # Para cargar MPV
data(table.b4)           # Para usar b4
table.b4                 # Para ver base

mod1 <- lm(y~x1+x2, data=table.b4)
mod2 <- lm(y~x1+x2+x3+x4, data=table.b4)

summary(mod1)
summary(mod2)

anova(mod1, mod2)

