
require(MASS)

head(Cars93[, c('Price', 'EngineSize', 'Type')])

# Forma 1
mod <- lm(Price ~ EngineSize + Type, data=Cars93)
summary(mod)

# Forma 2
anova(mod)

# Forma 3
mod.redu <- lm(Price ~ EngineSize, data=Cars93)
anova(mod, mod.redu)
      