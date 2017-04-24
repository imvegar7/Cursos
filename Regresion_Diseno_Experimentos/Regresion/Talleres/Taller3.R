# Para usar la tabla B3
require(MPV)
table.b3

# Punto 1
with(table.b3, plot(x=x10, y=y))

# Punto 2
with(table.b3, cor(y, x10))

# Punto 3
with(table.b3, cor.test(y, x10))

# Punto 4
mod <- lm(y ~ x10, data=table.b3)
summary(mod)

# Punto 5
# Beta0 no se interpreta
# Beta1: por cada 1000 libras adicionales en un carro, se espera que el
# rendimiento disminuya en 5.75 millas por galon.

# Punto 6
# El auto B rendiria 5.75 millas por galon menos que A.

# Punto 7
# 2.12e-10

# Punto 8
require(model)
beta.test(model=mod, param='x10',
          ref.value=-0.006, alternative='less')


