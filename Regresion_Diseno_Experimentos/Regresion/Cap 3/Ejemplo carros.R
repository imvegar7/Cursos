
# Ejemplo con Cars93

# -------------------------------------------------------------------------
# Lectura de la base de datos ---------------------------------------------
# -------------------------------------------------------------------------

require(MASS)
Cars93

datos <- Cars93[, c(7, 12, 13, 19, 21, 25)] # Algunos regresores

# -------------------------------------------------------------------------
# Matriz de correlaciones entre las variables -----------------------------
# -------------------------------------------------------------------------
require(corrplot)
M <- cor(datos)
corrplot(M, method="ellipse")
corrplot.mixed(M)

# -------------------------------------------------------------------------
# Diagramas de dispersion en 2d -------------------------------------------
# -------------------------------------------------------------------------
pairs(datos)

# Mejorando el grafico
panel.reg <- function (x, y)
{
  points(x, y, pch=20)
  abline(lm(y ~ x), lwd=2, col='dodgerblue2')
}
# Funci?n para crear el histograma
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="dodgerblue2", ...)
}
# Funci?n para obtener la correlaci?n
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}

pairs(datos,
      upper.panel = panel.reg,
      diag.panel = panel.hist,
      lower.panel = panel.cor)

# -------------------------------------------------------------------------
# Densidad de MPG.city ----------------------------------------------------
# -------------------------------------------------------------------------
plot(density(datos$MPG.city), lwd=4, main='', las=1)

# -------------------------------------------------------------------------
# Modelo ajustado ---------------------------------------------------------
# -------------------------------------------------------------------------

modfull <- lm(MPG.city ~ ., data=datos)
summary(modfull)

modred <- lm(MPG.city ~ Weight, data=datos)

anova(modfull, modred)
summary(modred)

with(datos, plot(x=Weight, y=MPG.city, las=1, pch=19))
abline(modred, col='red', lwd=4)

modfinal <- lm(MPG.city ~ Weight + I(Weight^2), data=datos)
summary(modfinal)

lines(sort(datos$Weight), fitted(modfinal)[order(datos$Weight)],
      col='blue', lwd=4) 

legend('topright', col=c('red', 'blue'), lty=1, bty='n',
       legend=c('Lineal', 'Cuadratico'))


