# Para cargar los datos
datos <- read.table(file='http://tinyurl.com/hwhb769', header=T)

pairs(datos[, c('precio', 'mt2', 'alcobas', 'banos',
                'administracion', 'avaluo')])


with(datos, plot(x=mt2, y=precio))
with(datos, plot(x=administracion, y=precio))
with(datos, plot(x=avaluo, y=precio))


with(datos, plot(x=alcobas, y=precio))
identify(x=datos$alcobas, y=datos$precio)

