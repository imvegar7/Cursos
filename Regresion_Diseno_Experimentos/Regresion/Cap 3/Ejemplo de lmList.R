
require(nlme)  # Paquete que contiene lmList

Orthodont  # los datos

plot(Orthodont)

OrthoFem <- Orthodont[ Orthodont$Sex == "Female", ]
plot(OrthoFem)

mod <- lmList(distance ~ age | Subject, data = OrthoFem)

mod


