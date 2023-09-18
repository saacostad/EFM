# |-------------------------------------|
# |   FUNCIONES PARA TRABAJAR ANGULOS   |
# |-------------------------------------|

grad_to_rad <- function(angle, minutes)
{
  return((angle + (minutes/60)) * (pi / 180))
}




# |-------------------------|
# |   SE CREAN LOS DATOS    |
# |-------------------------|

# Se guardan los datos del experimento con el mercurio
mercury_data = list(
  ang=list(7, 8, 9, 10, 10, 15, 17, 19, 20, 20, 7, 8, 9, 10, 10, 15, 17, 19, 20, 20),
  min=list(35, 36, 33, 5, 7, 20, 23, 22, 30, 35, 31, 30, 28, 0, 2, 12, 13, 13, 21, 26),
  ord=list(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, -1, -1, -1, -1, -1, -2, -2, -2, -2, -2),
  col=list("violet", "dark green", "light green", "dark yellow", "light yellow", 
           "violet", "dark green", "light green", "dark yellow", "light yellow",
           "violet", "dark green", "light green", "dark yellow", "light yellow",
           "violet", "dark green", "light green", "dark yellow", "light yellow")
)

# Se convierten los datos a un dataframe
mercury <- as.data.frame(do.call(cbind, mercury_data))
rm(mercury_data)

mercury_wavelenghts = c(406.215, 493.835, 546.07, 576.96, 579.06)

# Se guardan los datos del experimento con el hidrógeno
hidrogen_data = list(
  ang=list(7, 8, 11, 7, 8, 11, 15, 17, 23, 15, 17, 23),
  min=list(30, 24, 24, 34, 28, 29, 17, 10, 30, 10, 2, 19),
  ord=list(-1, -1, -1, 1, 1, 1, 2, 2, 2, -2, -2, -2),
  col=list("violet", "blue", "red",
           "violet", "blue", "red",
           "violet", "blue", "red",
           "violet", "blue", "red")
)

# Se convierten los datos a un dataframe
hidrogen <- as.data.frame(do.call(cbind, hidrogen_data))
rm(hidrogen_data)

# |-------------------------------|
# |   SE ESTIMA LA CONSTANTE D    |
# |-------------------------------| 
# Primero, se hallan los promedios de los ángulos para cada orden (-1 y 1, -2 y 2),
# y con respectivos promedios, hacer la gráfica y hallar los valores para d


# Obtención de los datos promedio para cada orden
mean_angle_first_order = c()

for (i in 1:5) {
  mean_rad = mean(c(
    grad_to_rad(unlist(mercury[mercury$ord == -1, c(1, 2)][[1]])[i], unlist(mercury[mercury$ord == -1, c(1, 2)][[2]])[i]),
    grad_to_rad(unlist(mercury[mercury$ord == 1, c(1, 2)][[1]])[i], unlist(mercury[mercury$ord == 1, c(1, 2)][[2]])[i])
  ))
  
  mean_angle_first_order <- append(mean_angle_first_order, mean_rad)
}


mean_angle_second_order = c()

for (i in 1:5) {
  mean_rad = mean(c(
    grad_to_rad(unlist(mercury[mercury$ord == -2, c(1, 2)][[1]])[i], unlist(mercury[mercury$ord == -2, c(1, 2)][[2]])[i]),
    grad_to_rad(unlist(mercury[mercury$ord == 2, c(1, 2)][[1]])[i], unlist(mercury[mercury$ord == 2, c(1, 2)][[2]])[i])
  ))
  
  mean_angle_second_order <- append(mean_angle_second_order, mean_rad)
}

rm(i, mean_rad)

# Se unen ambos nuevos ordenes
mercwav2 = 2*mercury_wavelenghts
ap_wave = append(mercury_wavelenghts, mercwav2)
ap_sin = append(sin(mean_angle_first_order), sin(mean_angle_second_order))

# Gráfica de los datos promedio
par(mar=c(5,6,4,1)+.1)
plot(sin(mean_angle_second_order), 2*mercury_wavelenghts, col = 2, ylim = c(0, 1200), xlim = c(0, 0.5),
     xlab = "sin(theta)",
     ylab = "m lambda [nm]",
     cex = 2,
     cex.lab = 2.5,
     cex.axis = 2.5,
     pch = 2,
     lwd = 2)
points(sin(mean_angle_first_order), mercury_wavelenghts, col = 1, cex = 2, lwd = 2)
abline(lm(mercury_wavelenghts ~ sin(mean_angle_first_order)), col = 1)
abline(lm(2*mercury_wavelenghts ~ sin(mean_angle_second_order)), col = 2)
abline(lm(ap_wave ~ ap_sin), col = 3, lwd = 3)

legend("bottomright", legend=c("reg. promedio m = abs(1)", "reg. promedio m = abs(2)", "reg. ambos órdenes"),
       col=c(1:3), cex = 2.5, lwd = 2)

# Impresion de los valores de d
d = lm(ap_wave ~ ap_sin)$coefficients[[2]]
print(paste("Valor para d: ", d))






# |---------------------------|
# |   TRABAJO CON HIDROGENO   |
# |---------------------------|
# Se empieza eliminando componentes basura que ya no se han de utilizar
rm(mercury, mean_angle_first_order, mean_angle_second_order, mercury_wavelenghts, ap_sin, ap_wave, mercwav2)


# Se obtienen las diferentes longitudes de onda para el hidrógeno
# Obtención de los datos promedio para cada orden
mean_angle_first_order = c()

for (i in 1:3) {
  mean_rad = mean(c(
    grad_to_rad(unlist(hidrogen[hidrogen$ord == -1, c(1, 2)][[1]])[i], unlist(hidrogen[hidrogen$ord == -1, c(1, 2)][[2]])[i]),
    grad_to_rad(unlist(hidrogen[hidrogen$ord == 1, c(1, 2)][[1]])[i], unlist(hidrogen[hidrogen$ord == 1, c(1, 2)][[2]])[i])
  ))
  
  mean_angle_first_order <- append(mean_angle_first_order, mean_rad)
}


mean_angle_second_order = c()

for (i in 1:3) {
  mean_rad = mean(c(
    grad_to_rad(unlist(hidrogen[hidrogen$ord == -2, c(1, 2)][[1]])[i], unlist(hidrogen[hidrogen$ord == -2, c(1, 2)][[2]])[i]),
    grad_to_rad(unlist(hidrogen[hidrogen$ord == 2, c(1, 2)][[1]])[i], unlist(hidrogen[hidrogen$ord == 2, c(1, 2)][[2]])[i])
  ))
  
  mean_angle_second_order <- append(mean_angle_second_order, mean_rad)
}

rm(i, mean_rad)

# Dado que se tienen dos valores (ordenes 1 y 2 promedio) para cada longitud de onda esperada, la longitud de onda será el promedio
# de lambda = d/m sin(theta) para ambos valores obtenidos
h_wavelenghts = c()
for (i in 3:1) {
  h_wavelenghts <- append(h_wavelenghts, mean(c( d*sin(mean_angle_first_order[i]), d/2 * sin(mean_angle_second_order[i])  )))
}

# Esperamos observar las 3 primeras
# líneas del espectro de Balmer
ns = c(1/9, 1/16, 1/25)                           # 1/n^2 de las líneas respectivas
inv_wavelengths = 1/(h_wavelenghts * 10^(-9))     # 1/lambda (ajustando las respectivas unidades)

# Se hace la respectiva gráfica
par(mar=c(5,6,4,1)+.1)
plot(ns, inv_wavelengths, col = 1,
     xlab = "1/n^2",
     ylab = "1/lambda [1/m]",
     cex = 2,
     cex.lab = 2.5,
     cex.axis = 2.5,
     lwd = 2)
abline(lm(inv_wavelengths ~ ns), lwd = 2)

# Se imprime el resultado 
model = lm(inv_wavelengths ~ ns)
Rh = -model$coefficients[[2]]

print(paste("Estimación para la R_H: ", Rh))
print(paste("Valor real R_H:", 10973731))
print(paste("Error relativo R_h:", 1 - Rh/(1.0973731 * 10^7)))


# Se imprimen otros resultados
print("Longitudes de onda estimadas: ")
print(paste("Valor estimado: ", h_wavelenghts[1], "  Valor real: 656.3  Color: Rojo"))
print(paste("Valor estimado: ", h_wavelenghts[2], "  Valor real: 486.1  Color: Azul-verde"))
print(paste("Valor estimado: ", h_wavelenghts[3], "  Valor real: 434.1  Color: Violeta"))
