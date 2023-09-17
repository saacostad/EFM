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

# Gráfica de los datos promedio
plot(sin(mean_angle_second_order), 2*mercury_wavelenghts, col = 2, ylim = c(0, 1200), xlim = c(0, 0.5))
points(sin(mean_angle_first_order), mercury_wavelenghts, col = 1)
abline(lm(mercury_wavelenghts ~ sin(mean_angle_first_order)), col = 1)
abline(lm(2*mercury_wavelenghts ~ sin(mean_angle_second_order)), col = 1)


# Obtención de d a partir de la regresión lineal
mercwav2 = 2*mercury_wavelenghts
model_1 = lm(sin(mean_angle_first_order) ~ mercury_wavelenghts)
model_2 = lm(sin(mean_angle_second_order) ~ mercwav2)

print(paste("Valor de d para orden 1: ", model_1$coefficients[[2]]))
print(paste("Valor de d para orden 2: ", model_2$coefficients[[2]]))
print(paste("Promedio valor de d: ", mean(c(model_1$coefficients[[2]], model_2$coefficients[[2]]))))



