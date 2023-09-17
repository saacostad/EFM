# |-----------------------------------|
# |   FUNCIONES OPERACIONES MOD 60    |
# |-----------------------------------|


suma60 <- function(angle1, angle2)
{
  angle = angle1[1] + angle2[1] + trunc(((angle1[2] + angle2[2])/60))
  minutes = (angle1[2] + angle2[2]) %% 60
  return(c(angle, minutes))
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
           "violet", "dark green", "light green", "dark yellow", "light yellow",)
)

# Se convierten los datos a un dataframe
mercury <- as.data.frame(do.call(cbind, mercury_data))



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

