# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|

try(setwd("./G1_FH"), silent = TRUE)                    # Se comprueba la ruta de carga de archivos
                                                        # Try() permite que se siga con la ejecución incluso si se encuentra un error

high_temp_P = list.files()[c(2*(1:15))]                 # Se cargan los paths de los archivos, me aprovecho de cómo se guardaron para
low_temp_P = list.files()[c(2*(0:14) + 1)]              # seleccionar cada toma de datos tomada



# Cargo los archivos aplicando la función anónima a cada elemento de las listas de paths. Lapply regresa una lista con la evalución de
# la función por cada elemento. La función se encarga de leer los datos en cada archivo y convertirlo a un DataFrame, por lo que cada 
# evaluación es un DataFrame, y así, la lista entera Lapply es la lista con los DataFrames de los datos de cada path.
# lapply("Lista de paths", function("path individual") "función de carga de datos")

high_temp_D = lapply(high_temp_P, function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", skip = 1))
low_temp_D = lapply(low_temp_P, function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", skip = 1))
rm(high_temp_P, low_temp_P)






# |---------------------------------|
# | FUNCIÓN PARA HALLAR LOS MÁXIMOS |
# |---------------------------------|

# Se recorren todos los datos en data, seleccionando una lista desde el punto x hasta el punto x + 2*treshold (así, el punto x + treshold
# es el punto medio de la selección). Si este punto medio en la selección tiene el valor en I más alto, entonces es un pico, por lo tanto, 
# se guarda. Se almacena este valor I para las siguientes iteraciones y se ignoran los siguientes puntos con el mismo valor de I.
# Se devuelven todas las filas con los picos hallados.
findMaxValues <- function(data, th)
{
  treshold = th
  last_data = strtoi(row.names(tail(data[[1]], 1)))
  
  max_D = list()
  last_max_value = 0

  for (x in 20:(last_data - 2*treshold))
  {
    if (max(data[[1]][x:(x + 2*treshold), 2]) == data[[1]][x + treshold, 2] && data[[1]][x + treshold, 2] != last_max_value)
    {
      max_D <- append(max_D, list(data[[1]][x + treshold, ]))
      last_max_value = data[[1]][x + treshold, 2]
    }
  }
  
  return(max_D)
}

# Bloque para corroborar los picos que se buscan
# Para valores de alta temperatura, conviene usar un treshold = 15
# Para valores de baja temperatura, conviene usar un treshold = 5
# for (i in 1:15)
# {
#   max_D = findMaxValues(low_temp_D[i], 5)
# 
#   plot(low_temp_D[[i]])
#   for (maxval in max_D)
#   {
#     abline(v = maxval[[1]])
#   }
# }
# rm(maxval)



# |-------------------------------------------------|
# | FUNCIÓN PARA HALLAR LA INCERTIDUMBRE EN UN PICO |
# |-------------------------------------------------|
# La incertidumbre se halla promediando la diferencia en V entre los datos contiguos al punto pico.
findError <- function(peak, data)
{
  peak_row = strtoi(rownames(peak))
  prev_V = data[peak_row - 1, ][[1]]
  pos_V = data[peak_row + 1, ][[1]]
  
  return(mean(c(abs(peak[[1]] - prev_V), abs(peak[[1]] - pos_V))))
}


# |-----------------------------------------------------|
# | FUNCIÓN PARA PROPAGAR LA INCERTIDUMBRE EN PRODUCTOS |
# |-----------------------------------------------------|
propProdErrors <- function(res, rval, eval){return(abs(res * sqrt(sum((eval/ rval)^2))))}
propSumErrors <- function(eval){return(sqrt(sum(eval^2)))}
propEscErrors <- function(c, eval){return(abs(c)*eval)}



# |-----------------------------------------|
# | SE ENCUENTRAN LOS VALORES DE LOS PICOS  |
# |-----------------------------------------|
low_temp_maxValues = lapply(low_temp_D, function(data) findMaxValues(list(data), 5))
high_temp_maxValues = lapply(high_temp_D, function(data) findMaxValues(list(data), 15))



# |------------------------|
# | HALLAR EL PRIMER PICO  |
# |------------------------|
first_peak_D = unlist(lapply(low_temp_maxValues, function(data) data[[1]][[1]]))

# |------------------------------------------|
# | Encontrar incertidumbre del primero pico |
# |------------------------------------------|
# Incertidumbre para el primer pico. Primero, se combinan todas las incertidumbres encontradas para los
# primeros picos en una sola lista. Luego, se encuentra el error de esa suma y finalmente el error del 
# factor de escala dado por el cociente del promedio.

f_peak_errors = c()
for (i in 1:15) {
  error = findError(low_temp_maxValues[[i]][[1]], low_temp_D[[i]])
  f_peak_errors <- append(f_peak_errors, error)
}

sum_error = propSumErrors(f_peak_errors)      # Se encuentra el error de la suma, que se trata como
                                              # como el error de la variable X en el escalamiento

mean_error = propEscErrors((1/15), sum_error)

rm(f_peak_errors, i, error, sum_error)

print(paste("Promedio para el valor del primer pico de voltaje: ", mean(first_peak_D)))
print(paste("Desviación para el valor del primer pico de voltaje: ", sd(first_peak_D)))


# |-------------------------------------------------------------|
# | FUNCIÓN PARA HALLAR EL ESPACIAMIENTO ENTRE PICOS DE VOLTAJE |
# |-------------------------------------------------------------|
# Se toman todos los datos de los picos para cada toma de datos y se almacenan las diferencias entre picos
# en una lista. Igualmente, la incertidumbre se halla propagando la incertimbre de la resta de los picos y
# hallando la incertidumbre de su promedio.

peaks_difference = c()
peaks_difference_error = c()
counter = 1
for (measure in high_temp_maxValues) {
  for (peak in 4:2){
    peaks_difference <- append(peaks_difference, measure[[peak]][1] - measure[[peak - 1]][1])
    
    error_1 = findError(measure[[peak]], high_temp_D[[counter]])
    error_2 = findError(measure[[peak - 1]], high_temp_D[[counter]])
    
    peaks_difference_error <- append(peaks_difference_error, propSumErrors(c(error_1, error_2)))
  }
  
  counter = counter + 1
}
peaks_difference = unlist(peaks_difference)

# Se halla finalmente el error del promedio
mean_error = propEscErrors((1/15), propSumErrors(peaks_difference_error))

rm(measure, peak, error_1, error_2, peaks_difference_error, counter)

# print(paste("Diferencia promedio entre picos en temperatura alta: ", mean(peaks_difference)))
# print(paste("Desv. Est de la diferencia entre picos en temperatura alta: ", sd(peaks_difference)))






