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
#     print(maxval[[1]])
#     abline(v = maxval[[1]])
#   }
# }


# Se crean las listas con los valores máximos encontrados
low_temp_maxValues = lapply(low_temp_D, function(data) findMaxValues(list(data), 5))
high_temp_maxValues = lapply(high_temp_D, function(data) findMaxValues(list(data), 15))

# |------------------------|
# | HALLAR EL PRIMER PICO  |
# |------------------------|

first_peak_D = unlist(lapply(low_temp_maxValues, function(data) data[[1]][[1]]))

print(paste("Promedio para el valor del primer pico de voltaje: ", mean(first_peak_D)))
print(paste("Desviación para el valor del primer pico de voltaje: ", sd(first_peak_D)))



# |-------------------------------------------------------------|
# | FUNCIÓN PARA HALLAR EL ESPACIAMIENTO ENTRE PICOS DE VOLTAJE |
# |-------------------------------------------------------------|
peaks_difference = c()
for (measure in high_temp_maxValues) {
  for (peak in 4:2){
    peaks_difference <- append(peaks_difference, measure[[peak]][1] - measure[[peak - 1]][1])
  }
}
peaks_difference = unlist(peaks_difference)

print(paste("Diferencia promedio entre picos en temperatura alta: ", mean(peaks_difference)))
print(paste("Desv. Est de la diferencia entre picos en temperatura alta: ", sd(peaks_difference)))




