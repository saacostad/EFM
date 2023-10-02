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



# |---------------------------------------|
# | FUNCIONES PARA PROPAGAR INCERTIDUMBRE |
# |---------------------------------------|
propProdErrors <- function(res, rval, eval){return(abs(res * sqrt(sum((eval/ rval)^2))))}
propSumErrors <- function(eval){return(sqrt(sum(eval^2)))}
propEscErrors <- function(c, eval){return(abs(c)*eval)}

# Función para hallar el mejor estimador del promedio de una serie de estimaciones experimentales. 
# Promedio ponderado con pesos la incertidumbre de la medición
meanExpData <- function(rval, eval)
{
  meanval = sum(rval / (eval^2)) / sum(1 / eval^2)
  errval = 1 / sqrt(sum(1 / eval^2))
  
  return(c(meanval, errval))
}






# |-----------------------------------------|
# | SE ENCUENTRAN LOS VALORES DE LOS PICOS  |
# |-----------------------------------------|
low_temp_maxValues = lapply(low_temp_D, function(data) findMaxValues(list(data), 5))
high_temp_maxValues = lapply(high_temp_D, function(data) findMaxValues(list(data), 15))






# |------------------------|
# | HALLAR EL PRIMER PICO  |
# |------------------------|
# Se encuentran los valores del primero pico para los datos tomados a temperaturas bajas
first_peak = unlist(lapply(low_temp_maxValues, function(data) data[[1]][[1]]))

# |------------------------------------------|
# | Encontrar incertidumbre del primero pico |
# |------------------------------------------|
# Incertidumbre para el primer pico. Primero, se combinan todas las incertidumbres encontradas para los
# primeros picos en una sola lista.
first_peak_errors = c()
for (i in 1:15) {
  error = findError(low_temp_maxValues[[i]][[1]], low_temp_D[[i]])
  first_peak_errors <- append(first_peak_errors, error)
}
rm(i, error)

first_peak_estimator = meanExpData(first_peak, first_peak_errors)





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
    peaks_difference <- append(peaks_difference, measure[[peak]][[1]] - measure[[peak - 1]][[1]])
    
    error_1 = findError(measure[[peak]], high_temp_D[[counter]])
    error_2 = findError(measure[[peak - 1]], high_temp_D[[counter]])
    
    peaks_difference_error <- append(peaks_difference_error, propSumErrors(c(error_1, error_2)))
  }
  
  counter = counter + 1
}
rm(counter, error_1, error_2, measure, peak)

# Se halla finalmente el error del promedio
peak_difference_estimator = meanExpData(peaks_difference, peaks_difference_error)






# |-------------------|
# | ENTREGA DE DATOS  |
# |-------------------|

# Impresión de los resultados principales
print(paste("Promedio para el valor del primer pico de voltaje: ", first_peak_estimator[1], " \\pm ", first_peak_estimator[2]))
print(paste("Diferencia promedio entre picos en temperatura alta: ", peak_difference_estimator[1], " \\pm ", peak_difference_estimator[2]))



# |---------------------|
# | Ploteo de gráficas  |
# |---------------------|
# Gráfica de todos los datos recolectados para bajas temperaturas
par(mar=c(5,6,4,1)+.1)
plot(low_temp_D[[1]],
     xlab = "U_a [V]",
     ylab = "I [ua]",
     cex = 0.5,
     cex.lab = 2,
     cex.axis = 2,
     lwd = 2)
for (i in 2:15) {
  points(low_temp_D[[i]],
         cex = 0.5, 
         col = i,
         pch = i,
         lwd = 2)
}

# Gráfica de dato máximo baja temperatura
par(mar=c(5,6,4,1)+.1)
plot(low_temp_D[[5]],
     xlab = "U_a [V]",
     ylab = "I [ua]",
     cex = 1.5,
     cex.lab = 2,
     cex.axis = 2,
     lwd = 2)
points(low_temp_D[[5]][ (strtoi(row.names(low_temp_maxValues[[5]][[1]])) - 5) : (strtoi(row.names(low_temp_maxValues[[5]][[1]]))+5), ],
       cex = 1.5, lwd = 2, col = 2)
points(low_temp_D[[5]][ (strtoi(row.names(low_temp_maxValues[[5]][[2]])) - 5) : (strtoi(row.names(low_temp_maxValues[[5]][[2]]))+5), ],
       cex = 1.5, lwd = 2, col = 2)
abline(v = c(low_temp_maxValues[[5]][[1]][[1]], low_temp_maxValues[[5]][[2]][[1]]), lwd = 2)



# Gráfica de todos los datos recolectados para altas temperaturas
par(mar=c(5,6,4,1)+.1)
plot(high_temp_D[[1]],
     xlab = "U_a [V]",
     ylab = "I [ua]",
     cex = 0.5,
     cex.lab = 2,
     cex.axis = 2,
     ylim = c(0, 3))
for (i in 2:15) {
  points(high_temp_D[[i]],
         cex = 0.5, 
         col = i,
         pch = i)
}

# Gráfica de dato máximo alta temperatura
par(mar=c(5,6,4,1)+.1)
plot(high_temp_D[[5]],
     xlab = "U_a [V]",
     ylab = "I [ua]",
     cex.lab = 2,
     cex.axis = 2,
     lwd = 2)
points(high_temp_D[[5]][ (strtoi(row.names(high_temp_maxValues[[5]][[1]])) - 15) : (strtoi(row.names(high_temp_maxValues[[5]][[1]]))+15), ],
       cex = 1.5, lwd = 2, col = 2)
points(high_temp_D[[5]][ (strtoi(row.names(high_temp_maxValues[[5]][[2]])) - 15) : (strtoi(row.names(high_temp_maxValues[[5]][[2]]))+15), ],
       cex = 1.5, lwd = 2, col = 2)
points(high_temp_D[[5]][ (strtoi(row.names(high_temp_maxValues[[5]][[3]])) - 15) : (strtoi(row.names(high_temp_maxValues[[5]][[3]]))+15), ],
       cex = 1.5, lwd = 2, col = 2)
points(high_temp_D[[5]][ (strtoi(row.names(high_temp_maxValues[[5]][[4]])) - 15) : (strtoi(row.names(high_temp_maxValues[[5]][[4]]))+15), ],
       cex = 1.5, lwd = 2, col = 2)
abline(v = c(high_temp_maxValues[[5]][[1]][[1]], high_temp_maxValues[[5]][[2]][[1]], high_temp_maxValues[[5]][[3]][[1]], high_temp_maxValues[[5]][[4]][[1]]), lwd = 2)

rm(i)


# |---------|
# | TABLAS  |
# |---------|
# Tabla para los datos encontrados del primer pico
print("")
print("TABLA DE DATOS PARA EL PRIMER PICO")
print("")
print("Medida & U_a [V] \\")

for (i in 1:15) {
  print(paste(i, " & ", signif(first_peak[i], 3), " \\pm ", signif(first_peak_errors[i], 2), " \\"))
}

# Tabla de datos para la diferencia entre picos
print("")
print("TABLA DE DATOS PARA DIFERENCIAS ENTRE PICOS")
print("")

print("Medida & p4 - p3 & p3 - p2 & p2 - p1 \\")
for (i in 0:14) {
  print(paste(i, " & ", 
              signif(peaks_difference[i*3 + 1], 3), " \\pm ", signif(peaks_difference_error[i*3 + 1], 2), " & ",
              signif(peaks_difference[i*3 + 2], 3), " \\pm ", signif(peaks_difference_error[i*3 + 2], 2), " & ",
              signif(peaks_difference[i*3 + 3], 3), " \\pm ", signif(peaks_difference_error[i*3 + 3], 2),
              " \\"))
}

rm(i)
