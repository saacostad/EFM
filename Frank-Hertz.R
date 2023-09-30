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

high_temp_D = lapply(high_temp_P, function(path) list(read.table(high_temp_P[1], header = TRUE, sep = "\t", dec = ",", skip = 1)))
low_temp_D = lapply(low_temp_P, function(path) list(read.table(high_temp_P[1], header = TRUE, sep = "\t", dec = ",", skip = 1)))




