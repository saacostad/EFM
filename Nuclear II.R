# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|
try(setwd("./Datos"), silent = TRUE)                         # Se comprueba la ruta de carga de archivos



# Datos para el PLOMO
leadTempData = read.table("plomo_datos.txt", header = TRUE, sep = "\t", dec = ",", col.names = rep(c("E", "Counts"), 8))
leadData = lapply(1:7, function(i) leadTempData[c(1 + 2*i, 2 + 2*i)])
for (i in 1:7) {
  colnames(leadData[[i]]) = c("E", "C")
}


# Datos para el ACERO
stealTempData = read.table("acero_datos.txt", header = TRUE, sep = "\t", dec = ",", col.names = rep(c("E", "Counts"), 8))
stealData = lapply(1:8, function(i) stealTempData[c(2*i - 1, 2*i)])
for (i in 1:8) {
  colnames(stealData[[i]]) = c("E", "C")
}


# Datos de CALIBRACION
calibrationData = leadTempData[1:2]


# Datos de INTEGRACION
leadIntegrationData = read.table("plomo_areas.txt", header = TRUE, sep = "\t", dec = ",", col.names = c("d", "I"))
aceroIntegrationData = read.table("acero_areas.txt", header = TRUE, sep = "\t", dec = ",", col.names = c("d", "I"))

rm(dataSet, leadTempData, stealTempData)

