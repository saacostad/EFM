# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|
try(setwd("./Datos"), silent = TRUE)                         # Se comprueba la ruta de carga de archivos



# Datos para el PLOMO
leadTempData = read.table("plomo_datos.txt", header = TRUE, sep = "\t", dec = ",", col.names = rep(c("E", "Counts"), 8), skip = 5)
leadData = lapply(1:7, function(i) leadTempData[c(1 + 2*i, 2 + 2*i)])
for (i in 1:7) {
  colnames(leadData[[i]]) = c("E", "C")
  leadData[[i]] = head(leadData[[i]], -5)
}


# Datos para el ACERO
steelTempData = read.table("acero_datos.txt", header = TRUE, sep = "\t", dec = ",", col.names = rep(c("E", "Counts"), 8), skip = 5)
steelData = lapply(1:8, function(i) steelTempData[c(2*i - 1, 2*i)])
for (i in 1:8) {
  colnames(steelData[[i]]) = c("E", "C")
  steelData[[i]] = head(steelData[[i]], -5)
}


# Datos de CALIBRACION
calibrationData = head(leadTempData[1:2], -1)


# Datos de INTEGRACION
leadIntegrationData = read.table("plomo_areas.txt", header = TRUE, sep = "\t", dec = ",", col.names = c("d", "I"))
steelIntegrationData = read.table("acero_areas.txt", header = TRUE, sep = "\t", dec = ",", col.names = c("d", "I"))

rm(dataSet, leadTempData, steelTempData)



# |-------------------|
# | ANALISIS DE DATOS |
# |-------------------|

# Graficas para los datos en conjunto 
# ___________________________________


# Grafica para la CALIBRACION
par(mar=c(5,6,4,1)+.1)
plot(calibrationData, col = 1,
     xlab = "E [KeV]",
     ylab = "conteos",
     cex = 0.75,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)


# Graficas para el PLOMO
par(mar=c(5,6,4,1)+.1)
plot(leadData[[1]], col = 1,
     xlab = "E [KeV]",
     ylab = "conteos",
     cex = 0.75,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)
for (i in 2:7) {
  points(leadData[[i]],
         pch = 19,
         col = i,
         cex = 0.75)
}
legend("topright", legend=c(
    unlist(lapply(c(0, 5, 10, 15, 20, 25, 30), function(val) paste("d = ", val, " [mm]")))
  ),
  col=c(1:7), cex = 1.35, lwd = 2)


# Graficas para el ACERO
par(mar=c(5,6,4,1)+.1)
plot(steelData[[1]], col = 1,
     xlab = "E [KeV]",
     ylab = "conteos",
     cex = 0.75,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)
for (i in 2:8) {
  points(steelData[[i]],
         pch = 19,
         col = i,
         cex = 0.75)
}
legend("topright", legend=c(
    unlist(lapply(c(0, 5, 10, 15, 20, 25, 30, 35), function(val) paste("d = ", val, " [mm]")))
  ),
  col=c(1:8), cex = 1.35, lwd = 2)



# Graficas para el COEFICIENTE DE ATENUACION
# __________________________________________

# Grafica para el PLOMO
par(mar=c(5,6,4,1)+.1)
plot(leadIntegrationData[[1]], log(leadIntegrationData[[2]]),
     xlab = "d [mm]",
     ylab = "~I [I]",
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)

lm(c(1, 2, 3), c(1, 2, 3))
