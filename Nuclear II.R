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


leadSemilogData = data.frame(list(leadIntegrationData[[1]]/10, log(leadIntegrationData[[2]])))
colnames(leadSemilogData) = c("d", "I")

# Grafica para el PLOMO
par(mar=c(5,6,4,1)+.1)
plot(leadSemilogData,
     ylim = c(8, 18),
     xlab = "d [cm]",
     ylab = "~I [I]",
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)
abline(lm(I ~ d, leadSemilogData), lwd = 2)

print(paste("Coeficiente de atenuacion PLOMO: ", -lm(d ~ I, leadSemilogData)[[1]][[2]], " pm ", 0.1209))



steelSemilogData = data.frame(list(steelIntegrationData[[1]]/10, log(steelIntegrationData[[2]])))
colnames(steelSemilogData) = c("d", "I")

# Grafica para el ACERO
par(mar=c(5,6,4,1)+.1)
plot(steelSemilogData,
     ylim = c(8, 18),
     xlab = "d [cm]",
     ylab = "~I [I]",
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)
abline(lm(I ~ d, steelSemilogData), lwd = 2)

print(paste("Coeficiente de atenuacion ACERO: ", -lm(d ~ I, steelSemilogData)[[1]][[2]], " pm ", 1.02))

steelSemilogData
steelIntegrationData

plot(leadIntegrationData, log = "y")
points(steelIntegrationData, log = "y", col = 2)

lm(d/10 ~ log(I), steelIntegrationData)
lm(d/10 ~ log(I), leadIntegrationData)

lm(log(I) ~ {d/10}, steelIntegrationData)
lm(log(I) ~ {d/10}, leadIntegrationData)


