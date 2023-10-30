# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|
try(setwd("./datos"), silent = TRUE)


CBdata = read.table(list.files()[9], header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R"))
IVarData = lapply(list.files()[c(1, 2, 3, 7)], 
              function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R")))
UVarData = lapply(list.files()[4:8], 
              function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R")))

# |-----------------|
# | OTRAS FUNCIONES |
# |-----------------|


findMaxValues <- function(data, th)
{
  treshold = th
  last_data = strtoi(row.names(tail(data[[1]], 1)))

  max_D = list()
  last_max_value = 0
  
  for (x in 30:(last_data - 2*treshold))
  {
    if (max(data[[1]][x:(x + 2*treshold), 2]) == data[[1]][x + treshold, 2] && data[[1]][x + treshold, 2] != last_max_value)
    {
      max_D <- append(max_D, list(data[[1]][x + treshold, ]))
      last_max_value = data[[1]][x + treshold, 2]
    }
  }
  
  return(max_D)
}


meanExpData <- function(rval, eval)
{
  meanval = sum(rval / (eval^2)) / sum(1 / eval^2)
  errval = 1 / sqrt(sum(1 / eval^2))
  
  return(c(meanval, errval))
}

propProdErrors <- function(res, rval, eval){return(abs(res * sqrt(sum((eval/ rval)^2))))}

# |-----------------------------------------------|
# |               PRIMER RESULTADO                |
# | LONGITUDES DE ONDA DADA LA CONDICIÓN DE BRAGG |
# |-----------------------------------------------|

td = 5.6402e-10 # Constante 2d para el monocristal de sal                      

# Función que retorna la longitud de onda estimada 
# por la Condición de Bragg dado el ángulo de 
# difracción y el orden
BraggCondition <- function(angle, order){
  relVal = td * sin(angle * pi / 180) / order
  error = td * cos(angle * pi / 180) * (0.1 * pi / 180) / order
  return(c(relVal, error))
}




# Utilizando los resultados del espectro completo, hallamos la longitud de onda para Ka y Kb
CBpeaks = findMaxValues(list(CBdata), 6)
CBlambdas = lapply(1:6, function(i) BraggCondition(CBpeaks[[i]][[1]], trunc((i + 1)/2)))

UVpeaks = lapply(UVarData, function(data) findMaxValues(list(data), 6))
UVlambdas = lapply(3:5, function(i) 
            unlist(c(BraggCondition(UVpeaks[[i]][[1]][[1]], 1), BraggCondition(UVpeaks[[i]][[2]][[1]], 1))))

IVpeaks = lapply(IVarData, function(data) findMaxValues(list(data), 6))
IVlambdas = lapply(1:4, function(i) 
            unlist(c(BraggCondition(IVpeaks[[i]][[1]][[1]], 1), BraggCondition(IVpeaks[[i]][[2]][[1]], 1))))

KbEst = unlist(append(lapply(CBlambdas[c(1, 3, 5)], function(l) l[[1]]), append(lapply(UVlambdas, function(l) l[[1]]), lapply(IVlambdas, function(l) l[[1]]))))
KaEst = unlist(append(lapply(CBlambdas[c(2, 4, 6)], function(l) l[[1]]), append(lapply(UVlambdas, function(l) l[[3]]), lapply(IVlambdas, function(l) l[[3]]))))

KbErrors = unlist(append(lapply(CBlambdas[c(1, 3, 5)], function(l) l[[2]]), append(lapply(UVlambdas, function(l) l[[2]]), lapply(IVlambdas, function(l) l[[2]]))))
KaErrors = unlist(append(lapply(CBlambdas[c(2, 4, 6)], function(l) l[[2]]), append(lapply(UVlambdas, function(l) l[[4]]), lapply(IVlambdas, function(l) l[[4]]))))


# Promediamos los resultados del mismo orden de difracción
Ka = meanExpData(KaEst, KaErrors)
Kb = meanExpData(KbEst, KbErrors)

rKa = 7.108e-11
rKb = 6.3095e-11

print("ESTIMAR LA LONGITUD DE ONDA Y ENERGÍA CARACTERÍSTICA DE LOS PICOS: COMPROBAR BRAGG")
print(" ")
print(paste("Estimación Ka: ", signif(Ka[1], 3), " pm ", signif(Ka[2], 1)))
print(paste("Error relativo Ka: ", 1 - (Ka[1] / rKa)))

print(paste("Estimación Kb: ", signif(Kb[1], 3), " pm ", signif(Kb[2], 3)))
print(paste("Error relativo Kb: ", 1 - (Kb[1] / rKb)))





# |-----------------------------|
# | Se encuentran las energías  |
# |-----------------------------|

hc = 1.23984193e-6   # Valor ed HC en eV

eKa = hc / Ka[1]
eKaError = propProdErrors(eKa, c(Ka[1]), c(Ka[2]))

eKb = hc / Kb[1]
eKbError = propProdErrors(eKb, c(Kb[1]), c(Kb[2]))

print(paste("Energía característica para Ka: ", signif(eKa, 3), " pm ", signif(eKaError, 3)))
print(paste("Energía característica para Kb: ", signif(eKb, 3), " pm ", signif(eKbError, 3)))


# |-------------------------------------|
# |         SEGUNDO RESULTADO           |
# | ENERGÍA LÍMITE PARA I Y U VARIABLE  |
# |-------------------------------------|

findAccRad <- function(data, th)
{
  maxVal = max(data[,2][1:th])
  
  for(i in 70:1){
    if(data[i,2] < maxVal){
      return(data[i, 1])
    }
  }
}

print(" ")
print(" ")
print(" ")
print("ENERGÍA LÍMITE PARA I Y U VAR: MOSTRAR QUE I VAR NO TIENE CAMBIO DE ENERGÍA MIENTRAS QUE U VAR SÍ LO TIENE")
print(" ")

IvarLim = unlist(lapply(IVarData, function(data) findAccRad(data, 13)))
print("Valores de b límite para I variable: ")
print(IvarLim)
print(paste("Coeficiente de variación de los datos: ", sd(IvarLim) / mean(IvarLim) * 100))
            


print(" ")
UvarLim = unlist(lapply(1:5, function(i) findAccRad(UVarData[[i]], trunc(55 / i))))
print("Valores de b límite para U variable: ")
print(UvarLim)
print(paste("Coeficiente de variación de los datos: ", sd(UvarLim) / mean(UvarLim) * 100))


# |-------------------------------------------|
# |           TERCER RESULTADO                |
# | ESTATICIDAD DE LOS PICOS CARACTERÍSTICOS  |
# |-------------------------------------------|

IVKa = unlist(lapply(IVpeaks, function(l) l[[1]][[1]]))
IVKb = unlist(lapply(IVpeaks, function(l) l[[2]][[1]]))

UVKa = unlist(lapply(UVpeaks[3:5], function(l) l[[1]][[1]]))
UVKb = append(unlist(lapply(UVpeaks[3:5], function(l) l[[2]][[1]])), UVpeaks[[2]][[1]][[1]])

print(" ")
print(" ")
print(" ")

print("VALORES DE B PARA LOS DIFERENTES PICOS: MOSTRAR QUE SON LOS MISMOS")
print(" ")

print("Valores de b para Ka encontrados para I variable: ")
print(IVKa)
print(paste("Coeficiente de variación de los datos: ", sd(IVKa) / mean(IVKa) * 100))

print(" ")
print("Valores de b para Kb encontrados para I variable: ")
print(IVKb)
print(paste("Coeficiente de variación de los datos: ", sd(IVKb) / mean(IVKb) * 100))


print(" ")
print(" ")
print("Valores de b para Ka encontrados para U variable: ")
print(UVKa)
print(paste("Coeficiente de variación de los datos: ", sd(UVKa) / mean(UVKa) * 100))

print(" ")
print("Valores de b para Kb encontrados para U variable: ")
print(UVKb)
print(paste("Coeficiente de variación de los datos: ", sd(UVKb) / mean(UVKb) * 100))



# |---------------------------------------|
# |           CUARTO RESULTADO            |
# | MAXIMOS DE LOS PICOS PARA I VARIABLE  |
# |---------------------------------------|

par(mar=c(5,6,4,1)+.1)
plot(c(0.4, 0.6, 0.8, 1), unlist((lapply(IVpeaks, function(l) l[[2]][[2]]))),
     lwd = 2, 
     col = 1,
     ylim = c(700, 2700),
     xlab = "I [mA]",
     ylab = "R [1/s]",
     cex.lab = 2.5,
     cex.axis = 2,
     cex = 1.5,)
points(c(0.4, 0.6, 0.8, 1), unlist((lapply(IVpeaks, function(l) l[[1]][[2]]))),
       lwd = 2, 
       col = 2,
       pch = 2,
       cex = 1.5)
legend("topleft", legend=c(
  unlist(lapply(c("Kb", "Ka"), function(val) paste("U = ", val, " [mA]")))),
  col=c(2:1), cex = 1.35, lwd = 2)

# |-------------------|
# | APARTADO GRÁFICO  |
# |-------------------|

# Gráficas para b completo
par(mar=c(5,6,4,1)+.1)
plot(CBdata, col = 1,
     xlab = "b [°]",
     ylab = "R [1/s]",
     cex = 1,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 1,
     lwd = 2)
abline(v=CBpeaks[[1]][[1]], col = 2, lwd = 2)
abline(v=CBpeaks[[2]][[1]], col = 2, lwd = 2)
abline(v=CBpeaks[[3]][[1]], col = 2, lwd = 2)
abline(v=CBpeaks[[4]][[1]], col = 2, lwd = 2)
abline(v=CBpeaks[[5]][[1]], col = 2, lwd = 2)
abline(v=CBpeaks[[6]][[1]], col = 2, lwd = 2)



# Gráficas para U Variable
par(mar=c(5,6,4,1)+.1)
plot(UVarData[[5]], col = 5,
     xlab = "b [°]",
     ylab = "R [1/s]",
     cex = 1,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 1,
     lwd = 2)
for (i in 4:1) {
  points(UVarData[[i]],
         col = i,
         pch = i,
         cex = 1,
         lwd = 2)
}
for (i in 5:2) {
  b = findMaxValues(UVarData[i], 6)
  abline(v=b[[1]][[1]], col = i, lwd = i*1.25)
  try(abline(v=b[[2]][[1]], col = i, lwd = i*1.25), silent = TRUE)
}
legend("topright", legend=c(
  unlist(lapply(c(15, 20, 25, 30, 32), function(val) paste("U = ", val, " [mA]")))),
  col=c(5:1), cex = 1.35, lwd = 2)



# Gráficas para I Variable
par(mar=c(5,6,4,1)+.1)
plot(IVarData[[4]], col = 4,
     xlab = "b [°]",
     ylab = "R [1/s]",
     cex = 1,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 1,
     lwd = 2)
for (i in 3:1) {
  points(IVarData[[i]],
         col = i,
         pch = i,
         cex = 1,
         lwd = 2)
}
for (i in 4:1) {
  b = findMaxValues(IVarData[i], 6)
  abline(v=b[[1]][[1]], col = i, lwd = i*1.25)
  abline(v=b[[2]][[1]], col = i, lwd = i*1.25)
}
legend("topright", legend=c(
  unlist(lapply(c(0.4, 0.6, 0.8, "1.0"), function(val) paste("I = ", val, " [mA]")))),
  col=c(4:1), cex = 1.35, lwd = 2)



rm(b, i, rKa, rKb, td)
