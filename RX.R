# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|
try(setwd("./datos"), silent = TRUE)


CBdata = read.table(list.files()[9], header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R"))
IVarData = lapply(list.files()[c(1, 2, 3, 7)], 
              function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R")))
UVarData = lapply(list.files()[4:8], 
              function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("b", "R")))



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
CBpeaks = findMaxValues(list(CBdata), 6)
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
  try(abline(v=b[[2]][[1]], col = i, lwd = i*1.25))
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

