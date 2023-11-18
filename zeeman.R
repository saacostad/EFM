# Funcion para leer los mazimos y minimos. Se copian los tres parametros
# del pico central de la izquierda, luego, los tres parametros del pico
# central izquierdo, y por ultimo, los tres parametros de alguno de los 
# picos laterales al pico central izquierdo en data.txt. 
readData <- function()
{
  dat = read.table("data.txt", header = FALSE, sep = "\t", dec = ".")
  
  leftMax = -dat[[3]][2] / (2 * dat[[3]][1])
  rightMax = -dat[[3]][5] / (2 * dat[[3]][4])
  sLeftMax = -dat[[3]][8] / (2 * dat[[3]][7])
  
  return(abs(leftMax - sLeftMax) / abs(rightMax - leftMax))
}

lambdaRatios = c(0.1569323, 0.1574435, 0.1616738, 0.1615201, 0.1762844, 0.1755534, 0.1854667, 0.1857519, 0.1920804, 0.1967048, 0.2056312, 0.206026)


lambda = 643.8e-9      # Longitud de onda de la linea roja del cadmio
dlambdaConstant = (lambda^2) / ( (2 * 4.04e-3) * sqrt( (1.4567)^2 - lambda) )

hc = 1.986e-25
mb = 9.27e-24

DeltaE = function(DL){
  return((hc * DL * dlambdaConstant) / lambda^2)
}

DeltaEs = unlist(lapply(lambdaRatios, function(rat) DeltaE(rat)))

B = c(351e-3,351e-3,  378e-3,378e-3,  401e-3,401e-3,  417e-3,417e-3,  429e-3,429e-3,  443e-3,443e-3)

dat = data.frame(list(B, DeltaEs))
colnames(dat) = c("B", "dE")

par(mar=c(5,6,4,1)+.1)
par(mar=c(5,6,4,1)+.1)
plot(dat, col = 1,
     xlab = "B [T]",
     ylab = "Delta E [J]",
     cex = 1.25,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 19,
     lwd = 2)
abline(lm(dE ~ B, dat), col = 2, lwd = 2.5)


print(paste("Estimacion mb: ", lm(dE ~ B, dat)[[1]][[2]]))
print(paste("Valor real mb: ", mb))
print(paste("Error relativo: ", 1 - lm(dE ~ B, dat)[[1]][[2]] / mb))



# GRAFICA DE EJEMPLO

dat = read.table("data.txt", header = TRUE, sep = "\t", dec = ".")


xs = seq(-460,-380, by = 0.25)

par(mar=c(5,6,4,1)+.1)
plot(dat, col = 1,
     ylab = "lumens [cd]",
     xlab = "x",
     cex = 1,
     cex.lab = 2.5,
     cex.axis = 2,
     pch = 1,
     lwd = 2)
lines(xs, (-0.9379344398699104*xs^2 -837.0697774042462*xs - 186594.77173348048), 
      lwd = 3, col = 2)
lines(xs, (-0.6843745664377998*xs^2 -545.6542450253522*xs - 108605.00796478231), 
      lwd = 3, col = 3)
lines(xs, (-1.2043316321720205*xs^2 -1051.6026998999291*xs - 229397.8791718079), 
      lwd = 3, col = 4)



