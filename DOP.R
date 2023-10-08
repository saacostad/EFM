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
  


# |-----------------------|
# | CONSTANTES NECESARIAS |
# |-----------------------|

# CONSTANTES EXPERIMENTALES
# -----------------------------------------------------------------

voltageError = 100  # Error en los datos del voltage

fd = 2.13E-10       # Se guardan los datos de las distancias de red
sd = 1.23E-10       # del grafito junto a su respectivo error
ed = 0.01E-10


L = 13.5E-2         # Se guardan datos de la distancia del tubo
eL = 0.2E-2


# CONSTANTES UNIVERSALES  
# -----------------------------------------------------------------

h = 4.135667696E-15   # Constante de Plank en eV
e = 1                 # Carga del electrón en eV
m = 9.109383706E-31   # Masa del electrón



# |-------------------|
# | CREACIÓN DE DATOS |
# |-------------------|
# Como los datos conseguidos son, en sí, imágenes, el procesamiento de estas fue hecho con
# hecho con ayuda de tracker, de donde se consiguen los siguientes datos.

# Datos obtenidos del voltage acelerador
voltage = c(2600, 3000, 3400, 3800, 4200, 4600, 5000, 
            2600, 3000, 3400, 3800, 4200, 4600, 5000)


# Datos obtenidos de los diámetros de los anillos internos
fiRing = c(2.837E-2, 2.691E-2, 2.470E-2, 2.366E-2, 2.283E-2, 2.248E-2, 2.241E-2, 
                2.860E-2, 2.702E-2, 2.549E-2, 2.376E-2, 2.241E-2, 2.209E-2, 2.178E-2)
foRing = c(3.762E-2, 3.546E-2, 3.271E-2, 3.032E-2, 2.877E-2, 2.859E-2, 2.860E-2, 
                3.582E-2, 3.524E-2, 3.330E-2, 3.080E-2, 2.881E-2, 2.787E-2, 2.767E-2)


# Datos obtenidos de los diámetros de los anillos externos
siRing = c(4.984E-2, 4.906E-2, 4.419E-2, 4.193E-2, 4.009E-2, 3.974E-2, 3.842E-2, 
                5.036E-2, 4.732E-2, 4.451E-2, 4.146E-2, 4.025E-2, 3.828E-2, 3.910E-2)
soRing = c(6.132E-2, 5.828E-2, 5.244E-2, 5.069E-2, 4.684E-2, 4.565E-2, 4.541E-2, 
                5.949E-2, 5.551E-2, 5.237E-2, 4.956E-2, 4.606E-2, 4.564E-2, 4.522E-2)





# Se crea DataFrame con los datos experimentales
expData <- data.frame(voltage, fiRing, foRing, siRing, soRing)
rm(voltage, fiRing, foRing, siRing, soRing)




# |-------------------|
# | ANÁLISIS DE DATOS |
# |-------------------|
# Ya con los datos experimentales, se analizan para obtener los intervalos necesarios
# para el experimento, es decir, los lambda experimentales y teóricos.


# OBTENCIÓN DE LOS LAMBDA TEÓRICOS  
# ------------------------------------------------------------------------
# Haciendo uso de la hipótesis de De Broglie, dado el potencial acelerador  
# del experimento se puede obtener una estimación para el lambda teórico

theoLambda <- function(U, dU)
{
  k = h / sqrt(2 * m * e)
  
  lambda = k / sqrt(U)
  
  # Cálculo del error teórico
  error = (k / 2) * u^(-3 / 2) * 100
  
  return(c(lambda, error))
}



# OBTENCIÓN DE LOS LAMBDA TEÓRICOS  
expLambda <- function(d, iD, oD, L)
{
  D = mean(c(iD, oD))
  dD = (oD - iD) / 2
  
  p = d * D
  dp = p * sqrt((dD / D)^2 + (ed / d)^2)
  
  q = p / L
  dq = q * sqrt((dp / p)^2 + (eL / L)^2)
  
  dLambda = dq / 2
  
  return(c(lambda, dLambda))
}





