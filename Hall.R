# |-----------------|
# | CARGA DE DATOS  |
# |-----------------|
try(setwd("./data"), silent = TRUE)                         # Se comprueba la ruta de carga de archivos


Bvalues = c(78E-3, 134E-3, 195E-3, 251E-3, 302E-3, 302E-3)  # Valores del C.M constante al variar C.C
Ivalues = c(10E-3, 15E-3, 20E-3, 25E-3, 30E-3, 31E-3)       # Valores de C.C constante al variar C.M


# Se leen los datos, cambiando directamente el nombre de las columnas para que sea más fácil hacer el análisis de
# las pendientes
Bdata = lapply(list.files()[1:6], 
               function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("x", "y")))
                                                      
Idata = lapply(list.files()[7:12], 
               function(path) read.table(path, header = TRUE, sep = "\t", dec = ",", col.names = c("x", "y")))
                                                             

# |---------------------------|
# | CONSTANTES EXPERIMENTALES |
# |---------------------------|

d = 1E-3
b = 10E-3
w = 20E-3

e0 = 1.6E-19

# |---------------------------------|
# | FUNCIONES PROPIAS DEL ANÁLISIS  |
# |---------------------------------|

# Función para obtener las pendientes de las regresiones lineales
getSlope <- function(eData)
{
  slopes = c()
  rsqrt = c()
  for (df in eData)
  {
    model = lm(y ~ x, df)
    slopes = append(slopes, 
                    coef(model)[2])
    rsqrt = append(rsqrt, 
                    summary(model)$r.squared)
  }
  return(c(slopes,rsqrt))
}


# |-------------------|
# | ANÁLISIS DE DATOS |
# |-------------------|
# Cabe aclarar que Bdata hace referencia a los datos tomados con B constante e I variable, por ende, 
# las pendientes para I variable son aquellas encontradas para Bdata, mismo caso para Idata.

Islopes = getSlope(Bdata)[1:6]              # Se obtienen los datos de las pendientes contra los valores
Bslopes = getSlope(Idata)[1:6]              # constantes de los parámetros para cada tipo de dato

Irsqrt = getSlope(Bdata)[7:12]               # Se obtienen los datos de las pendientes contra los valores
Brsqrt = getSlope(Idata)[7:12]               # constantes de los parámetros para cada tipo de dato

IDF = data.frame(Bvalues, Islopes)        # Se almacenan estos datos en Data Frames para ser analizados
BDF = data.frame(Ivalues, Bslopes)        # con mayor facilidad

# El primer resultado es obtener PS, el cual dada la pendiente de la regresión entre Islopes y Bvalues 
# A*, está dado por la expresión 1/(e0 * d * A*)

ps = 1 / (e0 * d * coef(lm(Islopes ~ Bvalues, IDF))[2])

print(paste("Valor hallado para ps: ", signif(ps, 3)))
print(paste("Valor de guia para ps: ", 1.1E21))
print(paste("Error relativo: ", signif(1 - (ps / 1.02E21), 3)))

# El segundo resultado es la constante R_H, para ello, primero nos aseguramos que U_H es lineal tanto
# para valores variables de I como de B. Para esto, nos basamos en el R^2 de las regresiones, buscando
# para todas valores cercanos a 1

print("")
print("")

print("Valores de R^2 para las regresiones U_H vs I: ")
print(signif(unlist(Irsqrt), 3))

print("")

print("Valores de R^2 para las regresiones U_H vs B: ")
print(signif(unlist(Brsqrt), 3))

# Ya corroborando la linealidad en ambos casos, se puede seguir a obtener R_H. Para ello, dadas las pendientes 
# A* de los datos pendientes vs parametros (respectivos), se tiene que R_H = d * A*. Así

RH_I = d * coef(lm(Islopes ~ Bvalues, IDF))[2]
RH_B = d * coef(lm(Bslopes ~ Ivalues, BDF))[2]

RH_mean = (RH_I + RH_B)/2

RH_real = 6.6E-3


print("")
print("")

print(paste("Estimación R_H dado I variable: ", signif(RH_I, 3)))
print(paste("Estimación R_H dado B variable: ", signif(RH_B, 3)))
print(paste("Estimación R_H promedio de I B: ", signif(RH_mean, 3)))
print(paste("Valor real (dada guía) de R_H: ", RH_real))
print(paste("Error relativo RH promedio: ", signif(1 - RH_mean/RH_real, 3)))
