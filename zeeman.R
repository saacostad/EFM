DLAMBDA = c(64-16, 64-15, 65-18, 67-19, 70-22, 71-23, 66-18, 67-19, 66-18, 67-19, 70-22, 67-19)
dlambda = c(16-8, 64-55, 65-55, 67-57, 30-22, 32-23, 28-18, 29-19, 28-18, 28-19, 33-22, 29-19)
current = c(5, 5, 5.5, 5.5, 6, 6, 6.5, 6.5, 7, 7, 7.5, 7.5)

lambda = 0.02e-9      # Longitud de onda de la linea roja del cadmio
dlambdaConstant = (lambda^2) / ( (2 * 4.04e-3) * sqrt( (1.4567)^2 - lambda) )


lambdaRatio <- function(D, d){
  return(d * dlambdaConstant / D)
}