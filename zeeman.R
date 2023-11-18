DLAMBDA = c(52.75, 50, 44.35, 54.61, 50.24, 47.89, 55.28, 49.1, 41.65, 40.74, 46.18, 54.16)
dlambda = c(8.01, 10, 7.48, 11.48, 13.26, 7.57, 11.33, 11.01, 13.62, 11.41, 10.46, 12.5)
current = c(5, 5, 5.5, 5.5, 6, 6, 6.5, 6.5, 7, 7, 7.5, 7.5)

lambda = 0.02e-9      # Longitud de onda de la linea roja del cadmio
dlambdaConstant = (lambda^2) / ( (2 * 4.04e-3) * sqrt( (1.4567)^2 - lambda) )


lambdaRatio <- function(D, d){
  return(d * dlambdaConstant / D)
}

lambdaRatios = unlist(lapply(1:12, function(i) {lambdaRatio(DLAMBDA[i], dlambda[i])}))

newLambdaRatios = unlist(lapply( ((1:6)*2 - 1), function(i) mean(c(lambdaRatios[i], lambdaRatios[i + 1]))))
ccurrent = c(5, 5.5, 6, 6.5, 7, 7.5)
plot(ccurrent, newLambdaRatios)


hc = 1.986e-25
mb = 9.27e-24

DeltaE = function(DL){
  return(hc * DL / lambda^2)
}

DeltaEs = unlist(lapply(newLambdaRatios, DeltaE))

B = c(351e-3, 378e-3, 401e-3, 417e-3, 443e-3)

plot(B, DeltaEs[c(1, 2, 3, 4, 6)])


dat = data.frame(list(B, DeltaEs[c(1, 2, 3, 4, 6)]))
colnames(dat) = c("B", "dE")


plot(dat)
lm(dE ~ B, dat)


1 - 9.922e-24 / mb
