log(2)/1.07


integrals = unlist(lapply(1:7, function(i) with(leadData[[i]], 
            integrate(approxfun(E, pmax(C, 0)), 
            lower = 662 - 55, upper = 662 - 35))[[1]]))
integrals = integrals + unlist(lapply(1:7, function(i) with(leadData[[i]], 
                        integrate(approxfun(E, pmax(C, 0)), 
                        lower = 662 - 35, upper = 662))[[1]]))
integrals = integrals + unlist(lapply(1:7, function(i) with(leadData[[i]], 
                        integrate(approxfun(E, pmax(C, 0)), 
                        lower = 662, upper = 662 + 50))[[1]]))



integrals = unlist(lapply(1:7, function(i) with(leadData[[i]], 
                        integrate(approxfun(E, pmax(C, 0)), 
                        lower = 800, upper = 975))[[1]]))
approxfun
#integrals = integrals - unlist(lapply(1:7, function(i) 105 * (leadData[[i]][316,][[2]] + leadData[[i]][372,][[2]]) / 2))

plot(leadData[[1]])
abline(v = 662 - 55)
abline(v = 662 + 50)

662 - 55
leadData[[1]][317,]

plot(c(0, 0.5, 1, 1.5, 2, 2.5, 3), log(integrals))

asd = data.frame(list(c(0, 0.5, 1, 1.5, 2, 2.5, 3), log(integrals)))
colnames(asd) = c("d", "I")

lm(I ~ d, asd)





integrals = unlist(lapply(1:8, function(i) with(steelData[[i]], 
            integrate(approxfun(E, pmax(C, 0)), 
            lower = 662 - 55, upper = 662 - 35))[[1]]))
integrals = integrals + unlist(lapply(1:8, function(i) with(steelData[[i]], 
                        integrate(approxfun(E, pmax(C, 0)), 
                        lower = 662 - 35, upper = 662))[[1]]))
integrals = integrals + unlist(lapply(1:8, function(i) with(steelData[[i]], 
                        integrate(approxfun(E, pmax(C, 0)), 
                        lower = 662, upper = 662 + 50))[[1]]))

integrals = integrals - unlist(lapply(1:8, function(i) 115 * (steelData[[i]][317,][[2]] + steelData[[1]][371,][[2]]) / 2))
integrals

plot(steelData[[1]])
abline(v = 662 - 55)
abline(v = 662 + 50)


plot(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), log(integrals))

asd = data.frame(list(c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5), log(integrals)))
colnames(asd) = c("d", "I")

lm(I ~ d, asd)