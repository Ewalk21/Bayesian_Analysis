
# Pump analysis

library(R2OpenBUGS)

setwd("D:\\Mathemagic\\Spring2019\\Bayesian_Analysis")

#poisson regression with an offset, theta corresponds to different pumps

pumpmodel=function(){
		for (i in 1 : N) {
			theta[i] ~ dgamma(alpha, beta)
			lambda[i] <- theta[i] * t[i]
			x[i] ~ dpois(lambda[i])
		}		
       alpha ~ dexp(1)
		beta ~ dgamma(0.1, 1.0)

}	

write.model(pumpmodel, "pumpmodel.txt")

data=list(t = c(94.3, 15.7, 62.9, 126, 5.24, 31.4, 1.05, 1.05, 2.1, 10.5),
	     x = c( 5, 1, 5, 14, 3, 19, 1, 1, 4, 22), N = 10)
	
inits=list(list(alpha = 1, beta = 1))

parameters <- c("alpha", "beta", "theta")

pump.sim <- bugs(data, inits, parameters, "pumpmodel.txt",n.chains=1,n.burnin=10000, n.iter=20000)

pump.sim$summary
summary(data)
plot(pump.sim)

