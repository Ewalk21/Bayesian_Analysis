####__________HW_2___BAYESIAN_ANALYSIS_SPRING_2019_______EVAN_WALKER____###
install.packages("nlme")
library(nlme)
library(R2OpenBUGS)
setwd("D:\\Mathemagic\\Spring2019\\Bayesian_Analysis")

data(Orthodont)
d=Orthodont
d$Sex=as.numeric(d$Sex)

###########_____PROBLEM_1_____############

######__________BAYESIAN_APPROACH_______#######
orthmodel=function()
{
  for( i in 1 : N ) {
    for( j in 1 : T ) {
      y[i , j] ~ dnorm(mu[i , j],tau.c)
      mu[i , j] <- alpha[i] + beta[i]*(x1[j]-xbar) + beta1[i]*(x2[j]-xbar)
    }
    alpha[i] ~ dnorm(alpha.c,alpha.tau)
    beta[i] ~ dnorm(beta.c,beta.tau)
    beta1[i] ~ dnorm(beta.c1,beta.tau1)
  }
  tau.c ~ dgamma(0.001,0.001)
  sigma <- 1 / sqrt(tau.c)
  alpha.c ~ dnorm(0.0,1.0E-6)	   
  alpha.tau ~ dgamma(0.001,0.001)
  beta.c ~ dnorm(0.0,1.0E-6)
  beta.tau ~ dgamma(0.001,0.001)
  beta.c1 ~ dnorm(0.0,1.0E-6)
  beta.tau1 ~ dnorm(0.001,0.001)
}

write.model(orthmodel, "orthmodel.txt")

data=list(x1=d$Sex, x2=d$age,xbar=11, N=27,T=4,x=c(8,10,12,14),
          y=structure(.Data=d$distance,.Dim=c(27,4)))

inits=list(list(alpha= rep(1,27), beta = rep(1,27),beta1 = rep(1,27),alpha.c = 1, beta.c = 1, 
                tau.c = 1, alpha.tau = 1, beta.tau = 1, beta.c1=1, beta.c2=1))

parameters <- c("alpha", "beta","beta1")

orth.sim <- bugs(data, inits, parameters, "orthmodel.txt",n.chains=1,n.burnin=10000, n.iter=20000)

orth.sim$summary
summary(data)
plot(orth.sim)



