
# Meta analysis

library(R2OpenBUGS)

setwd("D:\\Mathemagic\\Spring2019\\Bayesian_Analysis")
drugs=read.table("DrugData.txt",header =T)
drugs$I1=as.integer(I(drugs$time<=19))
drugs$I2=as.integer(I(20<=drugs$time)&I(drugs$time<=30)&I(drugs$generic==0))
drugs$I3=as.integer(I(drugs$time>=31)&I(drugs$generic==0))
drugs$I4=as.integer(I(drugs$time>=25)&I(drugs$generic==1))
drugs

lm1=lm(log(price)~ (I1*time),data=drugs)
lm2=lm(log(price)~ (I2*time),data=drugs)
lm3=lm(log(price)~ (I3*time),data=drugs)
lm4=lm(log(price)~ (generic*time),data=drugs)


pwlr=lm(log(price)~(I1*time)+(I2*time)+(I3*time)+(time*I4),data=drugs)
summary(pwlr)
yhat=fitted(pwlr)
plot(drugs$time,yhat)

x1=drugs$time*drugs$I1
y1=fitted(pwlr)*drugs$I1

x2=drugs$time*drugs$I2
y2=fitted(pwlr)*drugs$I2

x3=drugs$time*drugs$I3
y3=fitted(pwlr)*drugs$I3

x4=drugs$time*drugs$I4
y4=fitted(pwlr)*drugs$I4

plot(drugs$time,log(drugs$price),pch=ifelse(drugs$generic==0,1,2),
     xlab="Time in months", ylab="Log(Price)")
lines(x1[x1!=0],y1[y1!=0])
lines(x2[x2!=0],y2[y2!=0])
lines(x3[x3!=0],y3[y3!=0])
lines(x4[x4!=0],y4[y4!=0], lty=2)
legend(1,4,c("brand","generic"),pch=c(1,2),lty=c(1,2))
title("Figure 1. Price trends for brand and generic drugs")





drugsmodel <- function(){
  # likelihood
  for(i in 1:N)
  {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1*time[i]*I1[i] + beta2*time[i] + beta3*I2[i]*time[i] + beta4*time[i]*I3[i] + 
      beta5*I1[i] + beta6*I2[i] + beta7*gen[i]
  }
  # priors
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  beta2 ~ dnorm(0,0.001)
  beta3 ~ dnorm(0,0.001)
  beta4 ~ dnorm(0,0.001)
  beta5 ~ dnorm(0,0.001)
  beta6 ~ dnorm(0,0.001)
  beta7 ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  tau <- 1/(sigma*sigma)
  
}

write.model(drugsmodel, "drugsmodel.txt")

data=list(time=drugs$time, I1=drugs$I1, I2=drugs$I2, I3=drugs$I3,gen=drugs$generic, y=log(drugs$price), N=74)

inits=list(list(beta0 = rnorm(1,0,10000), beta1 = rnorm(1,0,10000), 
                beta2 = rnorm(1,0,10000), beta3 = rnorm(1,0,10000), 
                beta4 = rnorm(1,0,10000), beta5 = rnorm(1,0,10000),
                beta6 = rnorm(1,0,10000), beta7 = rnorm(1,0,10000)))

parameters <- c("beta0", "beta1","beta2","beta3","beta4","beta5","beta6","beta7")

drugs.sim <- bugs(data, inits, parameters, "drugsmodel.txt",n.chains=1,n.burnin=10000, n.iter=20000)

drugs.sim$summary
plot(drugs.sim)
