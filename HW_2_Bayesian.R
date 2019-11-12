####__________HW_2___BAYESIAN_ANALYSIS_SPRING_2019_______EVAN_WALKER____###
#install.packages("R2OpenBUGS")
library(R2OpenBUGS)
setwd("D:\\Mathemagic\\Spring2019\\Bayesian_Analysis")


###########_____PROBLEM_1_____############
#####_______LINEAR_REG_________#####
d1=read.table("lowbwt.dat",header=TRUE)
d1=d1[,c(4,6,8,9,11)]
summary(d1)


#####___________FREQUENTISTS_APPROACH__________#####
linmod=lm(BWT~ .,data=d1)
summary(linmod)


######__________BAYESIAN_APPROACH_______#######
bwtmodel=function()
{
  # likelihood
  for(i in 1:N)
  {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i]
  }
  # priors
  beta0 ~ dnorm(0,0.001)
  beta1 ~ dnorm(0,0.001)
  beta2 ~ dnorm(0,0.001)
  beta3 ~ dnorm(0,0.001)
  beta4 ~ dnorm(0,0.001)
  beta5 ~ dnorm(0,0.001)
  sigma ~ dunif(0,100)
  
  tau <- 1/(sigma*sigma)
  
}

write.model(bwtmodel, "bwtmodel.txt")

data=list(x1=d1$LWT, x2=d1$SMOKE, x3=d1$HT, x4=d1$UI, y=d1$BWT, N=189)

inits=list(list(beta0 = rnorm(1,0,10000), beta1 = rnorm(1,0,10000), 
                beta2 = rnorm(1,0,10000), beta3 = rnorm(1,0,10000), 
                beta4 = rnorm(1,0,10000)))

parameters <- c("beta0", "beta1","beta2","beta3","beta4")

bwt.sim <- bugs(data, inits, parameters, "bwtmodel.txt",n.chains=1,n.burnin=10000, n.iter=20000)

bwt.sim$summary
summary(data)
plot(bwt.sim)

#parameters significantly differ between frequenstist and bayesian




###########_____PROBLEM_2_____############
#####_________LOGREG________#######
d2=read.table("wcsg_short.txt",header=TRUE)
summary(d2)
nrow(d2)
p=sum(d2$chd)/nrow(d2)
p
r=sum(d2$chd[which(d2$chd==1)])


#####___________FREQUENTISTS_APPROACH__________#####
logregmod=glm(chd~ .,data=d2,family="binomial")
summary(logregmod)


######__________BAYESIAN_APPROACH_______#######
chdmodel=function(){
  for( i in 1 : N ) {
    y[i] ~ dbern(p[i])
    logit(p[i]) <- alpha0 + alpha1*x1[i] + alpha2*x2[i] + alpha3*x3[i]
  }
  alpha0 ~ dnorm(0.0,0.00001)
  alpha1 ~ dnorm(0.0,0.00001)
  alpha2 ~ dnorm(0.0,0.00001)
  alpha3 ~ dnorm(0.0,0.00001)
}

write.model(chdmodel, "chdmodel.txt")

data=list(x1=d2$age, x2=d2$bmi, x3=d2$smoking, y=d2$chd, N=3154)

inits=list(list(alpha0 = 0, alpha1 = 0, 
                alpha2 = 0, alpha3 = 0))

parameters <- c("alpha0", "alpha1","alpha2","alpha3")

chd.sim <- bugs(data, inits, parameters, "chdmodel.txt",n.chains=1,n.burnin=10000, n.iter=20000)

chd.sim$summary
summary(data)
plot(chd.sim)



# The estimations for the mean of the parameters are extremely close to that of the frequentist logistic regression.
#similarly, the deviation of the parameters are also close.
