##install.packages("quantreg")                                                                                       
library(quantreg)
library(MASS)
bootLAR<-function(x, y, conf, B){
  alpha <- 1-conf
  coe<-rq(y ~ x, 0.5)
  beta0<-coe$coefficients[1]
  beta1<-coe$coefficients[2]
  coef<-matrix(0, B, 2)
  beta<-matrix(0, 2, 2)
  numb<-rep(0, length(x))
  for(i in 1: B){
    numb <- sample(c(1: length(x)), size=length(x), replace=T)
    yy <- y[numb]
    xx <- x[numb]
    coe<-rq(yy ~ xx, 0.5)
    coef[i,1]<-coe$coefficients[1]
    coef[i,2]<-coe$coefficients[2]
  }
  beta[1,1]<-2*beta0 - quantile(coef[,1], 1-alpha/2)
  beta[1,2]<-2*beta0 - quantile(coef[,1], alpha/2)
  beta[2,1]<-2*beta1 - quantile(coef[,2], 1-alpha/2)
  beta[2,2]<-2*beta1 - quantile(coef[,2], alpha/2)
  return(beta)
}
##Simulation##
x<-rnorm(100)
y<-5+2*x+rnorm(100, 0.1)
bootLAR(x, y, 0.99, 1000)
bond<-bootLAR(x, y, 0.99, 1000)
cat("For beta0, the bootstrap pivotal confidence interval is from", bond[1,1], "to", bond[1,2])
cat("For beta1, the bootstrap pivotal confidence interval is from", bond[2,1], "to", bond[2,2])

##Finally, we get the bootstrap pivotal confidence intervals for the coefficient
##vectors beta0 and beta1, and the true values of beta0 and beta1 are
##always in the intervals.

