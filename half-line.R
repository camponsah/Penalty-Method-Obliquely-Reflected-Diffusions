
#begin= start time of the process
# end= end time of the process
#step= incremental time step
# a, p for penalty term, r=reflection, X0= intitial starting point
# 
SDE_RF <- function(begin, end,step=0.01,a=100,p=0.1,r=1, X0=0){
  t<-seq(begin,end,step) ### create sequence 
  n=length(t)
  dt  <- step
  dw<-rnorm(n, 0, sqrt(step))
  X<-NULL
  X[1] <- X0
  ## penalty function
  penalty<- function(a,p,r,x){
    ifelse(x>=0,0,r*a*((abs(x))^p))
  }
  for (i in 2:n) {
    X[i] <-  X[i-1] -dt + penalty(a,p,r,X[i-1])*dt + dw[i]
  }
  return(data.frame(t,X));
}


#BM<-SDE_RF(0,1)
