#begin= start time of the process
 # end= end time of the process
 #step= incremental time step
 # a, p for penalty term, r=reflection, X0= intitial starting point
 #  drift and sigma related to the process must be functions or numbers.
 SDE_RF <- function(begin, end,step=0.01,drift=0,sigma=1,a=100,p=0.1,r=1, X0=0){
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
   if(is.function(drift) & is.function(sigma)){
   ## drift
   d<- as.function(alist(x=,drift))
   drift<-d(1)
   ## sigma
   sig<- as.function(alist(x=,sigma))
   sigma<-sig(1)
   for (i in 2:n) {
     X[i] <-  X[i-1]+ drift(X[i-1])*dt + penalty(a,p,r,X[i-1])*dt + sigma(X[i-1])*dw[i]
     }
   }
   else if(is.function(drift) & !is.function(sigma)){
       ## drift
       d<- as.function(alist(x=,drift))
       drift<-d(1)
       for (i in 2:n) {
       X[i] <-  X[i-1]+ drift(X[i-1])*dt + penalty(a,p,r,X[i-1])*dt + sigma*dw[i]
     }
     }
   else if(!is.function(drift) & is.function(sigma)){
       ## sigma
       sig<- as.function(alist(x=,sigma))
       sigma<-sig(1)
       for (i in 2:n) {
        X[i] <-  X[i-1]+ drift*dt + penalty(a,p,r,X[i-1])*dt + sigma(X[i-1])*dw[i]
       }
    } else {
        for (i in 2:n) {
        X[i] <-  X[i-1]+ drift*dt + penalty(a,p,r,X[i-1])*dt + sigma*dw[i]
       }
      }
   return(data.frame(t,X));
 }
 
 ## Example
 ## Your drift and sigma must be functions or numbers 
 drift<-function(x){4*(2-x)} # drift term
 sigma1<-function(x){2*x}    # volatility term
 BM<-SDE_RF(0,10,step = 0.001,drift = drift, sigma = sigma1)
 plot(BM$t,BM$X, type="l", xlab="X", ylab="t")
 
 
      