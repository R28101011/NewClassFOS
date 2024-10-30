dist="normal"

m= 370  # m =c(370, 740, 1111)
ppn = 0.90 # Pn=c(0.95, 0.90, 0.80, 0.70)
sd= c(0.5, 1, 2) 

FOS = function(x, pn = ppn, alpha = 0.0027){
  Rx = sort(x)
  n = length(x)
  n1 = n + 1
  pp0=function(n){pnorm(n/m-1, mean =0, sd =sd[1])}
  p0=pp0(n)
  Xu = function(u){
    if (0<u && u<=1/(p0*n1)) {
      Rx[1]+(Rx[2]-Rx[1])*log(p0*n1*u)-(Rx[2]-Rx[1])*(log(p0*n1*u))^2
    } else if (1/(n1*p0)<u && u<1-1/(n1*p0)) {
      (1-(n1*u-floor(n1*u)))*Rx[floor(n1*u)]+(n1*u-floor(n1*u))*Rx[floor(n1*u)+1]
    } else {
      Rx[n]-(Rx[n]-Rx[n-1])*log(p0*n1*(1-u))+(Rx[n]-Rx[n-1])*(log(p0*n1*(1-u)))^2
    }
  }
  findkl = function(r, n, alpha = alpha, pn = pn ) pbeta(alpha/2, r, n-r+1)-(1+pn)/2
  findku = function(r, n, alpha = alpha, pn = pn ) pbeta(1-alpha/2, r, n-r+1)-(1-pn)/2
  kl <- uniroot(findkl, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ku <- uniroot(findku, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ur = kl/n1
  us = ku/n1
  LCL = Xu(ur)
  UCL = Xu(us)
  return(list(UCL=UCL,LCL=LCL))
}

FOS_1 = function(x, pn = ppn, alpha = 0.0027){
  Rx = sort(x)
  n = length(x)
  n1 = n + 1
  pp0=function(n){pnorm(n/m-1, mean =0, sd =sd[2])}
  p0=pp0(n)
  Xu = function(u){
    if (0<u && u<=1/(p0*n1)) {
      Rx[1]+(Rx[2]-Rx[1])*log(p0*n1*u)-(Rx[2]-Rx[1])*(log(p0*n1*u))^2
    } else if (1/(n1*p0)<u && u<1-1/(n1*p0)) {
      (1-(n1*u-floor(n1*u)))*Rx[floor(n1*u)]+(n1*u-floor(n1*u))*Rx[floor(n1*u)+1]
    } else {
      Rx[n]-(Rx[n]-Rx[n-1])*log(p0*n1*(1-u))+(Rx[n]-Rx[n-1])*(log(p0*n1*(1-u)))^2
    }
  }
  findkl = function(r, n, alpha = alpha, pn = pn ) pbeta(alpha/2, r, n-r+1)-(1+pn)/2
  findku = function(r, n, alpha = alpha, pn = pn ) pbeta(1-alpha/2, r, n-r+1)-(1-pn)/2
  kl <- uniroot(findkl, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ku <- uniroot(findku, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ur = kl/n1
  us = ku/n1
  LCL = Xu(ur)
  UCL = Xu(us)
  return(list(UCL=UCL,LCL=LCL))
}

FOS_2 = function(x, pn = ppn, alpha = 0.0027){
  Rx = sort(x)
  n = length(x)
  n1 = n + 1
  pp0=function(n){pnorm(n/m-1, mean =0, sd = sd[3])}
  p0=pp0(n)
  Xu = function(u){
    if (0<u && u<=1/(p0*n1)) {
      Rx[1]+(Rx[2]-Rx[1])*log(p0*n1*u)-(Rx[2]-Rx[1])*(log(p0*n1*u))^2
    } else if (1/(n1*p0)<u && u<1-1/(n1*p0)) {
      (1-(n1*u-floor(n1*u)))*Rx[floor(n1*u)]+(n1*u-floor(n1*u))*Rx[floor(n1*u)+1]
    } else {
      Rx[n]-(Rx[n]-Rx[n-1])*log(p0*n1*(1-u))+(Rx[n]-Rx[n-1])*(log(p0*n1*(1-u)))^2
    }
  }
  findkl = function(r, n, alpha = alpha, pn = pn ) pbeta(alpha/2, r, n-r+1)-(1+pn)/2
  findku = function(r, n, alpha = alpha, pn = pn ) pbeta(1-alpha/2, r, n-r+1)-(1-pn)/2
  kl <- uniroot(findkl, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ku <- uniroot(findku, c(0, n1), n = n, alpha = alpha, pn = pn)$root
  ur = kl/n1
  us = ku/n1
  LCL = Xu(ur)
  UCL = Xu(us)
  return(list(UCL=UCL,LCL=LCL))
}

###
all.n=c(741,371,1481,1482,1111,1112,1851,seq(50,2500,by=10))
# all.n <- c(50, 100)

D <- qnorm(p = 0.99865) - qnorm(p = 0.00135)
#D <- qchisq(p = 0.99865, 4) - qchisq(p = 0.00135, 4)
# D <- qbeta(p = 0.99865, shape1 = 9, shape2 = 1) - qbeta(p = 0.00135, shape1 = 9, shape2 = 1)
#D <- qlnorm(p = 0.99865, meanlog = 0, sdlog = 1)- qlnorm(p = 0.00135, meanlog = 0, sdlog = 1)
#D <- qt(p = 0.99865, df = 4)- qt(p = 0.00135, df = 4)
###
ind.len　<- c()
num.n <- length(all.n)
for (k in 1:num.n){
  ind0 <- seq(6*k-2, length.out=3)
  ind.len <- c(ind.len, ind0)
}

###
library(doParallel)
library(foreach)
cl <- makeCluster(detectCores()-2)
registerDoParallel(cl)
start.time <- Sys.time()
result <-foreach(n=all.n, .combine='cbind') %:%
  foreach(B=1:100, .combine='rbind') %dopar% {
    ###
    x0 <- rnorm(n=n)
    x1 <- rnorm(n=n)
    #x0 <- rchisq(n=n, df = 4)
    #x1 <- rchisq(n=n, df = 4)
    # x0 <- rbeta(n=n, shape1 = 9, shape2 = 1)
    # x1 <- rbeta(n=n, shape1 = 9, shape2 = 1)
    #x0 <- rlnorm(n=n, meanlog = 0, sdlog = 1)
    #x1 <- rlnorm(n=n, meanlog = 0, sdlog = 1)
    #x0 <- rt(n=n, df = 4)
    #x1 <- rt(n=n, df = 4)
    ###
    FOS_limits <- FOS(x0)
    FOS_1_limits <- FOS_1(x0)
    FOS_2_limits <- FOS_2(x0)
    FOS_len <- FOS_limits$UCL-FOS_limits$LCL
    FOS_1_len <- FOS_1_limits$UCL-FOS_1_limits$LCL
    FOS_2_len <- FOS_2_limits$UCL-FOS_2_limits$LCL
    FOS_pa<- mean(sapply(x1, FUN = function(x) x > FOS_limits$UCL | x < FOS_limits$LCL)) < 0.0027
    FOS_1_pa<- mean(sapply(x1, FUN = function(x) x > FOS_1_limits$UCL | x < FOS_1_limits$LCL)) < 0.0027
    FOS_2_pa<- mean(sapply(x1, FUN = function(x) x > FOS_2_limits$UCL | x < FOS_2_limits$LCL)) < 0.0027
    return(c(FOS_pa, FOS_1_pa, FOS_2_pa, FOS_len, FOS_1_len, FOS_2_len))
  }
stopCluster(cl)
res1 <- matrix(colMeans(result), ncol = 6, byrow = T)
end.time <- Sys.time()
end.time - start.time
pa <-  res1[,1:3]
Dbar <- res1[,4:6]
rrmse2 <- matrix( sqrt(colMeans((result[,ind.len]-D)^2))/D, ncol = 3, byrow = T)
RRMSE <- ifelse(( Dbar-D)>0, 1,-1 )*rrmse2
out <- cbind(pa, RRMSE)
row.names(out)<-all.n
colnames(out)<-c("FOS.Pa","FOS_1.Pa","FOS_2.Pa","FOS.Len","FOS_1.Len","FOS_2.Len")
###
setwd("~/Papercode")
write.csv(x = out,file =paste0(dist,"_Ad_FOS_Pn=",ppn,"_m=",m,"_s=0.5_1_2",".csv"))

