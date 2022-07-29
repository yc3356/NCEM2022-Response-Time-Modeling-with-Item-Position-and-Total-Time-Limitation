#library(truncnorm)
#library(R2jags)
library(jagsUI)

for (r in 1:10){
for (J in c(10,20,30)){
    for (I in c(200,500,1000)){
      filename <- paste("real/real-","rep",as.character(r),"-J",as.character(J),"-I",as.character(I),".rds",collapse="",sep="")
      real <- readRDS(filename)
    
      data <- list(JJ=real$JJ,II=real$II,item=real$item,time.limit=real$time.limit,RT=real$RT,logmeanT=real$logmeanT,
                 J=max(real$JJ),I=max(real$II),N=length(real$item))

      inits <- list(
        list(beta=real$beta+rnorm(length(real$beta),0,0.0001),
             tau=real$tau+rnorm(length(real$tau),0,0.0001),
             k=real$k+rnorm(length(real$k),0,0.0001)),
        list(beta=real$beta+rnorm(length(real$beta),0,0.0001),
            tau=real$tau+rnorm(length(real$tau),0,0.0001),
            k=real$k+rnorm(length(real$k),0,0.0001)),
        list(beta=real$beta+rnorm(length(real$beta),0,0.0001),
            tau=real$tau+rnorm(length(real$tau),0,0.0001),
            k=real$k+rnorm(length(real$k),0,0.0001)))

      model.fit <- jagsUI::autojags(model.file='lognormal_TP.txt',data=data,parameters.to.save=c("beta","tau","k"),n.chains = 3,inits=inits)
      
      filename <- paste("res/lognormal_TP","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
      saveRDS(model.fit,filename)
  }
}
}






