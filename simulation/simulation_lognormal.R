library(truncnorm)
library(R2jags)
library(modeest)
library(jagsUI)


for (r in 1:10){
for (J in c(10,20,30)){
    for (I in c(200,500,1000)){
    filename <- paste("real/real-","rep",as.character(r),"-J",as.character(J),"-I",as.character(I),".rds",collapse="",sep="")
    
    real <- readRDS(filename)
    
    data <- list(JJ=real$JJ,II=real$II,item=real$item,time.limit=real$time.limit,RT=real$RT,logmeanT=real$logmeanT,
                 J=max(real$JJ),I=max(real$II),N=length(real$item))
    
    model.fit <- jagsUI::autojags(model.file='lognormal.txt',data=data,parameters.to.save=c("beta","tau"),n.chains = 2)
    
    
    filename <- paste("res/lognormal","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    saveRDS(model.fit,filename)
}
}
}

