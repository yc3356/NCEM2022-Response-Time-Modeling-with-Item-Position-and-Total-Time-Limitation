r = 1
tau.ans <- c()
beta.ans <- c()
k.ans <- c()
DIC.ans <- c()
for (J in c(10,20,30)){
    for (I in c(200,500,1000)){
    filename <- paste("real/real-","rep",as.character(r),"-J",as.character(J),"-I",as.character(I),".rds",collapse="",sep="")
    real <- readRDS(filename)
    filename <- paste("res/lognormal","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    fit <- readRDS(filename)
    tau.ans <- c(tau.ans,sqrt(mean((real$tau-fit$q50$tau)^2)))
    beta.ans <- c(beta.ans,sqrt(mean((real$beta-fit$q50$beta)^2)))
    k.ans <- c(k.ans,NA)
    DIC.ans <- c(DIC.ans,round(fit$DIC,3))
    
    filename <- paste("res/lognormal_T","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    fit.t <- readRDS(filename)
    tau.ans <- c(tau.ans,sqrt(mean((real$tau-fit.t$q50$tau)^2)))
    beta.ans <- c(beta.ans,sqrt(mean((real$beta-fit.t$q50$beta)^2)))
    k.ans <- c(k.ans,NA)
    DIC.ans <- c(DIC.ans,round(fit.t$DIC,3))
    
    filename <- paste("res/lognormal_P","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    fit.p <- readRDS(filename)
    tau.ans <- c(tau.ans,sqrt(mean((real$tau-fit.p$q50$tau)^2)))
    beta.ans <- c(beta.ans,sqrt(mean((real$beta-fit.p$q50$beta)^2)))
    k.ans <- c(k.ans,sqrt(mean((real$k-fit.p$q50$k)^2)))
    DIC.ans <- c(DIC.ans,round(fit.p$DIC,3))

    filename <- paste("res/lognormal_TP","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    fit.tp <- readRDS(filename)
    tau.ans <- c(tau.ans,sqrt(mean((real$tau-fit.tp$q50$tau)^2)))
    beta.ans <- c(beta.ans,sqrt(mean((real$beta-fit.tp$q50$beta)^2)))
    k.ans <- c(k.ans,sqrt(mean((real$k-fit.tp$q50$k)^2)))
    DIC.ans <- c(DIC.ans,round(fit.tp$DIC,3))
  }
}


tau.ans <- round(tau.ans,3)
beta.ans <- round(beta.ans,3)
k.ans <- round(k.ans,3)

ans <- data.frame(tau=tau.ans,beta=beta.ans,k=k.ans,DIC=DIC.ans)

write.csv(ans,"ans.csv")
