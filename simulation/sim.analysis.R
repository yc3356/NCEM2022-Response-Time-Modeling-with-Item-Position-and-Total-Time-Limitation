library(modeest)
library(tidyverse)

## person speed
tau.res <- array(NA,c(10,9,4))
beta.res <- array(NA,c(10,9,4))
true.k <- matrix(NA,nrow=10,ncol=9)
  
    
#for (r in 1:10){
    k = 1
    for (J in c(20,40,60)){
      for (I in c(100,200,500)){
        
        filename <- paste("simulation/real/real-","rep", as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        real <- readRDS(filename)
        true.k[r,k] <- real$k
        
        ## lognormal
        filename <- paste("simulation/res/lognormal","rep", as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        lognormal.est <- readRDS(filename)
        tau.res[r,k,1] <- mean((real$tau - lognormal.est$BUGSoutput$median$tau))
        beta.res[r,k,1] <- mean((real$beta - lognormal.est$BUGSoutput$median$beta))
        ## lognormal-P
        filename <- paste("simulation/res/lognormal_P","rep", as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        lognormalP.est <- readRDS(filename)
        tau.res[r,k,2] <- mean((real$tau - lognormalP.est$BUGSoutput$median$tau))
        beta.res[r,k,2] <- mean((real$beta - lognormalP.est$BUGSoutput$median$beta))
        ## lognormal-T
        filename <- paste("simulation/res/lognormal_T","rep", as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        lognormalT.est <- readRDS(filename)
        tau.res[r,k,3] <- mean((real$tau - lognormalT.est$BUGSoutput$median$tau))
        beta.res[r,k,3] <- mean((real$beta - lognormalT.est$BUGSoutput$median$beta))
        ## lognormal-TP
        filename <- paste("simulation/res/lognormal_TP","rep", as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        lognormalTP.est <- readRDS(filename)
        tau.res[r,k,4] <- mean((real$tau - lognormalTP.est$BUGSoutput$median$tau))
        beta.res[r,k,4] <- mean((real$beta - lognormalTP.est$BUGSoutput$median$beta))

        k = k + 1
    }
  }



mean(tau.res[,1,1])
mean(tau.res[,1,2])
mean(tau.res[,1,3])
mean(tau.res[,1,4])

mean(tau.res[,2,1])
mean(tau.res[,2,2])
mean(tau.res[,2,3])
mean(tau.res[,2,4])

mean(tau.res[,3,1])
mean(tau.res[,3,2])
mean(tau.res[,3,3])
mean(tau.res[,3,4])

mean(tau.res[,4,1])
mean(tau.res[,4,2])
mean(tau.res[,4,3])
mean(tau.res[,4,4])

mean(tau.res[,5,1])
mean(tau.res[,5,2])
mean(tau.res[,5,3])
mean(tau.res[,5,4])

mean(tau.res[,6,1])
mean(tau.res[,6,2])
mean(tau.res[,6,3])
mean(tau.res[,6,4])

mean(tau.res[,7,1])
mean(tau.res[,7,2])
mean(tau.res[,7,3])
mean(tau.res[,7,4])

mean(tau.res[,8,1])
mean(tau.res[,8,2])
mean(tau.res[,8,3])
mean(tau.res[,8,4])

mean(tau.res[,9,1])
mean(tau.res[,9,2])
mean(tau.res[,9,3])
mean(tau.res[,9,4])

# 
mean(beta.res[,1,1])
mean(beta.res[,1,2])
mean(beta.res[,1,3])
mean(beta.res[,1,4])

mean(beta.res[,2,1])
mean(beta.res[,2,2])
mean(beta.res[,2,3])
mean(beta.res[,2,4])














## plot 1
condition1 <- c(tau.res[,1,1],tau.res[,1,2],tau.res[,1,3],tau.res[,1,4])
condition1 <- cbind(condition1, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition1 <- cbind(condition1, sign(rep(true.k[,1],each=4)))
colnames(condition1) <- c("Mean.Bias","Model","sign.gamma")
condition1 <- as.data.frame(condition1)
condition1$Mean.Bias <- as.numeric(condition1$Mean.Bias)
condition1$Model <- as.factor(condition1$Model)
condition1$sign.gamma <- as.factor(condition1$sign.gamma)

p1 <- ggplot(condition1,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
    theme_minimal()



## plot2
condition2 <- c(tau.res[,2,1],tau.res[,2,2],tau.res[,2,3],tau.res[,2,4])
condition2 <- cbind(condition2, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition2 <- cbind(condition2, sign(rep(true.k[,2],each=4)))
colnames(condition2) <- c("Mean.Bias","Model","sign.gamma")
condition2 <- as.data.frame(condition2)
condition2$Mean.Bias <- as.numeric(condition2$Mean.Bias)
condition2$Model <- as.factor(condition2$Model)
condition2$sign.gamma <- as.factor(condition2$sign.gamma)

p2 <- ggplot(condition2,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()



## plot3
condition3 <- c(tau.res[,3,1],tau.res[,3,2],tau.res[,3,3],tau.res[,3,4])
condition3 <- cbind(condition3, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition3 <- cbind(condition3, sign(rep(true.k[,3],each=4)))
colnames(condition3) <- c("Mean.Bias","Model","sign.gamma")
condition3 <- as.data.frame(condition3)
condition3$Mean.Bias <- as.numeric(condition3$Mean.Bias)
condition3$Model <- as.factor(condition3$Model)
condition3$sign.gamma <- as.factor(condition3$sign.gamma)

p3 <- ggplot(condition3,aes(x=Model, y=Mean.Bias
                            ,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()




## plot4
condition4 <- c(tau.res[,4,1],tau.res[,4,2],tau.res[,4,3],tau.res[,4,4])
condition4 <- cbind(condition4, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition4 <- cbind(condition4, sign(rep(true.k[,4],each=4)))
colnames(condition4) <- c("Mean.Bias","Model","sign.gamma")
condition4 <- as.data.frame(condition4)
condition4$Mean.Bias <- as.numeric(condition4$Mean.Bias)
condition4$Model <- as.factor(condition4$Model)
condition4$sign.gamma <- as.factor(condition4$sign.gamma)

p4 <- ggplot(condition4,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot5
condition5 <- c(tau.res[,5,1],tau.res[,5,2],tau.res[,5,3],tau.res[,5,4])
condition5 <- cbind(condition5, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition5 <- cbind(condition5, sign(rep(true.k[,5],each=4)))
colnames(condition5) <- c("Mean.Bias","Model","sign.gamma")
condition5 <- as.data.frame(condition5)
condition5$Mean.Bias <- as.numeric(condition5$Mean.Bias)
condition5$Model <- as.factor(condition5$Model)
condition5$sign.gamma <- as.factor(condition5$sign.gamma)

p5 <- ggplot(condition5,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()



## plot6
condition6 <- c(tau.res[,6,1],tau.res[,6,2],tau.res[,6,3],tau.res[,6,4])
condition6 <- cbind(condition6, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition6 <- cbind(condition6, sign(rep(true.k[,6],each=4)))
colnames(condition6) <- c("Mean.Bias","Model","sign.gamma")
condition6 <- as.data.frame(condition6)
condition6$Mean.Bias <- as.numeric(condition6$Mean.Bias)
condition6$Model <- as.factor(condition6$Model)
condition6$sign.gamma <- as.factor(condition6$sign.gamma)

p6 <- ggplot(condition6,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot7
condition7 <- c(tau.res[,7,1],tau.res[,7,2],tau.res[,7,3],tau.res[,7,4])
condition7 <- cbind(condition7, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition7 <- cbind(condition7, sign(rep(true.k[,7],each=4)))
colnames(condition7) <- c("Mean.Bias","Model","sign.gamma")
condition7 <- as.data.frame(condition7)
condition7$Mean.Bias <- as.numeric(condition7$Mean.Bias)
condition7$Model <- as.factor(condition7$Model)
condition7$sign.gamma <- as.factor(condition7$sign.gamma)

p7 <- ggplot(condition7,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot8
condition8 <- c(tau.res[,8,1],tau.res[,8,2],tau.res[,8,3],tau.res[,8,4])
condition8 <- cbind(condition8, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition8 <- cbind(condition8, sign(rep(true.k[,8],each=4)))
colnames(condition8) <- c("Mean.Bias","Model","sign.gamma")
condition8 <- as.data.frame(condition8)
condition8$Mean.Bias <- as.numeric(condition8$Mean.Bias)
condition8$Model <- as.factor(condition8$Model)
condition8$sign.gamma <- as.factor(condition8$sign.gamma)

p8 <- ggplot(condition8,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()

## plot9
condition9 <- c(tau.res[,9,1],tau.res[,9,2],tau.res[,9,3],tau.res[,9,4])
condition9 <- cbind(condition9, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition9 <- cbind(condition9, sign(rep(true.k[,9],each=4)))
colnames(condition9) <- c("Mean.Bias","Model","sign.gamma")
condition9 <- as.data.frame(condition9)
condition9$Mean.Bias <- as.numeric(condition9$Mean.Bias)
condition9$Model <- as.factor(condition9$Model)
condition9$sign.gamma <- as.factor(condition9$sign.gamma)

p9 <- ggplot(condition9,aes(x=Model, y=Mean.Bias,color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, 
          labels = c("J=5 & I = 100", "J=5 & I = 200", "J=5 & I = 500", 
                     "J=10 & I = 100", "J=10 & I = 200", "J=10 & I = 500",
                     "J=15 & I = 100", "J=15 & I = 200", "J=15 & I = 500"),
          ncol = 3, nrow = 3,common.legend = TRUE)








## plot 1
condition1 <- c(beta.res[,1,1],beta.res[,1,2],beta.res[,1,3],beta.res[,1,4])
condition1 <- cbind(condition1, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition1 <- cbind(condition1, sign(rep(true.k[,1],each=4)))
colnames(condition1) <- c("Mean.Bias","Model","sign.gamma")
condition1 <- as.data.frame(condition1)
condition1$Mean.Bias <- as.numeric(condition1$Mean.Bias)
condition1$Model <- as.factor(condition1$Model)
condition1$sign.gamma <- as.factor(condition1$sign.gamma)

p1 <- ggplot(condition1,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()



## plot2
condition2 <- c(beta.res[,2,1],beta.res[,2,2],beta.res[,2,3],beta.res[,2,4])
condition2 <- cbind(condition2, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition2 <- cbind(condition2, sign(rep(true.k[,2],each=4)))
colnames(condition2) <- c("Mean.Bias","Model","sign.gamma")
condition2 <- as.data.frame(condition2)
condition2$Mean.Bias <- as.numeric(condition2$Mean.Bias)
condition2$Model <- as.factor(condition2$Model)
condition2$sign.gamma <- as.factor(condition2$sign.gamma)

p2 <- ggplot(condition2,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()



## plot3
condition3 <- c(beta.res[,3,1],beta.res[,3,2],beta.res[,3,3],beta.res[,3,4])
condition3 <- cbind(condition3, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition3 <- cbind(condition3, sign(rep(true.k[,3],each=4)))
colnames(condition3) <- c("Mean.Bias","Model","sign.gamma")
condition3 <- as.data.frame(condition3)
condition3$Mean.Bias <- as.numeric(condition3$Mean.Bias)
condition3$Model <- as.factor(condition3$Model)
condition3$sign.gamma <- as.factor(condition3$sign.gamma)

p3 <- ggplot(condition3,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()




## plot4
condition4 <- c(beta.res[,4,1],beta.res[,4,2],beta.res[,4,3],beta.res[,4,4])
condition4 <- cbind(condition4, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition4 <- cbind(condition4, sign(rep(true.k[,4],each=4)))
colnames(condition4) <- c("Mean.Bias","Model","sign.gamma")
condition4 <- as.data.frame(condition4)
condition4$Mean.Bias <- as.numeric(condition4$Mean.Bias)
condition4$Model <- as.factor(condition4$Model)
condition4$sign.gamma <- as.factor(condition4$sign.gamma)

p4 <- ggplot(condition4,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot5
condition5 <- c(beta.res[,5,1],beta.res[,5,2],beta.res[,5,3],beta.res[,5,4])
condition5 <- cbind(condition5, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition5 <- cbind(condition5, sign(rep(true.k[,5],each=4)))
colnames(condition5) <- c("Mean.Bias","Model","sign.gamma")
condition5 <- as.data.frame(condition5)
condition5$Mean.Bias <- as.numeric(condition5$Mean.Bias)
condition5$Model <- as.factor(condition5$Model)
condition5$sign.gamma <- as.factor(condition5$sign.gamma)

p5 <- ggplot(condition5,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()



## plot6
condition6 <- c(beta.res[,6,1],beta.res[,6,2],beta.res[,6,3],beta.res[,6,4])
condition6 <- cbind(condition6, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition6 <- cbind(condition6, sign(rep(true.k[,6],each=4)))
colnames(condition6) <- c("Mean.Bias","Model","sign.gamma")
condition6 <- as.data.frame(condition6)
condition6$Mean.Bias <- as.numeric(condition6$Mean.Bias)
condition6$Model <- as.factor(condition6$Model)
condition6$sign.gamma <- as.factor(condition6$sign.gamma)

p6 <- ggplot(condition6,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot7
condition7 <- c(beta.res[,7,1],beta.res[,7,2],beta.res[,7,3],beta.res[,7,4])
condition7 <- cbind(condition7, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition7 <- cbind(condition7, sign(rep(true.k[,7],each=4)))
colnames(condition7) <- c("Mean.Bias","Model","sign.gamma")
condition7 <- as.data.frame(condition7)
condition7$Mean.Bias <- as.numeric(condition7$Mean.Bias)
condition7$Model <- as.factor(condition7$Model)
condition7$sign.gamma <- as.factor(condition7$sign.gamma)

p7 <- ggplot(condition7,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


## plot8
condition8 <- c(beta.res[,8,1],beta.res[,8,2],beta.res[,8,3],beta.res[,8,4])
condition8 <- cbind(condition8, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition8 <- cbind(condition8, sign(rep(true.k[,8],each=4)))
colnames(condition8) <- c("Mean.Bias","Model","sign.gamma")
condition8 <- as.data.frame(condition8)
condition8$Mean.Bias <- as.numeric(condition8$Mean.Bias)
condition8$Model <- as.factor(condition8$Model)
condition8$sign.gamma <- as.factor(condition8$sign.gamma)

p8 <- ggplot(condition8,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()

## plot9
condition9 <- c(beta.res[,9,1],beta.res[,9,2],beta.res[,9,3],beta.res[,9,4])
condition9 <- cbind(condition9, c(rep("lognormal",100),rep("lognormalP",100),rep("lognormalT",100),rep("lognormalTP",100)))
condition9 <- cbind(condition9, sign(rep(true.k[,9],each=4)))
colnames(condition9) <- c("Mean.Bias","Model","sign.gamma")
condition9 <- as.data.frame(condition9)
condition9$Mean.Bias <- as.numeric(condition9$Mean.Bias)
condition9$Model <- as.factor(condition9$Model)
condition9$sign.gamma <- as.factor(condition9$sign.gamma)

p9 <- ggplot(condition9,aes(x=Model, y=Mean.Bias))+#color=sign.gamma)) + 
  geom_boxplot()+ 
  theme_minimal()


library(ggpubr)
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, 
          labels = c("J=5 & I = 100", "J=5 & I = 200", "J=5 & I = 500", 
                     "J=10 & I = 100", "J=10 & I = 200", "J=10 & I = 500",
                     "J=15 & I = 100", "J=15 & I = 200", "J=15 & I = 500"),
          ncol = 3, nrow = 3,common.legend = TRUE)







ans <- c()
for (J in c(10,20,50)){
  for (I in c(100,500,2000)){
    filename <- paste("real/real-","J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    real <- readRDS(filename)
    
    filename <- paste("res/lognormal","J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    lognormal <- readRDS(filename)
    ans <- rbind(ans,round(c(median(real$tau - lognormal$q50$tau),median(real$beta - lognormal$q50$beta),NA),3))
    
    
    filename <- paste("res/lognormal_T","J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    lognormal.T <- readRDS(filename)
    ans <- rbind(ans,round(c(median(real$tau - lognormal.T$q50$tau),median(real$beta - lognormal.T$q50$beta),NA),3))
    
    filename <- paste("res/lognormal_P","J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    lognormal.P <- readRDS(filename)
    ans <- rbind(ans,round(c(median(real$tau - lognormal.P$q50$tau),median(real$beta - lognormal.P$q50$beta),
                             median(real$k - lognormal.P$q50$k)),3))
    
    filename <- paste("res/lognormal_TP","J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
    lognormal.PT <- readRDS(filename)
    ans <- rbind(ans,round(c(median(real$tau - lognormal.PT$q50$tau),median(real$beta - lognormal.PT$q50$beta),
                             median(real$k - lognormal.PT$q50$k)),3))
    
  }
}





r = 1  
est.tau = c()
est.beta = c()
for (J in c(10,20,30)){
    for (I in c(200,500,1000)){
       filename <- paste("real/real-","rep",as.character(r),"-J",as.character(J),"-I",as.character(I),".rds",collapse="",sep="")
        real <- readRDS(filename)
        true.k[r,k] <- real$k
        
        ## lognormal
        filename <- paste("res/lognormal","rep",as.character(r),"J",as.character(J),"I",as.character(I),".rds",collapse="",sep="")
        lognormal.est <- readRDS(filename)
        est.tau  <- c(est.tau,mean(real$tau - lognormal.est$BUGSoutput$median$tau))
        est.beta <- c(est.tau,mean(real$beta - lognormal.est$BUGSoutput$median$beta))
      }
  }
