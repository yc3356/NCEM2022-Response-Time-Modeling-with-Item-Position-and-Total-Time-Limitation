library(truncnorm)
library(EnvStats)

set.seed(3356)

for (r in 1:10){
  for (J in c(10,20,30)){
    for (I in c(200,500,1000)){
      sdlog <- 1/1.875
      beta <- runif(J,3,4)
      tau <- runif(I,-1,1)
      k <- runif(1,-0.1,0)
      #theta <- runif(J,-1,1)
      L <- 60 * J
      item <- c()
      II <- c()
      JJ <- c()
      RT <- c()
      time.limit <- c()
      for (i in 1:I){
        cur_item <- sample(1:J,replace = F)
        cur_time_limit <- c(L)
        cur_RT <- c(round(rlnormTrunc(n=1,meanlog=beta[cur_item[1]]-tau[i],sdlog=sdlog,min=0,max=L),5))
        cur_j <- c(1)
        indicator <- 0
        for (j in 2:J){
          this_time_limit = L - sum(cur_RT)
          if (this_time_limit<0.01){
            temp=0
          }else{
            temp <- round(rlnormTrunc(n=1,meanlog=beta[cur_item[j]]-tau[i]+k*(j-1),sdlog=sdlog,min=0,max=this_time_limit),5)
          }
          
          if (this_time_limit > 0.01 & temp>0 & (this_time_limit-temp)>0.01){
            cur_time_limit <- c(cur_time_limit, this_time_limit)
            cur_RT <- c(cur_RT,temp)
            cur_j <- c(cur_j,j)       
          }else{
            indicator <- 1
            break
          }
        }
        item <- c(item,cur_item[1:(j-indicator)])
        II <- c(II,rep(i,(j-indicator)))
        JJ <- c(JJ,cur_j)
        RT <- c(RT,cur_RT)
        time.limit <- c(time.limit,cur_time_limit)
      }
      
      if (sum((time.limit-RT)<0.001)>0){print("**********")}
      
      real <- list(JJ=JJ,II=II,item=item,RT=RT,time.limit=time.limit,
                   tau=tau,beta=beta,k=k,sdlog=sdlog,
                   logmeanT=mean(log(RT)))
      filename <- paste("real/real-","rep",as.character(r),"-J",as.character(J),"-I",as.character(I),".rds",collapse="",sep="")
      saveRDS(real,filename)
    }
  }
}


