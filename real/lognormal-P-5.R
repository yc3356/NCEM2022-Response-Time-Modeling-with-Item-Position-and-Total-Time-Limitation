library(R2jags)
library(jagsUI)

jags.data <- readRDS("data.rds")
fit <- autojags(model.file="lognormal-P-5.txt",data=jags.data,parameters.to.save=c("tau","beta","k"),n.chains = 2,Rhat.limit=1.3)
saveRDS(fit,"lognormal-P-5.rds")
