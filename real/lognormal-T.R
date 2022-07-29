library(jagsUI)

jags.data <- readRDS("data.rds")
fit <- autojags(model.file="lognormal-T.txt",data=jags.data,parameters.to.save=c("tau","beta"),n.chains = 2,Rhat.limit=1.3)
saveRDS(fit,"lognormal-T.rds")
