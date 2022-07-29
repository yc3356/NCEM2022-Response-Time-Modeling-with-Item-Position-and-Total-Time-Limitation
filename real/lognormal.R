library(jagsUI)
library(R2jags)

data <- readRDS("data.rds")
fit <- jagsUI::autojags(model.file="lognormal.txt",data=data,parameters.to.save=c("tau","beta"),n.chains = 2,Rhat.limit=1.3)
saveRDS(fit,"lognormal.rds")
