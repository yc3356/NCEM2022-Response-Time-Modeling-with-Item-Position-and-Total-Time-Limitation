library(rjags)
library(R2jags)
library(loo)
library(DHARMa)

data <- readRDS("real/data.rds")
chains=3
## log-normal model
model.fit <- jags(model.file="real/lognormal.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal.rds")
## lognormal-T
model.fit <- jags(model.file="real/lognormal-T.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal-T.rds")
## lognormal-P-5
model.fit <- jags(model.file="real/lognormal-P-5.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal-P-5.rds")
## lognormal-TP-5
model.fit <- jags(model.file="real/lognormal-P-6.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal-P-6.rds")
## lognormal-TP-5
model.fit <- jags(model.file="real/lognormal-PT-5.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal-PT-5.rds")
## lognormal-TP-6
model.fit <- jags(model.file="real/lognormal-PT-6.txt",data = data, n.chains=chains,parameters.to.save=c("loglike","predRT"))
saveRDS(model.fit,"lognormal-PT-6.rds")




# model.fit$BUGSoutput$DIC

# # Compute WAIC
# loglike <- model.fit$BUGSoutput$sims.list$loglike
# WAIC <- waic(loglike)



# simulations =  model.fit$BUGSoutput$sims.list$predRT
# pred = apply(model$BUGSoutput$sims.list$lambda, 2, median)
# dim(simulations)
# sim = createDHARMa(simulatedResponse = t(simulations), observedResponse = data$RT)
# plot(sim)

# testDispersion(sim)


# model.fit$