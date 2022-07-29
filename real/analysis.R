library(ggpubr)
library(ggplot2)
set.seed(3356)
data <- readRDS("data.rds")

result <- readRDS("lognormal-PT-6.rds")
data <- readRDS("data.rds")
est.k <- result$q50$k
mean(est.k<0)
est.tau <- result$q50$tau
est.beta <- result$q50$beta
JJ <- data$JJ
II <- data$II
item <- data$item

trajectory <- list()
for (i in 1:length(est.tau)){
    cur_JJ <- JJ[which(II==i)]
    cur_traj <- c(est.tau[i],est.tau[i] + est.k[i] * 1:(length(cur_JJ)-1))
    trajectory[[i]] <- c(cur_traj)
}


sampled_t <- sample(1:length(est.tau),100)
b1=ggplot()
for(i in sampled_t){
  b1 <- b1 + geom_line(aes(x=position,y=speed),
                    data=data.frame(
                        speed=trajectory[[i]],
                        position=1:length(trajectory[[i]])
                    ))+
                    geom_point(aes(x=position,y=speed),
                    data=data.frame(
                        speed=trajectory[[i]],
                        position=1:length(trajectory[[i]])
                    ))
}
b1 <- b1 + labs(title="Plot 1: Fitted Latent Speed Trajectory over Item Position", x ="Item Position", y = "Latent Speed") +
  theme_minimal(base_size = 12)






result <- readRDS("lognormal-PT-5.rds")
est.k <- result$q50$k
mean(est.k<0)
est.beta <- result$q50$beta
JJ <- data$JJ
II <- data$II
item <- data$item

trajectory2 <- list()
for (i in 1:length(est.beta)){

    cur_JJ <- sort(unique(JJ[which(item==i)]))
    cur_traj <- est.beta[i] + est.k[i] * (cur_JJ-1)
    trajectory2[[i]] <- data.frame(position=cur_JJ,time.intensity=cur_traj)
}


sampled_2 <- sample(1:length(est.beta),100)
b2=ggplot()
for(i in sampled_2){
  b2 <- b2 + geom_line(aes(x=position,y=time.intensity),
                    data=trajectory2[[i]])+
                    geom_point(aes(x=position,y=time.intensity),
                    data=trajectory2[[i]])
                    
}
b2 <- b2 + labs(title="Plot 2: Fitted Latent Time Intensity Trajectory over Item Position", x ="Item Position", y = "Latent Time Intensity") +
  theme_minimal(base_size = 12)


ggarrange(b1, b2 + rremove("x.text"), ncol = 1, nrow = 2)






lognormal_T <- readRDS('lognormal-T.rds')
# lognormal_PT_6 <- readRDS('lognormal-PT-6.rds')
# lognormal_P_6 <- readRDS('lognormal-P-6.rds')
lognormal <- readRDS('lognormal.rds')
lognormal_p_5 <- readRDS("lognormal-P-5.rds")
lognormal_PT_5 <- readRDS("lognormal-PT-5.rds")

lognormal_T_tau <- lognormal_T$q50$tau
lognormal_tau <- lognormal$q50$tau
lognormal_p_5_tau <- lognormal_p_5$q50$tau
lognormal_PT_5_tau <- lognormal_PT_5$q50$tau



plotdata3 <- data.frame(
  item = 1:length(lognormal_T_tau),
  lognormal_T = lognormal_T_tau,
  lognormal = lognormal_tau,
  lognormal_p_5 = lognormal_p_5_tau,
  lognormal_PT_5 = lognormal_PT_5_tau
)
library(ggplot2)
ggplot(data=plotdata3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_T_tau,color="lognormal-T"),size=3,alpha=0.3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_tau,color="lognormal"),size=3,alpha=0.3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_p_5_tau,color="lognormal-P"),size=3,alpha=0.3)+
  geom_abline(intercept = 0, slope = 1)+
  labs(x ="Fitted Latent Speed of Lognormal-PT", y = "Fitted Latent Speed of Lognormal/Lognormal-P/Lognormal-T") +
  theme_minimal(base_size = 12) + 
  theme_gray(base_size = 14)
ggsave('tau_cor.png')

# p2 <- ggplot(data=plotdata3)+
#   geom_point(aes(x=est6,y=est1,shape="lognormal-T"),size=3,alpha=0.5)+
#   geom_point(aes(x=est6,y=est5,shape="lognormal-P"),size=3,alpha=0.5)+
#   geom_point(aes(x=est6,y=est4,shape="lognormal"),size=3,alpha=0.5)+
#   geom_abline(intercept = 0, slope = 1)+
#   labs(x ="Fitted Latent Speed of Lognormal-PT (Formula 5)", y = "Fitted Latent Speed of Lognormal/Lognormal-P/Lognormal-T") +
#   theme_minimal(base_size = 12)


# ggarrange(p2, p1 , ncol = 2, nrow = 1,common.legend = TRUE)
# cor(plotdata3$est6,plotdata3$est2)





lognormal_T <- readRDS('lognormal-T.rds')
# lognormal_PT_6 <- readRDS('lognormal-PT-6.rds')
# lognormal_P_6 <- readRDS('lognormal-P-6.rds')
lognormal <- readRDS('lognormal.rds')
lognormal_p_5 <- readRDS("lognormal-P-5.rds")
lognormal_PT_5 <- readRDS("lognormal-PT-5.rds")

lognormal_T_tau <- lognormal_T$q50$beta
lognormal_tau <- lognormal$q50$beta
lognormal_p_5_tau <- lognormal_p_5$q50$beta
lognormal_PT_5_tau <- lognormal_PT_5$q50$beta

plotdata3 <- data.frame(
  item = 1:length(lognormal_T_tau),
  lognormal_T = lognormal_T_tau,
  lognormal = lognormal_tau,
  lognormal_p_5 = lognormal_p_5_tau,
  lognormal_PT_5 = lognormal_PT_5_tau
)
ggplot(data=plotdata3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_T_tau,color="lognormal-T"),size=3,alpha=0.3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_tau,color="lognormal"),size=3,alpha=0.3)+
  geom_point(aes(x=lognormal_PT_5_tau,y=lognormal_p_5_tau,color="lognormal-P"),size=3,alpha=0.3)+
  geom_abline(intercept = 0, slope = 1)+
  labs(x ="Fitted Latent Time Intensity of Lognormal-PT", y = "Fitted Latent Time Intensity of Lognormal/Lognormal-P/Lognormal-T") +
  theme_minimal(base_size = 12) + 
  theme_gray(base_size = 14)
ggsave('beta_cor.png')


















# est.beta.1 <- fit1$q50$beta
# est.beta.2 <- fit2$q50$beta
# est.beta.3 <- fit3$q50$beta
# est.beta.4 <- fit4$q50$beta
# est.beta.5 <- fit5$q50$beta
# est.beta.6 <- fit6$q50$beta

# plotdata4 <- data.frame(
#   item=1:length(est.beta.1),
#   est1=est.beta.1,
#   est2=est.beta.2,
#   est3=est.beta.3,
#   est4=est.beta.4,
#   est5=est.beta.5,
#   est6=est.beta.6
# )
# cor(plotdata4$est6,plotdata4$est2)
# #plotdata3 <- plotdata3[sampled,]
# library(ggplot2)
# p1 <- ggplot(data=plotdata4)+
#   geom_point(aes(x=est2,y=est1,shape="lognormal-T"),size=3,alpha=0.5)+
#   geom_point(aes(x=est2,y=est3,shape="lognormal-P"),size=3,alpha=0.5)+
#   geom_point(aes(x=est2,y=est4,shape="lognormal"),size=3,alpha=0.5)+
#   geom_abline(intercept = 0, slope = 1)+
#   labs(x ="Fitted Latent Time Intensity of Lognormal-PT (Formula 6)", y = "Fitted Latent Time Intensity of Lognormal/Lognormal-P/Lognormal-T") +
#   theme_minimal(base_size = 12)



# p2 <- ggplot(data=plotdata4)+
#   geom_point(aes(x=est6,y=est1,shape="lognormal-T"),size=3,alpha=0.5)+
#   geom_point(aes(x=est6,y=est5,shape="lognormal-P"),size=3,alpha=0.5)+
#   geom_point(aes(x=est6,y=est4,shape="lognormal"),size=3,alpha=0.5)+
#   geom_abline(intercept = 0, slope = 1)+
#   labs(x ="Fitted Latent Time Intensity of Lognormal-PT (Formula 5)", y = "Fitted Latent Time Intensity of Lognormal/Lognormal-P/Lognormal-T") +
#   theme_minimal(base_size = 12)

# ggarrange(p2, p1 + rremove("x.text"), ncol = 2, nrow = 1,common.legend = TRUE)









# est.tau.1 <- fit2$q50$k
# est.tau.2 <- fit3$q50$k
# est.tau.3 <- fit5$q50$k
# est.tau.4 <- fit6$q50$k


# plotdata5 <- data.frame(
#   item=1:length(est.tau.1),
#   est1=est.tau.1,
#   est2=est.tau.2
# )

# plotdata6 <- data.frame(
#   item=1:length(est.tau.3),
#   est3=est.tau.3,
#   est4=est.tau.4
# )

# p1 <- ggplot(data=plotdata5)+
#   geom_point(aes(x=est1,y=est2),size=2,alpha=0.5)+
#   geom_abline(intercept = 0, slope = 1)+
#   labs(x ="Fitted Latent Linear Weight of Position Effect of Lognormal-PT (Formula 6)", y = "Fitted Latent Linear Weight of Position Effect of Lognormal/Lognormal-P/Lognormal-T") +
#   theme_minimal(base_size = 12)


# p2 <- ggplot(data=plotdata6)+
#   geom_point(aes(x=est3,y=est4),size=2,alpha=0.5)+
#   geom_abline(intercept = 0, slope = 1)+
#   labs(x ="Fitted Latent Linear Weight of Position Effect of Lognormal-PT (Formula 5)", y = "Fitted Latent Linear Weight of Position Effect of Lognormal/Lognormal-P/Lognormal-T") +
#   theme_minimal(base_size = 12)

# ggarrange(p2, p1 + rremove("x.text"), ncol = 2, nrow = 1,common.legend = TRUE)








# data <- readRDS("data.rds")

# plotdata1 <- data.frame(RT=data$RT)
# p1 <- ggplot(plotdata1, aes(x=RT)) + 
#   geom_histogram(aes(y = stat(density)),alpha=0.8,binwidth=10)+
#   stat_function(fun = dlnorm, n = 100, args = list(meanlog=mean(log(data$RT)), sd = sd(log(data$RT)))) +
#   #geom_density(aes(x = RT))+
#   labs(title="Plot 1: Distribution of Response Times", x ="Response Time (Second)", y = "Density") +
#   theme_minimal(base_size = 10)

# p1

# total.time <- c()
# for (i in 1:data$I){
#   RTs <- data$RT[which(data$II==i)]
#   total.time <- c(total.time, sum(RTs))
# }
# data$time.limit[1]

# plotdata2 <- data.frame(RT=total.time)
# p2 <- ggplot(plotdata2, aes(x=RT)) + 
#   geom_histogram(aes(y = stat(density)),alpha=0.8,binwidth=200)+
#   #geom_density(aes(x = RT),alpha=0.5)+
#   geom_vline(xintercept = 7200,size=1.5)+
#   labs(title="Plot 2: Distribution of Total Response Times", x ="Total Response Time (second)", y = "Density") +
#   theme_minimal(base_size = 10)




# position.mean.time <- c()
# for (i in 1:64){
#   RTs <- data$RT[which(data$JJ==i)]
#   position.mean.time <- c(position.mean.time, median(RTs))
# }
# remaining.time.mean <- c()
# for (i in 1:64){
#   RTs <- data$time.limit[which(data$JJ==i)]
#   remaining.time.mean <- c(remaining.time.mean, mean(RTs))
# }

# plotdata3 <- data.frame(position=position.mean.time,
#                         remaining=remaining.time.mean,
#                         indexes=1:64)
# p3 <- ggplot(data=plotdata3,aes(x=indexes,y=position)) + 
#   geom_point()  +
#   geom_smooth(method = "lm", se = FALSE,color="Black") +
#   labs(title="Plot 3: Relationship Between Item Position and Median Response Time", x ="Item Position", y = "Median Response Time") +
#   theme_minimal(base_size = 10)
# p3



# p4 <- ggplot(data=plotdata3) + 
#   geom_line(aes(x=indexes,y=remaining))  +
#   labs(title="Plot 4: Relationship Between Item Position and Median Remaining Time", x ="Item Position", y = "Mean Remaining Time") +
#   theme_minimal(base_size = 10)
# p4




# library(ggpubr)
# ggarrange(p1, p2, p3, p4 + rremove("x.text"), ncol = 2, nrow = 2)




# fit1$DIC - 2*fit1$pD
# fit2$DIC - 2*fit2$pD
# fit3$DIC - 2*fit3$pD
# fit4$DIC - 2*fit4$pD
# fit5$DIC - 2*fit5$pD
# fit6$DIC - 2*fit6$pD

# fit1 <- readRDS('lognormal-T.rds')
# fit2 <- readRDS('lognormal-PT-6.rds')
# fit3 <- readRDS('lognormal-P-6.rds')
# fit4 <- readRDS('lognormal.rds')
# fit5 <- readRDS("lognormal-P-5.rds")
# fit6 <- readRDS("lognormal-PT-5.rds")


# library(jagsUI)
# pp.check(x=fit,observed = 'tau', simulated = 'tau.new')




# library(loo)
# log_like <- a[,,"deviance"] / (-2)
# loo(log_like)

# relative_eff(exp(log_like), chain_id = rep(1:3, each = 1000))







