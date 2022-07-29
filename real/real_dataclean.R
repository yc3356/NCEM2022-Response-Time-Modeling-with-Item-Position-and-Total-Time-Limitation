library(tidyverse)
library(haven)
library(mice)
library(sjlabelled)
data <- read_sav("CY6_MS_CMB_STU_TTM.sav")
data <- data %>% filter(CNT=="USA")
#whole <- data
#data <- whole

visit <- data %>% select(ends_with("V"))
onlyone <- which(apply(visit > 1,1,sum,na.rm=T)==0)
data <- data[onlyone,]


data <- data %>% filter(BOOKID>=31 & BOOKID <=66)
BOOKID <- data$BOOKID
CBASCI <- data$CBASCI
CBASCI[which(CBASCI==6)] = 5
CBASCI[which(CBASCI==7)] = 6
data <- data %>% select(ends_with("TT"))
data <- data[,colSums(is.na(data))<nrow(data)]
colnames(data) <- str_replace_all(colnames(data), "TT", "")



## code the invalid responses as NA

# for (i in 1:ncol(data)){
#   labels <- sjlabelled::get_values(data[,i])[[1]]
#   for (l in labels){
#     data[,i][which(data[,i] == l)] = 0
#   }
# }



# science cluster
table(CBASCI)
science_cluster <- list()
science_cluster[[1]] <- c("s01","s07")
science_cluster[[2]] <- c("s01","s10")
science_cluster[[3]] <- c("s02","s08")
science_cluster[[4]] <- c("s03","s09")
science_cluster[[5]] <- c("s03","s12")
science_cluster[[6]] <- c("s04","s07")
science_cluster[[7]] <- c("s04","s10")
science_cluster[[8]] <- c("s05","s11")
science_cluster[[9]] <- c("s06","s12")
science_cluster[[10]] <- c("s07","s06")
science_cluster[[11]] <- c("s08","s01")
science_cluster[[12]] <- c("s08","s05")
science_cluster[[13]] <- c("s09","s02")
science_cluster[[14]] <- c("s09","s06")
science_cluster[[15]] <- c("s10","s03")
science_cluster[[16]] <- c("s11","s02")
science_cluster[[17]] <- c("s11","s04")
science_cluster[[18]] <- c("s12","s05")
science_cluster[[19]] <- c("s07","s08")
science_cluster[[20]] <- c("s07","s09")
science_cluster[[21]] <- c("s07","s11")
science_cluster[[22]] <- c("s08","s10")
science_cluster[[23]] <- c("s08","s12")
science_cluster[[24]] <- c("s09","s08")
science_cluster[[25]] <- c("s09","s11")
science_cluster[[26]] <- c("s10","s07")
science_cluster[[27]] <- c("s10","s09")
science_cluster[[28]] <- c("s10","s12")
science_cluster[[29]] <- c("s11","s08")
science_cluster[[30]] <- c("s11","s10")
science_cluster[[31]] <- c("s12","s07")
science_cluster[[32]] <- c("s12","s09")
science_cluster[[33]] <- c("s12","s11")
science_cluster[[34]] <- c("s02","s04")
science_cluster[[35]] <- c("s05","s01")
science_cluster[[36]] <- c("s06","s03")

# booklet
table(BOOKID)
cluster <- list()
cluster[[31]] <- c("s","s","R1","R2")
cluster[[32]] <- c("s","s","R2","R3")
cluster[[33]] <- c("s","s","R3","R4")
cluster[[34]] <- c("s","s","R4","R5")
cluster[[35]] <- c("s","s","R5","R6")
cluster[[36]] <- c("s","s","R6","R1")
cluster[[37]] <- c("R1","R3","s","s")
cluster[[38]] <- c("R2","R4","s","s")
cluster[[39]] <- c("R3","R5","s","s")
cluster[[40]] <- c("R4","R6","s","s")
cluster[[41]] <- c("R5","R1","s","s")
cluster[[42]] <- c("R6","R2","s","s")
cluster[[43]] <- c("s","s","M1","M2")
cluster[[44]] <- c("s","s","M2","M3")
cluster[[45]] <- c("s","s","M3","M4")
cluster[[46]] <- c("s","s","M4","M5")
cluster[[47]] <- c("s","s","M5","M6")
cluster[[48]] <- c("s","s","M6","M1")
cluster[[49]] <- c("M1","M3","s","s")
cluster[[50]] <- c("M2","M4","s","s")
cluster[[51]] <- c("M3","M5","s","s")
cluster[[52]] <- c("M4","M6","s","s")
cluster[[53]] <- c("M5","M1","s","s")
cluster[[54]] <- c("M6","M2","s","s")
cluster[[55]] <- c("s","s","M1","R1")
cluster[[56]] <- c("s","s","R2","M2")
cluster[[57]] <- c("s","s","M3","R3")
cluster[[58]] <- c("s","s","R4","M4")
cluster[[59]] <- c("s","s","M5","R5")
cluster[[60]] <- c("s","s","R6","M6")
cluster[[61]] <- c("R1","M1","s","s")
cluster[[62]] <- c("M2","R2","s","s")
cluster[[63]] <- c("R3","M3","s","s")
cluster[[64]] <- c("M4","R4","s","s")
cluster[[65]] <- c("R5","M2","s","s")
cluster[[66]] <- c("M6","R6","s","s")


cluster_science_match <- matrix(c(1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29,
                                  1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29,
                                  1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  35,4,7,19,23,30,
                                  34,15,8,20,24,28,
                                  3,36,18,21,27,33,
                                  1,13,6,9,22,25,
                                  2,16,12,10,31,32,
                                  11,5,17,14,26,29),
                                nrow=36,ncol=6,byrow=TRUE)




# cluster order 
cluster_order <- list()
for (i in 1:nrow(data)){
  cur_cluster <- cluster[[BOOKID[i]]]
  cur_science <- science_cluster[[cluster_science_match[(as.numeric(BOOKID[i])-30),CBASCI[i]]]]
  s_index <- which(cur_cluster=="s")
  cur_cluster[s_index[1]] <- cur_science[1]
  cur_cluster[s_index[2]] <- cur_science[2]
  cluster_order[[i]] <- cur_cluster
}


# cluster item 
cluster_item <- list()
cluster_item[["R1"]] <- colnames(data)[1:13]
cluster_item[["R2"]] <- colnames(data)[14:27]
cluster_item[["R3"]] <- colnames(data)[28:42]
cluster_item[["R4"]] <- colnames(data)[43:56]
cluster_item[["R5"]] <- colnames(data)[57:70]
cluster_item[["R6"]] <- colnames(data)[71:86]
cluster_item[["M1"]] <- colnames(data)[87:98]
cluster_item[["M2"]] <- colnames(data)[99:109]
cluster_item[["M3"]] <- colnames(data)[110:120]
cluster_item[["M4"]] <- colnames(data)[121:132]
cluster_item[["M5"]] <- colnames(data)[133:144]
cluster_item[["M6"]] <- colnames(data)[145:156]
cluster_item[["s01"]] <- colnames(data)[157:174]
cluster_item[["s02"]] <- colnames(data)[175:192]
cluster_item[["s03"]] <- colnames(data)[193:209]
cluster_item[["s04"]] <- colnames(data)[210:219]
cluster_item[["s05"]] <- colnames(data)[220:232]
cluster_item[["s06"]] <- colnames(data)[233:242]
cluster_item[["s07"]] <- colnames(data)[243:259]
cluster_item[["s08"]] <- colnames(data)[260:275]
cluster_item[["s09"]] <- colnames(data)[276:291]
cluster_item[["s10"]] <- colnames(data)[292:308]
cluster_item[["s11"]] <- colnames(data)[309:324]
cluster_item[["s12"]] <- colnames(data)[325:340]


science.index <- 157:340

item_index <- list()
item_colnames <- list()
for (i in 1:nrow(data)){
  cur_cluster <- cluster_order[[i]]
  cur_index <- c()
  for (j in cur_cluster){
    cur_index <- c(cur_index,cluster_item[[j]])
  }
  item_colnames[[i]] <- cur_index
  indexes <- c()
  for (k in cur_index){
    indexes <- c(indexes,match(k,colnames(data)))
  }
  item_index[[i]] <- indexes
}


# prepare the input of model data
item <- c()
JJ <- c()
II <- c()
RT <- c()
time.limit <- c()

plot.item <- c()
plot.JJ <- c()
plot.II <- c()
plot.RT <- c()
plot.time.limit <- c()

L <- 120*60
error <- c()
person = 1
for (i in 1:nrow(data)){
  temp <- item_index[[i]]
  temp2 <- (data[i,temp]%>% unlist(., use.names=FALSE))/1000
  cur_limit <- L - c(0,cumsum(temp2)[1:(length(temp2)-1)])
  
  
  if (sum(cur_limit<0,na.rm=TRUE)>0){error <- c(error,i)}
  else if (sum(temp2,na.rm=T)>L){error <- c(error,i)}
  else if (NA %in% temp2){error <- c(error,i)}
  else{
    keep <- which(temp %in% science.index)
    item <- c(item,temp[keep])
    plot.item <- c(plot.item,temp)
    JJ <- c(JJ,keep)
    plot.JJ <- c(plot.JJ,1:length(temp))
    II <- c(II,rep(person,length(keep)))
    plot.II <- c(plot.II,rep(person,length(temp)))
    RT <- c(RT,temp2[keep])
    plot.RT <- c(plot.RT,temp2)
    time.limit <- c(time.limit,cur_limit[keep])
    plot.time.limit <- c(plot.time.limit,cur_limit)
    person = person + 1
  }
}



item = item - 156

jags.data <- list(
  N = length(item),
  II = II,
  JJ = JJ,
  logmeanT=mean(log(RT)),
  RT=RT,
  time.limit=time.limit,
  I = max(II),
  J = max(item),
  item=item
)

saveRDS(jags.data,"data.rds")








### visualization
library(ggplot2)
p1 <- ggplot(data=data.frame(RT=plot.RT))+
  geom_histogram(aes(x=RT, y=..density..),binwidth=5)+
  stat_function(fun = dlnorm, args = list(mean = mean(log(plot.RT)), sd = sd(log(plot.RT))))+
  labs(x="Response Time (Second)",y="Density",title = "Plot 1: Distribution of Response Time")+
  theme_minimal(base_size = 12)

p1
plot.total <- c()
for (i in 1:904){
  plot.total <- c(plot.total,sum(plot.RT[which(plot.II==i)]))
}

p2 <- ggplot(data=data.frame(RT=plot.total))+
  geom_histogram(aes(x=RT),binwidth=500)+
  geom_vline(xintercept=120*60,size=1.5)+
  labs(x="Total Response Time (Second)",y="Count",title = "Plot 2: Distribution of Total Response Time")+
  theme_minimal(base_size = 12)


position.rt <- c()
for (i in 1:65){
  position.rt <- c(position.rt,mean(plot.RT[which(plot.JJ==i)]))
}


p3 <- ggplot(data=data.frame(position.rt=position.rt,poisiton=1:65),aes(x=poisiton,y=position.rt))+
  geom_point()+
  geom_smooth(method = lm, se = FALSE,color="Black") +
  labs(x="Position",y="Average RT across Item and Examinees",title = "Plot 3: Scatter Plot of Item Position and Average Response Time")+
  theme_minimal(base_size = 12)


position.rt <- c()
for (i in 1:65){
  position.rt <- c(position.rt,mean(plot.time.limit[which(plot.JJ==i)]))
}



p4 <- ggplot(data=data.frame(position.rt=position.rt,poisiton=1:65),aes(x=poisiton,y=position.rt))+
  geom_line()+
  labs(x="Position",y="Average Remaining Time across Item and Examinees",title = "Plot 4: Relationship Between Remaining Respones Time and Item Position")+
  theme_minimal(base_size = 12)
library(ggpubr)
ggarrange(p1,p2,p3,p4 , ncol = 2, nrow = 2)
