library(dplyr)
library(tidyr)
library(kableExtra)
library(igraph)
library(energy)
library(plotrix)
library(ggplot2)

cor.coef <- array(0, c(n.sim, n.max.cluster, (max.time+1)))
dis.coer <- array(0, c(n.sim, n.max.cluster, (max.time+1)))
dis.p <- array(0, c(n.sim, n.max.cluster, (max.time+1)))
lm.p <- array(0,c(n.sim, n.max.cluster, (max.time+1)))
lm.beta <- array(0,c(n.sim, n.max.cluster, (max.time+1)))
z.beta <- array(0,c(n.sim, n.max.cluster, (max.time+1)))


for(j in 1:n.max.cluster){
  for(i in 1:n.sim){
    for(t in 1:(max.time+1)){
      cor.coef[i,j,t] <-  cor(outcome[[j]][i,,t],covariate[[j]][i,,t])
      dis.coer[i,j,t] <- dcor(outcome[[j]][i,,t],covariate[[j]][i,,t])
      dd <- dcorT.test(outcome[[j]][i,,t],covariate[[j]][i,,t])
      dis.p[i,j,t] <-dd$p.value
      lt <- lm(outcome[[j]][i,,t]~covariate[[j]][i,,t])
      ls <- summary(lt)
      lm.beta[i,j,t] <- ls$coefficients[2,"Estimate"]
      lm.p[i,j,t] <- ls$coefficients[2,4] ## p value for linear regression test, p value.
      z.beta[i,j,t] <- lm.beta[i,j,t]/ (ls$coefficients[2,"Std. Error"] * sd(covariate[[j]][i,,t]))
    }
  }
}


list.p.dist <- list()
for(j in 1:n.max.cluster){
  list.p.dist[[j]] <- 
    data.frame("times" = 0:(max.time),"rejection rates" =  sapply(1:(max.time+1), 
                                                                  function(t) mean(dis.p[,j,t] < 0.05)))
}

list.p.linear <- list()
for(j in 1:n.max.cluster){
  list.p.linear[[j]] <- 
    data.frame("times" = 0:(max.time),"rejection rates" =  sapply(1:(max.time+1), 
                                                                  function(t) mean(lm.p[,j,t] < 0.05)))
}

reject.linear.t <- sapply(1:10, function(x) list.p.linear[[x]]$rejection.rates) %>% 
  as.data.frame() 
colnames(reject.linear.t ) <- 1:10
rownames(reject.linear.t) <- paste0("t",0:max.time)


reject.dcor.t <- sapply(1:10, function(x) list.p.dist[[x]]$rejection.rates) %>% 
  as.data.frame() 
colnames(reject.dcor.t ) <- 1:10
rownames(reject.dcor.t) <- paste0("t",0:max.time)








