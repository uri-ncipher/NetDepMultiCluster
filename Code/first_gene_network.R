library(dplyr)
library(igraph)

popn <- 500
n.max.cluster <- 10

# generate nodes information for each cluster 
generate_nodes_number <- function(n.cluster,popn = 500){
  n.nodes.per.cluster <- popn / n.cluster
  if(n.nodes.per.cluster %% 2 == 0 ){
    nodes.start <- seq(1,popn,n.nodes.per.cluster)
    nodes.end <- seq(n.nodes.per.cluster,popn,n.nodes.per.cluster)
    nodes.interval <- rep(n.nodes.per.cluster,n.cluster)
  }else{
    n.nodes.per.cluster <- ifelse((popn %/% n.cluster ) %% 2 ==0,popn %/% n.cluster,
                                  popn %/% n.cluster-1) 
    diff.sum <- popn - n.nodes.per.cluster * n.cluster
    added.nodes.cluster <- diff.sum /2 
    nodes.interval <- rep(n.nodes.per.cluster,n.cluster)
    nodes.interval[(n.cluster - added.nodes.cluster+1): n.cluster] <-
      nodes.interval[(n.cluster - added.nodes.cluster+1): n.cluster]  +2
    nodes.start <- NULL
    nodes.start[1] <- 1
    nodes.end <- NULL
    nodes.end[1] <- nodes.interval[1]
    for(i in 2:n.cluster){
      nodes.start[i] <- nodes.start[i-1] + nodes.interval[i-1]
      nodes.end[i] <- nodes.end[i-1] + nodes.interval[i]
    }
  }
  return(nodes = list(nodes.interval = nodes.interval,
                      nodes.start = nodes.start,
                      nodes.end = nodes.end))  
}


nodes.list <- list()
for(i in 1: n.max.cluster){
  nodes.list[[i]] <- generate_nodes_number(i)
}

sum.graphs<- list()
for(i in 1:n.max.cluster){
  sum.graphs[[i]] <- sample_k_regular(nodes.list[[i]]$nodes.interval[1],5) 
  if(i >1){
    for(j in 2:i){
      sum.graphs[[i]] <- sum.graphs[[i]]+
        sample_k_regular(nodes.list[[i]]$nodes.interval[j],5) 
    }
  }
}

### random  remove 30% edges for a given adjacency matrix
new.adjacency <- function(Adj,miss.prob=0.7){
  new.Adj <- Adj
  popn <- nrow(Adj)
  for(i in 1 : popn){
    for(j in 1 : popn){
      new.Adj[i,j] <- ifelse(Adj[i,j] == 1, rbinom(1,1,miss.prob), Adj[i,j])
    }
    for(j in 1: i){
      new.Adj[i,j] = new.Adj[j,i]
    }
  }
  return(new.Adj)
}

