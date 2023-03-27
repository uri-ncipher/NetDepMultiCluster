library(dplyr)
library(igraph)


n.max.cluster <- 10
n.p.cluster <- 100
total.n <- c(1:10) * 100

# generate nodes information for each cluster
generate_nodes_number <- function(n.cluster){
  nodes.start <- seq(1, total.n[n.cluster], n.p.cluster)
  nodes.end <- seq(1, total.n[n.cluster], n.p.cluster) + 99
  nodes.interval <- rep(n.p.cluster,n.cluster)
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
new.adjacency <- function(Adj, miss.prob=0.7){
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

