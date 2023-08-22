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


