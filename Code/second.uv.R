library(igraph)
library(MASS)
library(dplyr)
library(ape)

load("/Data/second.graphds.RData")

n.sim <- 500
max.time <- 1
miss.prob <- 0.8

new.adjacency <- function(Adj,miss.prob=miss.prob){
  new.Adj <- Adj
  popn <- nrow(Adj)
  for(i in 1 : popn){
    for(j in 1 : popn){
      new.Adj[i,j] <- ifelse(Adj[i,j] == 1, rbinom(1,1,miss.prob), Adj[i,j])
    } # add 20% missing edges
    for(j in 1: i){
      new.Adj[i,j] = new.Adj[j,i]
    }
  }
  return(new.Adj)
}


### get uv from give network data
get.uv <- function(n.cluster, nodes,new.A){
  ### n.cluster is the number of cluster in this scenario
  ### nodes is the nodes list
  #### new.A is the adjacency matrix after 30% missing
  furthest_dis<-list()
  Adj <- new.A
  G <- list()
  u <- NULL
  v <- NULL
  for(i in 1:n.cluster){
    G[[i]] <- 
      graph_from_adjacency_matrix(Adj[nodes$nodes.start[i]:nodes$nodes.end[i],
                                      nodes$nodes.start[i]:nodes$nodes.end[i]])
    furthest_dis[[i]] <- get_diameter(G[[i]])
    u[i] <- furthest_dis[[i]][1] + ifelse(i == 1, 0, nodes$nodes.end[i-1])
    v[i] <- furthest_dis[[i]][length(furthest_dis[[i]])] +
      ifelse(i == 1, 0, nodes$nodes.end[i-1])
  }
  uv.df <- data.frame(u,v)
  return(uv.df)
}


uv.peer.process <- function(A, max.time, n.cluster, Graph, nodes, uv.df, epsilon = 0.1) 
{
  popn = nrow(A)
  outcome = matrix(0, ncol = popn, nrow = (max.time + 1))
  outcome[1, ] = rnorm(popn, 0, 1)
  for (t in 2:(max.time + 1)) {
    nodes.dist <- distances(Graph)
    for (i in 1:popn) {
      for (k in 1:n.cluster) {
        if (i %in% c(nodes$nodes.start[k]:nodes$nodes.end[k])) {
          dist_iu <- nodes.dist[i, uv.df$u[k]]
          dist_iv <- nodes.dist[i, uv.df$v[k]]
          if ((i == uv.df$u[k]) | (i == uv.df$v[k])) {
            outcome[t, i] <- outcome[t - 1, i]
          }
          else if (dist_iu == Inf | dist_iv == Inf) {
            outcome[t, i] <- outcome[t - 1, i]
          }
          else if (dist_iu < dist_iv) {
            outcome[t, i] <- outcome[t - 1, uv.df$u[k]]/dist_iu + 
              rnorm(1, 0, epsilon)
          }
          else if (dist_iv < dist_iu) {
            outcome[t, i] <- outcome[t - 1, uv.df$v[k]]/dist_iv + 
              rnorm(1, 0, epsilon)
          }
          else {
            outcome[t, i] <- (outcome[t - 1, uv.df$u[k]] + 
                                outcome[t - 1, uv.df$v[k]])/(2 * dist_iu) + 
              rnorm(1, 0, epsilon)
          }
        }
      }
    }
  }
  outcomes = list()
  for (t in 1:(max.time + 1)) {
    outcomes[[t]] = as.numeric(outcome[t, ])
  }
  names(outcomes) = as.character(paste("time", c(0:max.time), sep = ""))
  return(outcomes)
}


uv.per.simulation <- function(max.time = 1, n.cluster, miss.prob = 0.8, Graph.list, 
                              Nodes.list, epsilon = 0.1) 
{
  nodes <- Nodes.list[[n.cluster]]
  Adj <- as.matrix(as_adjacency_matrix(Graph.list[[n.cluster]]))
  new.A <- new.adjacency(Adj = Adj, miss.prob = miss.prob)
  new.G <- graph_from_adjacency_matrix(new.A, mode = "undirected")
  uv.df <- get.uv(n.cluster, nodes, new.A)
  outcomes <- 
    uv.peer.process(A = new.A, max.time = max.time, 
                    n.cluster = n.cluster, Graph = new.G, nodes = nodes, 
                    uv.df = uv.df, epsilon = epsilon)
  outcomes$MoranI <- list()
  for(i in 1:(max.time + 1)){
    outcomes$MoranI[[i]] <- Moran.I(outcomes[[i]],new.A)
  }
  names(outcomes$MoranI) = as.character(paste("M.",c(0:max.time), sep = ""))
  
  covariates <- 
    uv.peer.process(A = new.A, max.time = max.time, 
                    n.cluster = n.cluster, Graph = new.G, nodes = nodes, 
                    uv.df = uv.df, epsilon = epsilon)
  covariates$MoranI <- list()
  for(i in 1:(max.time + 1)){
    covariates$MoranI[[i]] <- Moran.I(covariates[[i]],new.A)
  }
  names(covariates$MoranI) = as.character(paste("M.",c(0:max.time), sep = ""))
  
  return(res = list(outcomes = outcomes, covariates = covariates, 
                    adj = new.A, graph = new.G, uv = uv.df))
}


covariate <- list()
outcome <- list()
cov.MoranI <- list()
out.MoranI <- list()
new.graph.list <- list() # save all graph 
new.adj.list <- list() # save all adjacent matrix
uv.list <- list()

for(j in 1:n.max.cluster){
  covariate[[j]] <- array(1, c(n.sim, total.n[j], (max.time+1)))
  outcome[[j]] <-  array(1, c(n.sim, total.n[j], (max.time+1)))
  
  cov.MoranI[[j]] <- matrix(1, nrow = n.sim, ncol = (max.time+1))
  out.MoranI[[j]] <- matrix(1, nrow = n.sim, ncol = (max.time+1))
  new.graph.list[[j]]<- list()
  new.adj.list[[j]] <- list()
  uv.list[[j]] <- list()
}


for(j in 1:n.max.cluster){
  for(i in 1:n.sim){
    per.sim <-uv.per.simulation(max.time = 1, n.cluster = j, miss.prob = miss.prob,
                                sum.graphs, nodes.list, epsilon = 0.1 )
    for(t in 1:(max.time+1)){
      covariate[[j]][i,,t] <- per.sim$covariates[[t]]
      outcome[[j]][i,,t] <- per.sim$outcomes[[t]]
      cov.MoranI[[j]][i,t] <-  per.sim$covariates$MoranI[[t]]$p.value
      out.MoranI[[j]][i,t] <- per.sim$outcomes$MoranI[[t]]$p.value
    }
    new.graph.list[[j]][[i]] <- per.sim$graph
    new.adj.list[[j]][[i]] <-  per.sim$adj
    uv.list[[j]][[i]] <- per.sim$uv
  }
}
