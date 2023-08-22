library(igraph)
library(MASS)
library(dplyr)
library(ape)

load("/Data/first.graphs.RData")
n.sim <- 500
popn <- 500
max.time <- 10
n.max.cluster <- 10
miss.prob <- 0.7


### random  remove 30% edges for a given adjacency matrix
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


#######function for direct transmission
direct.peer.process <- function(A, max.time = 10, n.cluster,
                                nodes, mprob = 0.8, epsilon = 1){
  # A is Adjacency matrix, n.cluster is the number of clusters of this graph,
  # nodes is the nodes information of this graph
  # if n.cluster is 5, nodes should be nodes.list[[5]]
  # mprob is the probability for binary
  # epsilon is the sd of normal random error
  # max.time is the max iteration time, in diract transmission, we use 10
  
  popn = nrow(A)
  # Generate the initial popn's outcomes (at t = 0)
  outcome = matrix(0, ncol = popn, nrow = (max.time+1) )
  outcome[1,] = rnorm(popn, 0,1)  #y^t_{ik} ~N(0,1)
  
  for (t in 2:(max.time + 1)){
    for (i in 1: popn ){
      z = ifelse(sum(A[i,]) == 0, 0, sum(A[i,] * outcome[t-1,])/sum(A[i,]))  + rnorm(1,0,epsilon)
      outcome[t,i] = ifelse(rbinom(1,1,mprob) == 1, z, outcome[t-1,i])
    }
  }
  
  
  outcomes = list()
  for(t in 1:(max.time + 1)){
    outcomes[[t]] = as.numeric(outcome[t,])
  }
  names(outcomes) = as.character(paste("time" ,c(0:max.time), sep = ""))
  return(outcomes)
}

######## per simulation for direct transmission in each scenario (n.cluster) 
#####both for X and Y
direct.per.simulation <- function(max.time = 10,n.cluster,Graph.list, Nodes.list
                                  , mprob = 0.8, epsilon = 1, miss.prob = 0.7){
  # the function for each simulation, 
  # return outcomes from the direct.peer.process and the Moran.I test
  # mprob is the probability for binary distribution in direct transmission proces
  # miss.prob is the probability for missing edges, 0.7 means we have 30% missing edges
  
  nodes <- Nodes.list[[n.cluster]]
  Adj <- as.matrix(as_adjacency_matrix(Graph.list[[n.cluster]]))
  new.A <- new.adjacency(Adj = Adj, miss.prob = miss.prob)
  new.G <- graph_from_adjacency_matrix(new.A, mode = "undirected")
  outcomes <- 
    direct.peer.process(A = new.A, max.time = max.time,n.cluster = n.cluster,
                        nodes = nodes, mprob = mprob, epsilon = epsilon)
  outcomes$MoranI <- list()
  for(i in 1:(max.time + 1)){
    outcomes$MoranI[[i]] <- Moran.I(outcomes[[i]],new.A)
  }
  names(outcomes$MoranI) = as.character(paste("M.",c(0:max.time), sep = ""))
  
  covariates <- 
    direct.peer.process(A = new.A, max.time = max.time,n.cluster = n.cluster,
                        nodes = nodes, mprob = mprob, epsilon = epsilon)
  covariates$MoranI <- list()
  for(i in 1:(max.time + 1)){
    covariates$MoranI[[i]] <- Moran.I(covariates[[i]],new.A)
  }
  names(covariates$MoranI) = as.character(paste("M.",c(0:max.time), sep = ""))
  
  return(res = list(outcomes = outcomes, covariates = covariates, 
                    adj = new.A, graph = new.G))
}


covariate <- list()
outcome <- list()
cov.MoranI <- list()
out.MoranI <- list()
new.graph.list <- list() # save all graph 
new.adj.list <- list() # save all adjacent matrix

for(j in 1:n.max.cluster){
  covariate[[j]] <- array(1, c(n.sim,popn, (max.time+1)))
  outcome[[j]] <-  array(1, c(n.sim,popn, (max.time+1)))
  
  cov.MoranI[[j]] <- matrix(1, nrow = n.sim, ncol = (max.time+1))
  out.MoranI[[j]] <- matrix(1, nrow = n.sim, ncol = (max.time+1))
  new.graph.list[[j]]<- list()
  new.adj.list[[j]] <- list()
}

for(j in 1:n.max.cluster){
  for(i in 1:n.sim){
    per.sim <-direct.per.simulation(max.time = max.time, n.cluster = j, sum.graphs, nodes.list,
                                    mprob = 0.8, epsilon = 0.1, miss.prob = miss.prob)
    for(t in 1:(max.time+1) ){
      covariate[[j]][i,,t] <- per.sim$covariates[[t]]
      outcome[[j]][i,,t] <- per.sim$outcomes[[t]]
      cov.MoranI[[j]][i,t] <-  per.sim$covariates$MoranI[[t]]$p.value
      out.MoranI[[j]][i,t] <- per.sim$outcomes$MoranI[[t]]$p.value
    }
    new.graph.list[[j]][[i]] <- per.sim$graph
    new.adj.list[[j]][[i]] <-  per.sim$adj
  }
}


