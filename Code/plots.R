library(dplyr)
library(igraph)
library(RColorBrewer)
library(ggplot2)



################################
######correlation coefficient###
################################

hist(cor.coef[,1,(max.time+1)],main = "Correlation coefficients (one cluster)",
     xlab = "Correlation coefficient",col="lightblue",breaks = seq(-1,1,0.1),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)
hist(cor.coef[,5,(max.time+1)],main = "Correlation coefficients (five clusters)",
     xlab = "Correlation coefficient",col="lightblue",breaks = seq(-1,1,0.1),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)
hist(cor.coef[,10,(max.time+1)],main = "Correlation coefficients (ten clusters)",
     xlab = "Correlation coefficient",col="lightblue",breaks = seq(-1,1,0.1),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)

################################
########Distance correlation####
################################

hist(dis.coer[,1,(max.time+1)],main = "Distance correlations (one cluster)",
     xlab = "Distance correlation",col="lightblue",breaks = seq(0,1,0.05),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)
hist(dis.coer[,5,(max.time+1)],main = "Distance correlations (five clusters)",
     xlab = "Distance correlation",col="lightblue",breaks = seq(0,1,0.05),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)
hist(dis.coer[,10,(max.time+1)],main = "Distance correlations  (ten clusters)",
     xlab = "Distance correlation",col="lightblue",breaks = seq(0,1,0.05),
     cex.lab = 1.3, cex.axis = 1.5, cex.main = 1.3, cex.sub = 1.5)


################################
############zbeta###############
################################

##############direct############
hist(z.beta[,1,(max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (one cluster)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-70,70,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)
hist(z.beta[, 5, (max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (five clusters)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-70,70,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)
hist(z.beta[, 10, (max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (ten clusters)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-70,70,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)

###########uv####################
hist(z.beta[,1,(max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (one cluster)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-150,150,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)
hist(z.beta[,5,(max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (five clusters)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-150,150,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)
hist(z.beta[,10,(max.time + 1)],
     main = expression(bold(paste("Standardized ", beta, " coefficients (ten clusters)"))),
     xlab = expression(paste("Standardized ", beta, " coefficients")),
     col = "lightblue", breaks = seq(-150,150,length.out = 23),
     cex.lab = 1.3, cex.axis = 1.45, cex.main = 1.3, cex.sub = 1.5)


################################
##########Network graphs########
################################


plot.graph <- function(graph.list, outcome.list,
                       n.cluster, uv.flag = TRUE,
                       uv.list = NULL,
                       nodes.list = nodes.list){
  ## uv.flag means plot with uv, or without uv
  outcome <- outcome.list[[n.cluster]][1,,1] # outcome vector
  graph <- graph.list[[n.cluster]][[1]] # use first simulation
  nodes <- nodes.list[[n.cluster]]
  if(uv.flag ==TRUE){
    uv.df <- uv.list[[n.cluster]][[1]] # use first simulation
  }
  popn <- length(outcome)
  if( uv.flag == TRUE){
    #V(graph)$color =  "darkslategray1"
    V(graph)$color = "#FFB90F"
    V(graph)[c(uv.df$u,uv.df$v)]$color = "red"
    V(graph)$size = 3.5
    V(graph)[c(uv.df$u,uv.df$v)]$size = 7
  }else{
    V(graph)$color = "#FFB90F"
    V(graph)$size = 3.5
  }
  plot(graph,layout = layout.fruchterman.reingold(graph), vertex.color = V(graph)$color,
       vertex.label = NA,vertex.size =V(graph)$size,
       cex.main = 1.5 ,cex.sub = 1.5 )
}

####direct transmission process######
par(mar=c(0,0,1,0))
plot.graph(new.graph.list, outcome, n.cluster = 1, uv.flag = FALSE,
           nodes.list = nodes.list)

######### uv ########################
par(mar=c(0,0,1,0))
plot.graph(new.graph.list, outcome, n.cluster = 1, uv.flag = TRUE,
           uv.list = uv.list, nodes.list = nodes.list)

################################################
#########Network graphs with color qunatile#####
################################################

Palette = colorRampPalette(brewer.pal(9, "YlOrRd"))(20) # 20 different colors

plot.colored.graph <- function( graph.list, outcome.list,max.time,
                                n.cluster, uv.flag = TRUE,
                                uv.list = NULL, 
                                nodes.list = nodes.list){
  outcome <- outcome.list[[n.cluster]][1,,max.time+1] # outcome vector
  graph <- graph.list[[n.cluster]][[1]]
  nodes <- nodes.list[[n.cluster]]
  if(uv.flag == TRUE){
    uv.df <- uv.list[[n.cluster]][[1]]
  }
  popn <- length(outcome)
  
  cuts<- quantile(outcome, seq(0, 1, 1/20))
  V(graph)$color = Palette[1]
  for(k in 2:20){
    V(graph)$color = ifelse(outcome > cuts[k-1] &outcome <= cuts[k],
                            Palette[k], V(graph)$color)
  }
  
  if(uv.flag == TRUE){
    V(graph)$size = 3.5
    V(graph)[c(uv.df$u,uv.df$v)]$size = 7
  }else{
    V(graph)$size = 3.5
  }
  
  plot(graph,layout = layout.fruchterman.reingold(graph), vertex.color = V(graph)$color,
       vertex.label = NA, vertex.size =V(graph)$size,
       cex.main = 1.5 ,cex.sub = 1.5 )
  
}
####direct transmission process######
par(mar=c(0,0,1,0))
plot.colored.graph(new.graph.list,outcome, max.time = max.time, n.cluster =  1,
                   uv.flag = FALSE, nodes.list = nodes.list) 
title("One cluster",line = -0.3)

###########uv #######################
par(mar=c(0,0,1,0))
plot.colored.graph(new.graph.list,outcome, max.time = max.time, n.cluster =  1,
                   uv.flag = TRUE, uv.list = uv.list,nodes.list = nodes.list) 
title("One cluster",line = -0.3)

################rejection rates according to times##########
### rejection rates accourding to times
cob.t <- left_join(reject.dcor.t,reject.linear.t, by = c("times","Cluster"))
cob.t$Cluster <- factor(cob.t$Cluster, levels = c("1","5","10"))

gg.dcor <- ggplot( data = cob.t) + 
  geom_line(aes (x = times, y = dcor, col = Cluster))+
  geom_point(aes (x = times, y = dcor, col = Cluster), size = 2)+
  scale_color_discrete(breaks = c("1","5","10"), labels =c("1","5","10"))+
  # scale_shape_discrete(breaks = c("1","5","10"), labels =c("1","5","10"))+
  scale_x_continuous(name = "t", limits = c(0,10), breaks = c(0:10), labels = c("0 (Null)", 1:10)) +
  scale_y_continuous(name = "Rejection rates", limits = c(0,1), breaks = c(0, 0.05, seq(0.2,1,0.2)))+
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16))+
  geom_hline(aes (yintercept = 0.05),linetype= 2, size = 0.4, alpha =0.8, color = '#00b0a6')

gg.linear <- ggplot( data = cob.t) + 
  geom_line(aes (x = times, y = linear, col = Cluster))+
  geom_point(aes (x = times, y = linear, col = Cluster), size = 2)+
  scale_color_discrete(breaks = c("1","5","10"), labels =c("1","5","10"))+
  # scale_shape_discrete(breaks = c("1","5","10"), labels =c("1","5","10"))+
  scale_x_continuous(name = "t", limits = c(0,10), breaks = c(0:10), labels = c("0 (Null)", 1:10)) +
  scale_y_continuous(name = "Rejection rates", limits = c(0,1), breaks = c(0, 0.05, seq(0.2,1,0.2)))+
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16))+
  geom_hline(aes (yintercept = 0.05),linetype= 2, size = 0.4, alpha =0.8, color = '#00b0a6')


par(mar=c(0,0,1,0))
gg.dcor

par(mar=c(0,0,1,0))
gg.linear
