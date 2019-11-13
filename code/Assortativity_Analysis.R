
A <- make_ring(5, directed = FALSE, mutual = FALSE, circular = TRUE)
plot(A)

assortativity_degree(A)

g <- erdos.renyi.game(10, 1/2,directed = F)
plot(g, vertex.size=10, vertex.label=NA)
assortativity.degree(g)

g <- make_full_graph(5)

plot(g)

assortativity.degree(g,directed = T)

######## STAR NETWORK WITH 10 Nodes #############
g <- make_star(10,mode = c('in'))
degree(g,mode = c("out"))
plot(g, vertex.size=10)
assortativity.degree(g,directed = T)

#####################################################

######## Ring Graph ############################

ring <- graph.ring(10, directed = FALSE, mutual = FALSE, circular=TRUE)

plot(ring,vertex.size=10)

assortativity.degree(ring,directed = F)

######## Complete Graph ##########################

CompleteGraph <- function(n) {
  myEdges <- combn(1:n,2)
  myGraph <- graph(myEdges, directed=FALSE)
  return(myGraph)
}

myGraph <- CompleteGraph(10)

plot(myGraph,vertex.size=10,vertex.label=NA)

assortativity.degree(myGraph)

###############################################
g = random.graph.game(1000,0.9,type = c("gnp"))
plot(g,vertex.size=10,vertex.label=NA)

g <- erdos.renyi.game(1000, 0.9, type = "gnp")
assortativity.degree(g)

g <- erdos.renyi.game(1000, 0.5, type = "gnp")
assortativity.degree(g)


g <- erdos.renyi.game(1000, 0.1, type = "gnp")
assortativity.degree(g)
