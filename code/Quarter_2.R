############################################### LIBRARIES ###################################################################################################################3
source("Lib.R")
library(XLConnect)
library(GGally)
library(tidyverse)
library(visNetwork)
library(magrittr)
library(DiagrammeR)
library(data.table)
library(plotly)
library(intergraph)
library(networkD3)
library(optrees)
library(disparityfilter)
library(network)
library(Matrix)
library(igraph)
library(CINNA)
library(ggplot2)
library(poweRlaw)
library(devtools)
library(ForceAtlas2)
library(remotes)




setwd("/Ashoka University/DISSERTATION/Production Networks/DATA/New_Weighted Edgelists/")
.#############################################################################################################################################################################
# Import dataset

# sum(is.na(trans_Q2_16_17$tax_on_pur)) ## Since tax on sale has less missing values we will use that as our weight
#sum(is.na(trans_Q2_16_17$tax_on_sal))

Tax_Data = data.frame(trans_Q2_16_17)


Tax_Data$tax_on_sal <- Tax_Data$tax_on_sal/46246370142 ## Only do when doing the Backbone Calculation


########################### Making Edgelist and Nodes of the Original Tax Network ######################################

sources <- Tax_Data %>%
  distinct(purchaser_id) %>%
  rename (label = purchaser_id)
destinations <- Tax_Data %>%
  distinct(seller_id) %>%
  rename (label = seller_id)

nodes <- full_join(sources,destinations,by="label")
nodes <- nodes %>% rowid_to_column("id")

Tax_Network <- Tax_Data %>%
  group_by(purchaser_id,seller_id) %>%
  summarise(weight = tax_on_sal) %>%
  #summarise(weight = 1) %>%
  ungroup()

edges <- Tax_Network %>%
  left_join(nodes,by=c("purchaser_id" = "label")) %>%
  rename(from = id)

edges <- edges %>%
  left_join(nodes,by=c("seller_id" = "label")) %>%
  rename(to = id)

edges <- select(edges,from,to,weight)

write.csv(edges,"Production_Network_Q2_edgelist_weighted")

#####################################  Tax Network ############################################################################
library(network)

Tax_Network_1 <- network(edges,vertex.attr = nodes,matrix.type="edgelist",ignore.eval=TRUE,directed = T)

class(Tax_Network)

detach(package:network)
rm(Tax_Network)


Tax_Network <- graph_from_data_frame(d=edges,vertices=nodes,directed= TRUE)


#########################################GIANT COMPONENT ANALYSIS#####################################################################################
Production_Network_Q2_edgelist_Wieghted <- select(Production_Network_Q2_edgelist_Wieghted,-c(X.1,X))
Tax_Network <- graph_from_data_frame(Production_Network_Q2_edgelist_Wieghted)

sum(is.na(Production_Network_Q2_edgelist_Wieghted$to))

cl_strong <- clusters(Tax_Network,mode="weak")
which.max(cl_strong$no)
table(cl_strong$csize)
plot(cluster.distribution(Tax_Network,mode="strong", cumulative = FALSE, mul.size = FALSE),log="xy")



cl_weak <- clusters(Tax_Network,mode="weak")
table(cl_weak$csize)
plot(cluster.distribution(Tax_Network,mode="weak",cumulative = FALSE,mul.size = FALSE),log="xy")


  
G <-   induced.subgraph(Tax_Network, which(cl_strong$membership == which.max(cl_strong$csize)))
G <- as_long_data_frame(G)
G_2 <- select (G,-c(from,to))
G_2 <- G_2 %>% rename(from = `ver[el[, 1], ]`, to = `ver2[el[, 2], ]`)
G_2 <- select(G_2,-c(from_name,to_name))

write.csv(G_2, "Giant_Component_edgelist_weighted_Q2(original_id)")
################################### To get edge-list of original Unique_Id's ############################################

G <-   induced.subgraph(Tax_Network, which(cl_weak$membership == which.max(cl_weak$csize)))
ecount(G)
G <- as_long_data_frame(G)
G_2_original_id <- select(G,-c(from,to,from_name,to_name))

write.csv(G_2_original_id,"Giant_Component_edgelist(original_id)")
write.csv(G_2_original_id,"Giant_Component_edgelist_weighted_Q2(original_id)")

##################### BUILDING ADJACENCY MATRIX ###############################################

vars_Q2 <- sort(unique(unlist(G_2[c("from_label","to_label")])))
M_Q2 <- matrix(0,nr=length(vars_Q2),nc=length(vars_Q2),dimnames =list(vars_Q2,vars_Q2))
M_Q2[as.matrix(G_2[c("from_label","to_label")])] <- G_2$weight

#el <- cbind(a= G_2$from_label,b=G_2$to_label)
#mat <- matrix(0,38884,38884)
#mat[el] <- 1

Adj_Mat <- as.matrix(get.adjacency(graph.data.frame(G_2))) 


####### Due to comutational issues could not build adjacency matrix ##############




#################################### Making Edgelist and Nodes of the Giant Component ########################################################

G <- G %>% 
  rename( purchaser_id=from_label, seller_id =to_label )



sources_GC <- G %>%
  distinct(purchaser_id) %>%
  rename (label = purchaser_id)

destinations_GC <- G %>%
  distinct(seller_id) %>%
  rename (label = seller_id)


nodes_GC <- full_join(sources_GC,destinations_GC,by="label")
nodes_GC <- nodes_GC %>% rowid_to_column("id")


GC_Network <- G %>%
  group_by(purchaser_id,seller_id) %>%
  summarise(weight = weight) %>%
  ungroup()

edges_GC <- GC_Network %>%
  left_join(nodes_GC,by=c("purchaser_id" = "label")) %>%
  rename(from = id)

edges_GC <- edges_GC %>%
  left_join(nodes_GC,by=c("seller_id" = "label")) %>%
  rename(to = id)

edges_GC <- select(edges_GC,from,to,weight)

edges_GC_Q2 <- data.frame(edges_GC)
class(edges_GC_Q2)
edges_GC_Q2 <- edges_GC_Q2[(edges_GC_Q2$from != edges_GC_Q2$to),]

edges_GC_Q2$weight[is.na(edges_GC_Q2$weight)] <- 0
#write.csv(edges_GC_Q2, "Giant_Component_Q2_weighted_unique")


################################################  Community Structure Analysis ##############################################################

col.tr <- grDevices::adjustcolor("557799", alpha=0.7)
plot(Tax_Network,edge.arrow.size=0.1,vertex.label=NA, remove.multiple = F, remove.loops = T,vertex.size=1,vertex.color=col.tr	)

install.packages("linkcomm")
library(linkcomm)

head(edges)

edges_1 <- edges[,-3]
edges <- data.frame(edges)
lc <- getLinkCommunities(edges,directed = TRUE,dirweight = 1)
print(lc)

plot(lc, type = "graph", layout = layout.fruchterman.reingold,vlabel= FALSE, shownodesin = 50)
plot(lc,type="graph",layout = "spencer.circle",vlabel = FALSE)

plot(lc, type = "graph", layout = layout.fruchterman.reingold,vlabel= FALSE, shownodesin = 50)

plot(lc, type = "members")

######################### CCDF Out-Degree Distribution #########################################3

#install.packages("freqdist")

library(freqdist)
library(data.table)
library(dplyr)

GC_2 <- select(`Giant_Component_edgelist(Original_Id_Q2)`,-c(X))

Freq_Dist <- data.frame(freqdist(GC_2$from_label))
setDT(Freq_Dist,keep.rownames = TRUE)
colnames(Freq_Dist)[1] <- "from_label"
Freq_Dist <- select(Freq_Dist,-c(percentage,cumulativepercentage))
Freq_Dist_1 <- arrange(Freq_Dist,frequencies)

n <- dim(Freq_Dist_1)[1]
Freq_Dist_1<- Freq_Dist_1[1:(n-1),]

Freq_Dist_Out_1 <- transform(Freq_Dist_1, cumFreq = cumsum(Freq_Dist_1$frequencies))
Freq_Dist_Out_1 <- transform(Freq_Dist_Out_1, relative = Freq_Dist_Out_1$cumFreq/sum(Freq_Dist_1$frequencies))
Freq_Dist_Out_1 <- transform(Freq_Dist_Out_1,CCDF = 1 - Freq_Dist_Out_1$relative)

plot(x= Freq_Dist_Out_1$frequencies,y=Freq_Dist_Out_1$CCDF,log = "xy")


######################### Sir's way of Extracting Giant Component ###############################################################

Tax_Network <- graph_from_data_frame(d=edges,vertices=nodes,directed= TRUE)
nv <- vcount(Tax_Network) # count vertex
ne <- ecount(Tax_Network) # count edges
# Ordering cluster list by cluster siz
Clus <- clusters(Tax_Network)
# largest cluster/component/subgraph
lg <-  induced.subgraph(Tax_Network, which(Clus$membership == which.max(Clus$csize))) 
E_lg <- as.data.frame(get.edgelist(lg))
colnames(E_lg) <- c("Source", "Target")
net_lg <- network(E_lg, directed = T) 
nodes_lg <- network.vertex.names(net_lg)
nodes_GC <- data.frame(nodes_lg)
net = network(edges, directed = T)
nodes <- network.vertex.names(net)


net_lg %v% "actor" =  ifelse(nodes %in% nodes_lg, "giant", 
                          "periphery")

net_lg %v% "color" =  ifelse(net_lg %v% "actor" == "giant", "lightslategray",
                          "black")

path <- 'F:/Ashoka University/DISSERTATION/Production Networks/Code/Giant_Network(Q_2).jpeg'

jpeg(file = path,width = 8, height =8, units = "in", res = 800, pointsize = 1/800)
set.seed(111)




plot(net_lg, mode = "fruchtermanreingold", size=1,  color = 'actor',
       edge.color = c("color", "grey80"),
       palette = c("periphery"="black","giant"="lightslategray","backbone"="red4"),
       legend.size = 20, legend.position = "bottom", size.legend=5)

dev.off()

##################################  BACKBONE FUNCTION ###############################################################

#===================================================================
# backbone function
get.backbone = function(graph, alpha, directed = TRUE)
{
  G = graph
  
  # get edgelist
  edgelist = get.data.frame(G)
  colnames(edgelist) = c("Source","Target","weight")
  
  # get nodes list
  nodes = unique(c(edgelist[,1], edgelist[,2]))
  N = length(nodes)
  
  # initialize backbone dataframe
  backbone = NULL
  
  # cat("Disparity Filter\n")
  # cat("alpha =", alpha, "\n")
  # cat("\nOriginal graph\n")
  # print(G)
  
  for (i in 1:N) # for each node
  {
    # cat('\n i :',i)
    # get neighbors
    nei = edgelist[edgelist$Source == nodes[i],]
    nei = rbind(nei, edgelist[edgelist$Target == nodes[i],])
    
    # get degree for node i
    k_i = length(edgelist$Target[edgelist$Target == nodes[i]]) + 
      length(edgelist$Target[edgelist$Source == nodes[i]])
    # cat('\n k_i :', k_i)
    if (k_i>1)
    {
      for (j in 1:k_i) # for each neighbor
      {
        # cat('\n j :',j)
        # compute weighted edge
        p_ij = as.numeric(nei$weight[j]) / sum(as.numeric(nei$weight))
        
        # VIA INTEGRATION
        integrand_i = function(x){(1-x)^(k_i-2)}
        integration = integrate(integrand_i, lower = 0, upper = p_ij)
        alpha_ij = 1 - (k_i - 1) * integration$value
        
        #alpha_ij = (1 - p_ij)^(k_i - 1)
        # cat('\n alpha_ij : ', alpha_ij)
        
        if(alpha_ij < alpha) # 
        {
          backbone = rbind(backbone, c(nei$Source[j], nei$Target[j], nei$weight[j]))
          # cat('\n backbone : ', backbone)
        } # end inner if
      } # end for loop of j
    } #end outer if
  } # end for loop of i
  
  # cat('\n backbone : ', backbone)
  colnames(backbone) = c("Source","Target","weight")
  backbone = unique(backbone[,c('Source','Target','weight')])
  G_backbone = graph.data.frame(backbone, directed = directed)
  
  # cat("\nBackbone graph\n")
  # print(G_backbone)
  return(G_backbone)
}

GC_2 <- data.frame(G_2_original_id)

# NAN <- data.frame(is.na(GC_2))

GC_2 <- select(GC_2, -c(weight))


colnames(GC_2) <- c("Source", "Target")

g <- graph.data.frame(GC_2, directed=T) 

#G_2_original_id$weight[is.na(G_2_original_id$weight)] <- 0


GC_2$weight[GC_2$weight == 0] <- 0.00001

E(g)$weight <- GC_2$weight

alpha = 0.0001

bb <- get.backbone(g, alpha = alpha, directed = T)



path <- 'F:/Ashoka University/DISSERTATION/Production Networks/Code/Backbone_Q2.jpeg'
jpeg(file = path,width = 8, height =8, units = "in", res = 800, pointsize = 1/800)
set.seed(111)

ggnet2(bb, mode = "fruchtermanreingold", size=1, node.color = "red4",
       edge.size = 0.5, edge.color = "black",label = FALSE)

dev.off()


plot(GC_2$weight)

plot(GC_2$weight,log="xy")
hist(GC_2$weight)
class(bb)
vcount(bb)
ecount(bb)





################################# MAKING EDGE LIST AND NODELIST OF BACKBONE OF GIANT COMPONENT ############################################################################################

Tax_Data_BB <- as_long_data_frame(bb)
Tax_Data_BB$weight <- as.numeric(Tax_Data_BB$weight)


Tax_Data_BB <- select(Tax_Data_BB, -c(`ver[el[, 1], ]`,`ver2[el[, 2], ]`))

write.csv(Tax_Data_BB, "Backbone_Weihted_Q2_Unique(ID's of GC)")
sum(Tax_Data_BB$weight)

#################################  Creating a Sparse Matrix ##################################################################################################################################

G_3 <- as.data.frame(`Giant_Component_edgelist(New_Unique_Id)Q2`)

G_4 <- graph.data.frame(G_3,directed = TRUE)
vcount(G_4)

G_3 <- select(G_3,-c(X,weight))

G_3 <- data.frame(G_3)

adjacency = sparseMatrix(i=as.integer(G_3$from),j=as.integer(G_3$to),x=1,dims = rep(38884,2),use.last.ij = TRUE )
### TOY SPARSE MATRIX ###############################################3

edges = data.frame(i = 1:20, j = sample(1:20, 20, replace = TRUE))



adjacency = sparseMatrix(i = as.integer(edges$i),j = as.integer(edges$j),x = 1,dims = rep(20, 2),use.last.ij = TRUE )
adjacency = data.matrix(adjacency)

###############################################################################################################################################3

###################################### MAKING ADJACENCY MATRIX OF BACKBONE NETWORK ########################################################################

BB_2 <- data.frame(Backbone_Q2_edgelist_Wieghted)
BB_2 <- select(BB_2,-c(X.1,X))

BB_2_graph <- graph.data.frame(BB_2,directed = TRUE)
BB_2_ADJ <- as_adjacency_matrix(BB_2_graph)
class(BB_2_ADJ)
BB_2_ADJM <- as.matrix(BB_2_ADJ)

####################################  SECOND-ORDER NEIGHBOURHOOD CONNECTIVITY DISTRIBUTION #####################################################

Second_Order_Neighbourhood = function(matrix){
  
  Total_Degree <- colSums(matrix)
  list <- c()
  for(i in 1:nrow(matrix)){
    for(j in 1:ncol(matrix)){
      if (matrix[i,j] == 1){
        degree = Total_Degree[j]
        list = c(list,degree)
        
      }
      
      
    }
    
    Sec_Order_Neigh = list
    
    
  }
  
  return(Sec_Order_Neigh)
  
  }

#Second_Order_Neighbourhood <- neighborhood(BB_2_graph,order = 2)
#A <- ego_size(BB_2_graph,order=2)
#A <- data.frame(A)
#Second_Order_Neighbourhood_1 <- data.frame(Second_Order_Neighbourhood)
 

My_data <- data.frame(Second_Order_Neighbourhood(BB_2_ADJM))

hist(log(Second_Order_Neighbourhood(BB_2_ADJM)),type="p",col="red",lwd = 0.5)

ggplot(My_data, aes(x = Second_Order_Neighbourhood.BB_2_ADJM.)) + geom_histogram() + scale_y_log10()   +  scale_x_log10() 

My_table <- data.frame(table(My_data))



######################### COMBINING AND PLOTTING QUARTER_2 AND QUARTER_3 RESULTS FOR SECOND-ORDER NEIGHBOURHOOD CONNECTIVITY DISTRIBUTION ####################################################################################
colnames(My_data)[1] <- "Values"
colnames(My_data_3)[1] <- 'Values'
My_data$source <- 'Quarter_2'
My_data_3$source <- 'Quarter_3'


Both_Quarters_Neigh_Dist <- rbind(My_data,My_data_3)

ggplot(Both_Quarters_Neigh_Dist, aes(Values, fill = source)) + geom_density(alpha = 0.3) 

ggplot(Both_Quarters_Neigh_Dist, aes(Values, fill = source)) + 
  geom_histogram(alpha = 0.4, aes(y = ..density..), position = 'identity') 

###################################################################################################################################################################################################################################################

################################### REWIRING ANALYSIS FOR BOTH QUARTERS ###########################################################################################################################################################################

GC_Q2 <- graph.data.frame(`Giant_Component_edgelist(Original_Id_Q2)`,directed = T)

GC_Q3 <- graph.data.frame(`Giant_Componentedgelist_Q3(original_id)`,directed=T)

igraph::get.edgelist(GC_Q2) 

u1 <- as.undirected(GC_Q3)
u2 <- as.undirected(GC_Q2)


same=length(E(intersection(GC_Q2,GC_Q3)))
reverse=length(E(difference(GC_Q2,GC_Q3)))-length(E(difference(u2,u1)))
addition=length(E(difference(GC_Q2,GC_Q3)))-reverse

Q_2_3_diff <- difference(GC_Q3,GC_Q2,byname = "auto")

Q_2_3_diff <- as_long_data_frame(Q_2_3_diff)

remove(Q_2_3_diff)


GC_2_ADJ <- as_adjacency_matrix(GC_Q2, attr = NULL, edges = TRUE, names = TRUE,sparse = igraph_opt("sparsematrices"))
class(GC_2_ADJ)
GC_2_ADJ <- as_long_data_frame(GC_2_ADJ)

############################################### DEGREE DISTRIBUTIONS #############################################################################3


#### 1> In- Degree of the Entire Tax Network ########
vcount(Tax_Network)
Indegree_Q2 <- degree(Tax_Network, mode = c("in"),loops = FALSE, normalized = FALSE)
Indegree_Q2.histogram <- as.data.frame(table(Indegree_Q2))
Indegree_Q2.histogram[,1] <- as.numeric(Indegree_Q2.histogram[,1])


Indegree_Q2 <- Indegree_Q2[Indegree_Q2>0]
m_pl <- displ$new(Indegree_Q2)
est_pl <- estimate_xmin(m_pl)
Power_In_Degree <- est_pl$pars
fit_power_law(Indegree_Q2)
fit.data <- lines(m_pl, draw = F)

m_pl$setXmin(est_pl)
plot.data <- plot(m_pl, draw = F)
fit.data <- lines(m_pl, draw = F)


# ggplot(Indegree_Q2.histogram, aes(x = Indegree_Q2, y = Freq)) + geom_point() +
#   scale_x_continuous("Degree\n(nodes with this amount of connections)",
#                      breaks = c(1, 3, 10, 30, 100, 300),trans = "log10") +
#   scale_y_continuous("Frequency\n(how many of them)",
#                      breaks = c(1, 3, 10, 30, 100, 300, 1000),
#                      trans = "log10") +
#   ggtitle("Degree Distribution (log-log)") +
#   theme_bw() + 

 ggplot(plot.data) + geom_point(aes(x=log(x), y=log(y),fill = 'Power = 2.96')) + labs(x="log(k)", y="log(CDF)") + theme_bw() + 
  geom_line(data=fit.data, aes(x=log(x), y=log(y)), colour="red",lwd = 1)
### CCDF of the In-Degree of the Entire Tax Network #####
# 
Indegree_Q2.histogram <- arrange(Indegree_Q2.histogram,Indegree_Q2.histogram$Indegree_Q2)
# 
CCDF_IN_2 <- transform(Indegree_Q2.histogram, cumFreq = cumsum(Indegree_Q2.histogram$Freq))
# # Note the sum and subtract the last row from the dataset
# n <- dim(CCDF_IN_2)[1]
# CCDF_IN_2<- CCDF_IN_2[1:(n-1),]
 CCDF_IN_2 <- transform(CCDF_IN_2, relative = CCDF_IN_2$cumFreq/132556)
 CCDF_IN_2 <- transform(CCDF_IN_2,CCDF = 1 - CCDF_IN_2$relative)
# CCDF_IN_2[,1] <- as.numeric(CCDF_IN_2[,1])
# 


ggplot(CCDF_IN_2, aes(x = Node_Q2, y = CCDF)) + geom_point(aes(fill= 'Stretched Exp = Yes, A=4.7,b=2.2,c=0.26')) +
  scale_x_continuous("Indegree",trans = "log10") +
  scale_y_continuous("CCDF",
                     trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw()  + geom_line(aes(x= CCDF_IN_2$Node_Q2, 4.716*exp(-2.207*(CCDF_IN_2$Node_Q2)**0.2582)),colour="red",lwd = 1)


# DRAW IN LIN-LOG SCALE ALSO


 
######################  OUT-DEGREE DISTRIBUTION ENTIRE TAX NETWORK #################################################
 
 Outdegree_Q2 <- degree(Tax_Network, mode = c("out"),loops = FALSE, normalized = FALSE)
 Outdegree_Q2.histogram <- as.data.frame(table(Outdegree_Q2))
 Outdegree_Q2.histogram[,1] <- as.numeric(Outdegree_Q2.histogram[,1])
 Outdegree_Q2.histogram <- arrange(Outdegree_Q2.histogram,Outdegree_Q2.histogram$Outdegree_Q2)
 CCDF_Out_2 <- transform(Outdegree_Q2.histogram,cumFreq = cumsum(Outdegree_Q2.histogram$Freq))
 CCDF_Out_2 <- transform(CCDF_Out_2, relative = CCDF_Out_2$cumFreq/132556)
 CCDF_Out_2 <- transform(CCDF_Out_2,CCDF = 1 - CCDF_Out_2$relative)
 
 ggplot(CCDF_Out_2, aes(x = Outdegree_Q2, y = CCDF)) + geom_point(aes(fill = 'Power Law = Yes, Power= 3.01')) +
   scale_x_continuous("Outdegree_Q2",trans = "log10") +
   scale_y_continuous("CCDF",
                      trans = "log10") +
   ggtitle("CCDF Distribution (log-log)") +
   theme_bw() + geom_line(aes(x= CCDF_Out_2$Outdegree_Q2,10.01*(CCDF_Out_2$Outdegree_Q2)**-2.012),colour="red",lwd = 1 )
 
        
 #####################################  IN-DEGREE GIANT COMPONENT-Q2 ##########################################################
 
 
 GC_2 <- data.frame(`Giant_Component_edgelist(Original_Id_Q2)`)
 GC_2 <- select(GC_2, -c(X,weight))
 GC_2 <- graph.data.frame(GC_2,directed = T)

 Indegree_Q2 <- degree(GC_2, mode = c("in"),loops = FALSE, normalized = FALSE)
 Indegree_Q2.histogram <- as.data.frame(table(Indegree_Q2))
 Indegree_Q2.histogram[,1] <- as.numeric(Indegree_Q2.histogram[,1])
 Indegree_Q2.histogram <- arrange(Indegree_Q2.histogram,Indegree_Q2.histogram$Indegree_Q2)
 CCDF_IN_GC_2 <- transform(Indegree_Q2.histogram, cumFreq = cumsum(Indegree_Q2.histogram$Freq))
 CCDF_IN_GC_2 <- transform(CCDF_IN_GC_2,relative = CCDF_IN_GC_2$cumFreq/38884)
 CCDF_IN_GC_2 <- transform(CCDF_IN_GC_2,CCDF = 1- CCDF_IN_GC_2$relative) 

write.csv(CCDF_IN_GC_2,"CCDF_IN_GC_Q2")
 

ggplot(GC_IN_DEGREE_Q2, aes(x = Indegree_Q2, y = CCDF)) + geom_point() +
  scale_x_continuous("Indegree",trans = "log10") +
  scale_y_continuous("CCDF",
                     trans = "log10") +
  ggtitle("CCDF Distribution (log-log): Power Law") +
  theme_bw()  + geom_line(aes(x=GC_IN_DEGREE_Q2$Indegree_Q2, 
                              7.647*(GC_IN_DEGREE_Q2$Indegree_Q2)**-1.663)
                          ,colour="red",lwd = 1)
 
 ######################################### OUTDEGREE GIANT COMPONENT Q_2 #######################################################
 
 Outdegree_Q2 <- degree(GC_2, mode = c("out"),loops = FALSE, normalized = FALSE)
 Outdegree_Q2.histogram <- as.data.frame(table(Outdegree_Q2))
 Outdegree_Q2.histogram[,1] <- as.numeric(Outdegree_Q2.histogram[,1])
 Outdegree_Q2.histogram <- arrange(Outdegree_Q2.histogram,Outdegree_Q2.histogram$Outdegree_Q2)
 CCDF_Out_GC_2 <- transform(Outdegree_Q2.histogram,cumFreq = cumsum(Outdegree_Q2.histogram$Freq))
 CCDF_Out_GC_2 <- transform(CCDF_Out_GC_2, relative = CCDF_Out_GC_2$cumFreq/38884)
 CCDF_Out_GC_2 <- transform(CCDF_Out_GC_2,CCDF = 1- CCDF_Out_GC_2$relative)
 
 write.csv(CCDF_Out_GC_2,'CCDF_Out_GC_2')
 
 ggplot(GC_Out_DEGREE_Q2, aes(x = Outdegree_Q2, y = CCDF)) + geom_point() +
   scale_x_continuous("Outdegree_GC_Q2",trans = "log10") +
   scale_y_continuous("CCDF",
                      trans = "log10") +
   ggtitle("CCDF Distribution (log-log): Power Law") +
   theme_bw()  + geom_line(aes(x=GC_Out_DEGREE_Q2$Outdegree_Q2, 
                               5.756 *(GC_Out_DEGREE_Q2$Outdegree_Q2)**-1.845)
                           ,colour="red",lwd = 1)
 
 ####################################################################################################################################################3
 #################################################  INDEGREE BACKBONE Q_2 ###################################################################################33
 
 BB_2 <- data.frame(`Backbone_Weihted_Q2_Unique(ID's.of.GC)`)
 BB_2 <- select(BB_2,-c(X,weight))
 BB_2 <- graph.data.frame(BB_2,directed = T)

 Indegree_Q2 <- degree(BB_2, mode = c("in"),loops = FALSE, normalized = FALSE)
 Indegree_Q2.histogram <- as.data.frame(table(Indegree_Q2))
 Indegree_Q2.histogram[,1] <- as.numeric(Indegree_Q2.histogram[,1])
 Indegree_Q2.histogram <- arrange(Indegree_Q2.histogram,Indegree_Q2.histogram$Indegree_Q2)
 CCDF_IN_BB_2 <- transform(Indegree_Q2.histogram, cumFreq = cumsum(Indegree_Q2.histogram$Freq))
 CCDF_IN_BB_2 <- transform(CCDF_IN_BB_2,relative = CCDF_IN_BB_2$cumFreq/5106)
 CCDF_IN_BB_2 <- transform(CCDF_IN_BB_2,CCDF = 1- CCDF_IN_BB_2$relative) 
 
 ggplot(CCDF_IN_BB_2, aes(x = Indegree_Q2, y = CCDF)) + geom_point( aes(fill = 'Power law :Powe= 2.4')) +
   scale_x_continuous("Indegree_BB_Q2",trans = "log10") +
   scale_y_continuous("CCDF",trans = "log10") +
   ggtitle("CCDF Distribution (log-log)") +
   theme_bw() + geom_line(aes(x=CCDF_IN_BB_2$Indegree_Q2,0.3778*(CCDF_IN_BB_2$Indegree_Q2)**-1.625),colour = "red",lwd=1)

 ##########################################################################################################################################################################
 ########################################################## OUTDEGREE BACKBONE Q_2 ####################################################################################
 
 Outdegree_Q2 <- degree(BB_2, mode = c("out"),loops = FALSE, normalized = FALSE)
 Outdegree_Q2.histogram <- as.data.frame(table(Outdegree_Q2))
 Outdegree_Q2.histogram[,1] <- as.numeric(Outdegree_Q2.histogram[,1])
 Outdegree_Q2.histogram <- arrange(Outdegree_Q2.histogram,Outdegree_Q2.histogram$Outdegree_Q2)
 CCDF_Out_BB_2 <- transform(Outdegree_Q2.histogram,cumFreq = cumsum(Outdegree_Q2.histogram$Freq))
 CCDF_Out_BB_2 <- transform(CCDF_Out_BB_2, relative = CCDF_Out_BB_2$cumFreq/5106)
 CCDF_Out_BB_2 <- transform(CCDF_Out_BB_2,CCDF = 1- CCDF_Out_BB_2$relative)
 
 ggplot(CCDF_Out_BB_2, aes(x = Outdegree_Q2, y = CCDF)) + geom_point( aes(fill = 'Power law :Power= 3.17')) +
   scale_x_continuous("Outdegree_BB_Q2",trans = "log10") +
   scale_y_continuous("CCDF",trans = "log10") +
   ggtitle("CCDF Distribution (log-log)") +
   theme_bw() + geom_line(aes(x=CCDF_Out_BB_2$Outdegree_Q2,0.6329*(CCDF_Out_BB_2$Outdegree_Q2)**-2.467),colour = "red",lwd=1)
 
 #########################################################################################################################################################################################
 ############################################# DISASSORTATIVITY ANALYSIS ##################################################################################################################

### ENTIRE TAX NETWORK
Tax_Network <- graph_from_data_frame(d=edges,vertices=nodes,directed= TRUE)
PN_2 <- assortativity_degree(Tax_Network) 

PN_2 <- read.csv("Production_Network_Q3_edgelist_Wieghted.csv")
PN_2 <- data.frame(PN_2)
PN_2 <- select(PN_2,-c(X.1,X,weight))
PN_2 <- graph.data.frame(PN_2)
assortativity_degree(PN_2) ## -0.07

### GIANT COMPONENT QUARTER 2
GC_2 <- data.frame(Giant_Component_Q2_edgelist_Wieghted)
GC_2 <- select(GC_2, -c(X,X.1))
GC_2 <- GC_2[,c(2,3,1)]
GC_2 <- graph.data.frame(GC_2,directed = T)
A <- assortativity_degree(GC_2,directed = TRUE) ## -0.041

### BACKBONE QUARTER 2
BB_2 <- data.frame(Backbone_Q2_edgelist_Wieghted)
BB_2 <- select(BB_2,-c(X.1,X))
BB_2 <- graph.data.frame(BB_2,directed = T)
B <- assortativity_degree(BB_2,directed = TRUE) ## -0.069

##########################################################################################
l <- layout.forceatlas2(BB_2, directed=TRUE, iterations = 100, 
                        linlog = FALSE, pos = NULL, nohubs = FALSE, 
                        k = 400, gravity=1, ks=0.1, ksmax=10, delta = 1,  
                        center=NULL, tolerance = 0.1, dim = 2,
                        plotstep=10, plotlabels= FALSE)

plot(BB_2,layout=l,vertex.size = 0.5, vertex.color = "red")

ggnet2(BB_2, mode = l, size=0.5, node.color = "red4",
       edge.size = 0.5, edge.color = "black",label = FALSE)

BB_2 <- data.frame(Backbone_Q2_edgelist_Wieghted)
BB_2 <- select(BB_2, -c(X))
BB_2 <- graph.data.frame(BB_2,directed = T)

#######################################################################################


#### For Second-Order Neighbourhood Distributions in case of Giant Component ###

GC_Q2 <- fread("Giant_Component_Weighted_Q2_Original(ID's ).csv")



GC_Q2 <- select(GC_Q2,-c(`""`))

GC_Q2  <- graph_from_data_frame(GC_Q2,directed = T)

GC_Q2 <- as_adjacency_matrix(GC_Q2,sparse = T)


My_data <- data.frame(Second_Order_Neighbourhood(GC_Q2))

##################### END OF DO FILE ############################################
