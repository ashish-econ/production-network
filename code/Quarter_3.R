library(tidyverse)

# Import dataset

Tax_Data = data.frame(trans_Q3_16_17)
# sum(is.na(Tax_Data$tax_on_pur)) # 142661
# sum(is.na(Tax_Data$tax_on_sal)) # 52050

sum(Tax_Data$tax_on_sal,na.rm = T)
Tax_Data$tax_on_sal <- Tax_Data$tax_on_sal/47190626311

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
write.csv(edges,"Production_Network_Q3_edgelist_weighted")


#####################################  Tax Network ############################################################################

library(network)

Tax_Network <- network(edges,vertex.attr = nodes,matrix.type="edgelist",ignore.eval=TRUE)

class(Tax_Network)



detach(package:network)
rm(Tax_Network)
library(igraph)
library(CINNA)

Tax_Network <- graph_from_data_frame(d=edges,vertices=nodes,directed= TRUE)


############################GIANT COMPONENT ANALYSIS#####################################################################################


cl_strong <- clusters(Tax_Network,mode="strong")
which.max(cl_strong)
table(cl_strong$csize)
plot(cluster.distribution(Tax_Network,mode="strong", cumulative = FALSE, mul.size = FALSE),log="xy")


cl_weak <- clusters(Tax_Network,mode="weak")
table(cl_weak$csize)
plot(cluster.distribution(Tax_Network,mode="weak",cumulative = FALSE,mul.size = FALSE),log="xy")



G_3 <-   induced.subgraph(Tax_Network, which(cl_strong$membership == which.max(cl_strong$csize)))
G_3 <- as_long_data_frame(G_3)
G_3 <- select (G_3,-c(from,to))
G_3 <- select(G_3,-c(from_name,to_name))
class(G_3)

G_3 <-   induced.subgraph(Tax_Network, which(cl_strong$membership == which.max(cl_strong$csize)))
G_3 <- as_long_data_frame(G_3)
G_3_original_id <- select(G_3,-c(from,to,from_name,to_name))
write.csv(G_3_original_id,"Giant_Component_edgelist_Weighted_Q3(original_id)")




#################################### Making Edgelist and Nodes of the Giant Component And Assigning Them New Unique ID'S ########################################################

G <- G_3 %>% 
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

edges_GC_Q3 <- data.frame(edges_GC)
class(edges_GC_Q3)

edges_GC_Q3 <- edges_GC_Q3[(edges_GC_Q3$from != edges_GC_Q3$to),]

class(edges_GC_Q3)

edges_GC_Q3 <- data.matrix(edges_GC_Q3)

edges_GC_Q3$weight[is.na(edges_GC_Q3$weight)] <- 0
write.csv(edges_GC_Q3, "Giant_Component_Weighted_Q3")


############################################################# Community Structure Analysis #################################################

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

GC_3 <- select(`Giant_Componentedgelist_Q3(original_id)`,-c(X))

Freq_Dist3 <- data.frame(freqdist(GC_3$from_label))
setDT(Freq_Dist3,keep.rownames = TRUE)
colnames(Freq_Dist3)[1] <- "from_label"
Freq_Dist3 <- select(Freq_Dist3,-c(percentage,cumulativepercentage))
Freq_Dist_2 <- arrange(Freq_Dist3,frequencies)

n<-dim(Freq_Dist_2)[1]
Freq_Dist_2<- Freq_Dist_2[1:(n-1),]

Freq_Dist_Out_2 <- transform(Freq_Dist_2, cumFreq = cumsum(Freq_Dist_2$frequencies))
Freq_Dist_Out_2 <- transform(Freq_Dist_Out_2, relative = Freq_Dist_Out_2$cumFreq/sum(Freq_Dist_2$frequencies))
Freq_Dist_Out_2 <- transform(Freq_Dist_Out_2,CCDF = 1 - Freq_Dist_Out_2$relative)

plot(x= Freq_Dist_Out_2$frequencies,y=Freq_Dist_Out_2$CCDF,log = "xy")

################   PLOTIING CCDF OF OUT DISTRIBUTION OF  BOTH QUARTERS IN SAME FIGURE ##########################################

plot(x=Freq_Dist_Out_1$frequencies,Freq_Dist_Out_1$CCDF,type="p",col="red",log="xy",lwd = 0.5)

lines(x=Freq_Dist_Out_2$frequencies,Freq_Dist_Out_2$CCDF,col="green",log="xy",lwd = 0.5,type="p")


################################################## BACKBONE OF QUARTER 3 ###########################################################################################


GC_3 <- data.frame(`Giant_Component_edgelist_Weighted(Unique_Id_Q3)`)
G_3_original_id <- data.frame(Giant_Component_Q3_edgelist_Wieghted)
G_3_original_id <- select(G_3_original_id,-c(X.1,X))
GC_3 <- data.frame(G_3_original_id)


GC_3 <- select(GC_3, -c(weight))


colnames(GC_3) <- c("Source", "Target")

g <- graph.data.frame(GC_3, directed=T) 

G_3_original_id$weight[is.na(G_3_original_id$weight)] <- 0

GC_3 <- G_3_original_id
GC_3$weight[GC_3$weight == 0] <- 0.00001

E(g)$weight <- GC_3$weight

alpha = 0.00001

bb <- get.backbone(g, alpha = alpha, directed = T)

Degree <- data.frame(table(degree(bb)))


path <- 'F:/Ashoka University/DISSERTATION/Production Networks/Code/Backbone_Q3.jpeg'
jpeg(file = path,width = 8, height =8, units = "in", res = 800, pointsize = 1/800)
set.seed(111)


bb <- graph.data.frame(BB_3)

ggnet2(bb, mode = "fruchtermanreingold", size=0.7, node.color = 'orangered',
       edge.size = 0.5, edge.color = "black",label = FALSE)

dev.off()

plot(GC_2$weight,log="xy")
hist(GC_2$weight)
class(bb)
vcount(bb)

Tax_Data_BB <- as_long_data_frame(bb)
Tax_Data_BB$weight <- as.numeric(Tax_Data_BB$weight)


Tax_Data_BB <- select(Tax_Data_BB, -c(`ver[el[, 1], ]`,`ver2[el[, 2], ]`))



###################################### MAKING ADJACENCY MATRIX OF BACKBONE NETWORK ########################################################################

BB_2 <- data.frame(`Backbone_Weihted_Q2_Unique(ID's.of.GC)`)
BB_2 <- select(BB_2,-c(X))

BB_2_graph <- graph.data.frame(BB_2,directed = TRUE)
BB_2_ADJ <- as_adjacency_matrix(BB_2_graph)
class(BB_2_ADJ)
BB_2_ADJM <- as.matrix(BB_2_ADJ)


####################################  SECOND-ORDER NEIGHBOURHOOD CONNECTIVITY DISTRIBUTION #####################################################


BB_3 <- data.frame(BACKBONE_Q3_edgelist_Wieghted)
BB_3 <- select(BB_3,-c(X.1,X))

BB_3_graph <- graph.data.frame(BB_3,directed = TRUE)
BB_3_ADJ <- as_adjacency_matrix(BB_3_graph)
class(BB_3_ADJ)
BB_3_ADJM <- as.matrix(BB_3_ADJ)

My_data_3 <- data.frame(Second_Order_Neighbourhood(BB_3_ADJM))
# My_data_3 <- select(My_data_3,-c(source))

ggplot(My_data_3, aes(x =Second_Order_Neighbourhood.BB_3_ADJM.)) + geom_histogram() #+ scale_x_log10()   

hist(log(Second_Order_Neighbourhood(BB_3_ADJM)),type="p",col="red",lwd = 0.5)


############################################### DEGREE DISTRIBUTIONS #############################################################################3


############################################### 1> In- Degree of the Entire Tax Network #####################################################
vcount(Tax_Network)
Indegree_Q3 <- degree(Tax_Network, mode = c("in"),loops = FALSE, normalized = FALSE)
Indegree_Q3.histogram <- as.data.frame(table(Indegree_Q3))
Indegree_Q3.histogram[,1] <- as.numeric(Indegree_Q3.histogram[,1])
Indegree_Q3.histogram <- arrange(Indegree_Q3.histogram,Indegree_Q3.histogram$Indegree_Q3)
CCDF_IN_3 <- transform(Indegree_Q3.histogram, cumFreq = cumsum(Indegree_Q3.histogram$Freq))
CCDF_IN_3 <- transform(CCDF_IN_3, relative = CCDF_IN_3$cumFreq/138677)
CCDF_IN_3 <- transform(CCDF_IN_3,CCDF = 1 - CCDF_IN_3$relative)


ggplot(CCDF_IN_3, aes(x = Indegree_Q3, y = CCDF)) + geom_point(aes(fill= 'Stretched Exp = Yes, 
 A=3.79,b=1.99,c=0.27,R-sq = 0.99')) +
  scale_x_continuous("Indegree",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") + theme_bw()+ geom_line(aes(x= CCDF_IN_3$Indegree_Q3, 3.786*exp(-1.999*(CCDF_IN_3$Indegree_Q3)**0.2734)),colour="red",lwd = 1)



#################################  OUT-DEGREE DISTRIBUTION ENTIRE TAX NETWORK #################################################

Outdegree_Q3 <- degree(Tax_Network, mode = c("out"),loops = FALSE, normalized = FALSE)
Outdegree_Q3.histogram <- as.data.frame(table(Outdegree_Q3))
Outdegree_Q3.histogram[,1] <- as.numeric(Outdegree_Q3.histogram[,1])
Outdegree_Q3.histogram <- arrange(Outdegree_Q3.histogram,Outdegree_Q3.histogram$Outdegree_Q3)
CCDF_Out_3 <- transform(Outdegree_Q3.histogram,cumFreq = cumsum(Outdegree_Q3.histogram$Freq))
CCDF_Out_3 <- transform(CCDF_Out_3, relative = CCDF_Out_3$cumFreq/138677)
CCDF_Out_3 <- transform(CCDF_Out_3,CCDF = 1 - CCDF_Out_3$relative)


ggplot(CCDF_Out_3, aes(x = Outdegree_Q3, y = CCDF)) + geom_point(aes(fill = 'Power Law = Yes, Power= 2.96,R-sq=0.996')) +
  scale_x_continuous("Outdegree_Q3",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw() +  geom_line(aes(x= CCDF_Out_3$Outdegree_Q3,7.785*(CCDF_Out_3$Outdegree_Q3)**-1.96),colour="red",lwd = 1 )
#####################################  IN-DEGREE GIANT COMPONENT-Q3 ##########################################################


GC_3 <- data.frame(`Giant_Componentedgelist_Q3(original_id)`)
GC_3 <- select(GC_3, -c(X,weight))
GC_3 <- graph.data.frame(GC_3,directed = T)

Indegree_Q3 <- degree(GC_3, mode = c("in"),loops = FALSE, normalized = FALSE)
Indegree_Q3.histogram <- as.data.frame(table(Indegree_Q3))
Indegree_Q3.histogram[,1] <- as.numeric(Indegree_Q3.histogram[,1])
Indegree_Q3.histogram <- arrange(Indegree_Q3.histogram,Indegree_Q3.histogram$Indegree_Q3)
CCDF_IN_GC_3 <- transform(Indegree_Q3.histogram, cumFreq = cumsum(Indegree_Q3.histogram$Freq))
CCDF_IN_GC_3 <- transform(CCDF_IN_GC_3,relative = CCDF_IN_GC_3$cumFreq/41245)
CCDF_IN_GC_3 <- transform(CCDF_IN_GC_3,CCDF = 1- CCDF_IN_GC_3$relative) 

ggplot(CCDF_IN_GC_3, aes(x = Indegree_Q3, y = CCDF)) + geom_point(aes(fill= 'Stretched Exp = Yes,
A=2.19,b=1.19,c=0.369,R-sq=0.99')) +
  scale_x_continuous("Indegree_GC_Q3",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw() + geom_line(aes(x= CCDF_IN_GC_3$Indegree_Q3, 8.057 *(CCDF_IN_GC_3$Indegree_Q3)**-1.677),colour="red",lwd = 1 )

######################################### OUTDEGREE GIANT COMPONENT Q_3 #######################################################

Outdegree_Q3 <- degree(GC_3, mode = c("out"),loops = FALSE, normalized = FALSE)
Outdegree_Q3.histogram <- as.data.frame(table(Outdegree_Q3))
Outdegree_Q3.histogram[,1] <- as.numeric(Outdegree_Q3.histogram[,1])
Outdegree_Q3.histogram <- arrange(Outdegree_Q3.histogram,Outdegree_Q3.histogram$Outdegree_Q3)
CCDF_Out_GC_3 <- transform(Outdegree_Q3.histogram,cumFreq = cumsum(Outdegree_Q3.histogram$Freq))
CCDF_Out_GC_3 <- transform(CCDF_Out_GC_3, relative = CCDF_Out_GC_3$cumFreq/41245)
CCDF_Out_GC_3 <- transform(CCDF_Out_GC_3,CCDF = 1- CCDF_Out_GC_3$relative)


ggplot(CCDF_Out_GC_3, aes(x = Outdegree_Q3, y = CCDF)) + geom_point(aes(fill="Power-law:Yes
Power = 2.85,R-sq =0.99")) +
  scale_x_continuous("Outdegree_GC_Q3",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw()+geom_line(aes(x=CCDF_Out_GC_3$Outdegree_Q3,10.54*(CCDF_Out_GC_3$Outdegree_Q3)**-2.045),colour = "red",lwd=0.7)

#########################################################################################################################################################3
########################################### INDEGREE BACKBONE Q_3 ############################################################################################3

BB_3 <- data.frame(`Backbone_Weihted_Q3_Unique(ID's.of.GC)`)
BB_3 <- select(BB_3,-c(X,weight))
BB_3 <- graph.data.frame(BB_3,directed = T)

Indegree_Q3 <- degree(BB_3, mode = c("in"),loops = FALSE, normalized = FALSE)
Indegree_Q3.histogram <- as.data.frame(table(Indegree_Q3))
Indegree_Q3.histogram[,1] <- as.numeric(Indegree_Q3.histogram[,1])
Indegree_Q3.histogram <- arrange(Indegree_Q3.histogram,Indegree_Q3.histogram$Indegree_Q3)
CCDF_IN_BB_3 <- transform(Indegree_Q3.histogram, cumFreq = cumsum(Indegree_Q3.histogram$Freq))
CCDF_IN_BB_3 <- transform(CCDF_IN_BB_3,relative = CCDF_IN_BB_3$cumFreq/5981)
CCDF_IN_BB_3 <- transform(CCDF_IN_BB_3,CCDF = 1- CCDF_IN_BB_3$relative) 


ggplot(CCDF_IN_BB_3, aes(x = Indegree_Q3, y = CCDF)) + geom_point(aes(fill = 'Power= 2.36,R-sq = 0.998')) +
  scale_x_continuous("Indegree_BB_Q3",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw() + geom_line(aes(x=CCDF_IN_BB_3$Indegree_Q3,0.3349*(CCDF_IN_BB_3$Indegree_Q3)**-1.623),colour = "red",lwd=1)
##########################################################################################################################################################################
########################################################## OUTDEGREE BACKBONE Q_3 ####################################################################################

Outdegree_Q3 <- degree(BB_3, mode = c("out"),loops = FALSE, normalized = FALSE)
Outdegree_Q3.histogram <- as.data.frame(table(Outdegree_Q3))
Outdegree_Q3.histogram[,1] <- as.numeric(Outdegree_Q3.histogram[,1])
Outdegree_Q3.histogram <- arrange(Outdegree_Q3.histogram,Outdegree_Q3.histogram$Outdegree_Q3)
CCDF_Out_BB_3 <- transform(Outdegree_Q3.histogram,cumFreq = cumsum(Outdegree_Q3.histogram$Freq))
CCDF_Out_BB_3 <- transform(CCDF_Out_BB_3, relative = CCDF_Out_BB_3$cumFreq/5981)
CCDF_Out_BB_3 <- transform(CCDF_Out_BB_3,CCDF = 1- CCDF_Out_BB_3$relative)

ggplot(CCDF_Out_BB_3, aes(x = Outdegree_Q3, y = CCDF)) + geom_point(aes(fill = 'Power law,
Power= 3.5,R-sq=0.99')) +
  scale_x_continuous("Outdegree_BB_Q3",trans = "log10") +
  scale_y_continuous("CCDF",trans = "log10") +
  ggtitle("CCDF Distribution (log-log)") +
  theme_bw() +  geom_line(aes(x=CCDF_Out_BB_3$Outdegree_Q3,0.4686*(CCDF_Out_BB_2$Outdegree_Q2)**-2.304),colour = "red",lwd=1)


#########################################################################################################################################################################################
############################################# DISASSORTATIVITY ANALYSIS Quarter 3##################################################################################################################

### ENTIRE TAX NETWORK ###################
PN_3 <- data.frame(Production_Network_Q3_edgelist_Wieghted)
PN_3 <- select(PN_3,-c(X,X.1))
PN_3 <- graph.data.frame(PN_3,directed = T)
PN_3 <- assortativity_degree(PN_3,directed = T) ## -0.063




### GIANT COMPONENT QUARTER 3 #############
GC_3 <- data.frame(Giant_Component_Q3_edgelist_Wieghted)
GC_3 <- select(GC_3, -c(X,X.1))
GC_3 <- GC_3[,c(2,3,1)]
GC_3 <- graph.data.frame(GC_3,directed = T)
A_1 <- assortativity_degree(GC_3,directed = TRUE) ## -0.0392

### BACKBONE QUARTER 3 ###########
BB_3 <- data.frame(BACKBONE_Q3_edgelist_Wieghted)
BB_3 <- select(BB_3,-c(X,X.1))
BB_3 <- graph.data.frame(BB_3,directed = T)
B_1 <- assortativity_degree(BB_Q3,directed = TRUE) ## -0.069

##########################################################################################


# colnames(Giant_Component_Q2_edgelist_Wieghted)[colnames(Giant_Component_Q2_edgelist_Wieghted)=="to_label"] <- "to"

Giant_Component_Q2_edgelist_Wieghted <- Giant_Component_Q2_edgelist_Wieghted[, c(4, 5, 3, 1, 2)]

BB_3 <- data.frame(BACKBONE_Q3_edgelist_Wieghted)
BB_3 <-  select(BB_3,-c(X.1,X))
BB_3 <- BB_3[,c(2,3,1)]
BB_3 <- graph.data.frame(BB_3,directed = T)
vcount(BB_3)
ecount(BB_3)


