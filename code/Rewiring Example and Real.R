GC_Q2 <- data.frame(`Giant_Component_edgelist(Original_Id_Q2)`)

GC_Q2 <- select(GC_Q2,-c(X))

GC_Q2[,2] <- as.character(GC_Q2[,2])
GC_Q2[,3] <- as.character(GC_Q2[,3])


GC_Q2 <- as.matrix(GC_Q2)

G_2=graph.edgelist(GC_Q2[,2:3])

E(G_2)$weight=as.numeric(GC_Q2[,1])

GC_Q3 <- data.frame(`Giant_Componentedgelist_Q3(original_id)`)
GC_Q3 <- select(GC_Q3,-c(X))
GC_Q3[,2] <- as.character(GC_Q3[,2])
GC_Q3[,3] <- as.character(GC_Q3[,3])
GC_Q3 <- as.matrix(GC_Q3)
G_3 <- graph.edgelist(GC_Q3[,2:3])
E(G_3)$weight = as.numeric(GC_Q3[,1])


Diff_Q3_Q2 <- difference(G_3,G_2,byname = "auto")

Diff_Q3_Q2 <- as_long_data_frame(Diff_Q3_Q2)


Diff_Q2_Q3 <- difference(G_2,G_3,byname="auto")

Diff_Q2_Q3 <- as_long_data_frame(Diff_Q2_Q3)















adj_2 = get.adjacency(G_2,attr='weight')

######################################################################

library(tidyverse)
edge_list_1 <- tibble(from = c(1, 2, 2, 3, 4), to = c(2, 3, 4, 2, 1))
node_list_1 <- tibble(id = 1:4)

edge_list_2 <- tibble(from = c(1,2,3,4,5,6),to=c(2,3,4,2,1,5))
node_list_2 <- tibble(id=1:6)


G_1 <- graph.data.frame(edge_list_1,directed = T)
G_2 <- graph.data.frame(edge_list_2,directed = T)

G_diff <- difference(G_2,G_1,byname = 'auto')
G_dif <- as_long_data_frame(G_diff)

G_diff_2 <- difference(G_1,G_2,byname='auto')
G_dif_2 <- as_long_data_frame(G_diff_2)


####### REAL DATASET ######################################

Tax_Data <- data.table(trans_Q2_16_17)

Tax_Data_1 <- select(Tax_Data,c(purchaser_id,seller_id))

Tax_Data_1 <- Tax_Data_1 %>% rename(Source = purchaser_id,Target = seller_id)



Tax_Data_2 <- data.table(trans_Q3_16_17)  

Tax_Data_2 <- select(Tax_Data_2,c(purchaser_id,seller_id))

Tax_Data_2 <- Tax_Data_2 %>% rename(Target = seller_id,Source = purchaser_id)

Tax_Data_2 <- Tax_Data_2[,c(2,1)]



PN_22 <- graph.data.frame(Tax_Data_1,directed = T)
PN_33 <- graph.data.frame(Tax_Data_2,directed = T)


### i.e.Edges that are  In Q_3 but not in Q_2
G_Diff_from_2_to_3 <- difference(PN_33,PN_22,byname = "auto")
G_Diff_from_2_to_3 <- as_long_data_frame(G_Diff_from_2_to_3)

### Edges that are in Q_2 but not in Q_3
G_Diff_from_3_to_2 <- difference(PN_22,PN_33,byname="auto")
G_Diff_from_3_to_2 <- as_long_data_frame(G_Diff_from_3_to_2)

