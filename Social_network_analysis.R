library(igraph)
library(dplyr)
setwd("/Users/Abhyudit/Desktop/network/")
products <- read.csv("products.csv", header=T, as.is=T)
copurchase <- read.csv("copurchase.csv", header=T, as.is=T)

#ANSWER 1

products <- subset(products,products$group=='Book')
products <- subset(products,products$salesrank<=150000&products$salesrank>=0)
elements <- products$id
copurchase <- filter(copurchase,copurchase$Source %in% elements & copurchase$Target %in% elements)
View(copurchase)

#ANSWER 2

in_degree <- count(copurchase, id = copurchase$Target)
in_degree$in_degree <- in_degree$n
in_degree <- subset(in_degree,select=c(id,in_degree))

#ANSWER 3

out_degree <- count(copurchase, id = copurchase$Source)
out_degree$out_degree <- out_degree$n
out_degree <- subset(out_degree,select=c(id,out_degree))

#ANSWER 4

in_and_out_degrees <- merge(in_degree,out_degree,by="id")
in_and_out_degrees$total_degree <- in_and_out_degrees$in_degree+in_and_out_degrees$out_degree
View(in_and_out_degrees)
unique_id <- in_and_out_degrees$id[which.max(in_and_out_degrees$total_degree)]
highest_degree <- in_and_out_degrees$total_degree[which.max(in_and_out_degrees$total_degree)]
unique_id #Vertex with higest total degree
highest_degree #Value of highest total degree 
g1<-graph.data.frame(copurchase,directed = TRUE)
subcomponents <- subcomponent(g1, "33", mode="all")
subcomponents

#ANSWER 5
g2<-subgraph(g1,subcomponents)
V(g2)$degree<- degree(g2)
plot(g2,
     vertex.size=V(g2)$degree*0.4,
     edge.arrow.size=0.05,
     edge.color="black",
     vertex.label=NA,
     vertex.label.cex=0.4,
     vertex.color=rainbow(5),
     layout=layout.kamada.kawai)
diameter(g2,direct=T, weights=NA)
diameter_of_vertices <- get_diameter(g2, directed=T)

#ANSWER 6

deg_graph_all <- degree(g2, mode='all')
deg_graph_in <- degree(g2, mode='in')
deg_graph_out <- degree(g2, mode='out')
deg <- cbind.data.frame(deg_graph_all,deg_graph_in,deg_graph_out)
View(deg)
edge_density(g2, loops=F)
ecount(g2)/(vcount(g2)*(vcount(g2)-1))
reciprocity(g2)
closeness(g2, mode='all', weights=NA)
betweenness(g2, directed='T', weights=NA)
hub_score(g2, scale = TRUE, weights = NULL, options = arpack_defaults)
authority_score(g2, scale = TRUE, weights = NULL, options = arpack_defaults)

#ANSWER 7

vertex_id <- c()
nghb_mn_rating<- c()
nghb_mn_review_cnt<- c()
nghb_mn_salesrank <-c()
total_vertex <- unique(c(copurchase$Target,copurchase$Source))
for(i in 1:length(products$id)){
  a <- filter(copurchase,Target==total_vertex[i])
  unique_neighbors <- unique(a$Source)
  index_of_unique_neighbords <- which(unique_neighbors==products$id)
  vertex_id[i] <- products$id[i]
  for(j in 1:length(index_of_unique_neighbords)){
  nghb_mn_rating[i] <- mean(products$rating[index_of_unique_neighbords[j]])
  nghb_mn_salesrank[i] <- mean(products$salesrank[index_of_unique_neighbords[j]])
  nghb_mn_review_cnt[i] <- mean(products$review_cnt[index_of_unique_neighbords[j]])
}
}

avg_for_neighbors <- cbind(vertex_id,nghb_mn_rating,nghb_mn_salesrank,nghb_mn_review_cnt)
View(avg_for_neighbors)

#ANSWER 8
new_products <- merge(products, in_and_out_degrees, by = "id", all.x = TRUE)

summary(m1 <- glm(formula = new_products$salesrank ~ new_products$in_degree + new_products$out_degree + new_products$total_degree +
                    new_products$review_cnt + new_products$downloads + new_products$rating, family="poisson", data=products))

exp(coef(m1))
