## Association Rules

library(arules)
data(Groceries)

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules)


rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001, conf = 0.8),
                 appearance = list(rhs=c("tropical fruit", "whole milk"),
                                   default="lhs"))
inspect(rules)

rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)







## Recommendation System

music <- read.csv(file="lastfm-matrix-germany.csv")
head(music[,c(1,3:8)])

## ITEM ITEM

# Removendo coluna de usuário
music_itens <- (music[,!(names(music) %in% c("user"))])

# Função que calcula a distância de coseno
cosine_distance <- function(x,y) {
  cosine <- sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(cosine)
}

# Matrix para guardar as distâncias
mat <- matrix(NA, 
                 nrow=ncol(music_itens),
                 ncol=ncol(music_itens),
                 dimnames=list(colnames(music_itens),
                               colnames(music_itens)))
music_itens_sim <- as.data.frame(mat)

for(i in 1:ncol(music_itens)) {
  for(j in 1:ncol(music_itens)) {
    music_itens_sim[i,j]= cosine_distance(music_itens[i],
                                          music_itens[j])
  }
}


# Encontrar os 10 itens mais parecidas de cada item
music_itens_prox <- matrix(NA, 
                           nrow=ncol(music_itens_sim),
                           ncol=11,
                           dimnames=list(colnames(music_itens_sim)))

music_itens_val <- matrix(NA, 
                           nrow=ncol(music_itens_sim),
                           ncol=11,
                           dimnames=list(colnames(music_itens_sim)))

for(i in 1:ncol(music_itens)){
  music_itens_prox[i,] <- (t(
    head(n=11,
         rownames(music_itens_sim[order(music_itens_sim[,i],
                                        decreasing=TRUE),][i]))))
  music_itens_val[i,] <- (t(
    head(n=11,
         music_itens_sim[order(music_itens_sim[,i],
                                        decreasing=TRUE),][i])))
}

# Recomendaçao:

getScore <- function(history, similarities){
  return(sum(history*similarities)/sum(similarities))
}

top_recomendation <- function(user){
  produtos <- colnames(music_itens)
  top_rec <- array()
  for(j in 1:ncol(music_itens)){
    product <- produtos[j]
    
    if(as.integer(music[music$user==user,product]) == 1){ 
      top_rec[j] <- -999
    } else {
      ## 10 produtos mais próximos
      topN<-music_itens_prox[product,2:11]
      topN.similarities <- music_itens_val[product,2:11]
      
      # Checa se o usuário de fato comprou estes 10 itens
      topN.purchases<- music[music$user==user,topN]

      # Calculamos o score
      top_rec[j] <- getScore(topN.purchases,topN.similarities)
    }
  }
  return(produtos[order(top_rec, decreasing = TRUE)])
}

top_recomendation(1)



### Movie Dataset

library(recommenderlab)
library(reshape2)
library(ggplot2)
# carregando arquivo de treino
df_train<-read.csv("/Users/carlos/Downloads/Aula17/train_v2.csv",
                   header=TRUE)
head(df_train)
# removendo ID coluna
df_train<-df_train[,-c(1)]

## Montando matriz de notas
ratings<-acast(df_train, user ~ movie)

R<-as.matrix(ratings)

# Converetendo R para uma estrutura do tipo realRatingMatrix 
# realRatingMatrix é uma estrutura matrix sparsa do recommenderlab 
r <- as(R, "realRatingMatrix")
r

# Normalizando as notas
r_m <- normalize(r)
r_m

image(r_m, main = "Normalized Ratings")

## Estimando a user based e um item based
## Não rode o primeiro
rec1=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine"))
rec2=Recommender(r[1:nrow(r)],method="IBCF", param=list(normalize = "Z-score",method="Jaccard"))

print(rec2)
names(getModel(rec2))

## TopN recomendações
recom <- predict(rec2, r[1:nrow(r)], type="topNList", n=10)
recom
as(recom, "list")



### ALS

rec=Recommender(r[1:nrow(r)],method="ALS", param=list(normalize = "Z-score"))

print(rec)



### Graphs

library(igraph)

## Creating Graphs

g<-graph.empty(n=10, directed=TRUE)
plot(g)

g<-graph.full(n=10, directed = FALSE, loops = FALSE)
plot(g)

g<-graph.star(n=10, mode="out")
plot(g)

g<-graph.star(n=10, mode="in")
plot(g)

g<-graph.ring(n=10)
plot(g)

edges <- c(1,2, 3,2, 2,4)
g<-graph(edges, n=max(edges), directed=TRUE)
plot(g)

df <- cbind(c(1,2,3,1,5),
            c(2,4,5,3,2))
g<-graph_from_data_frame(df, directed = TRUE, vertices = NULL)
plot(g)


##Basics 
edges <- c(1,2, 3,2, 2,4)
g<-graph(edges, n=max(edges), directed=TRUE)
vcount(g)

ecount(g)

neighbors(g, 1, mode = 1)

incident(g,2, mode=c("all", "out", "in", "total"))

is.directed(g)

are.connected(g, 1, 3)

get.edgelist(g)


## Read a txt
advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
g <- graph.data.frame(advice_data_frame)
plot(g)

## Density

install.packages("igraphdata")
data(karate, package="igraphdata")
g <- upgrade_graph(karate)

edge_density(g, loops=F)
ecount(g)/(vcount(g)*(vcount(g)-1))


## transitivity
transitivity(g, type="global")

## diameter
diameter(g)

## Degree
degree(g, mode="all")


hist(degree(g, mode="all"), 
     breaks=1:vcount(g)-1, 
     main="Histogram of node degree")


## Closeness
closeness(g, mode="all", weights=NA)

## betweenness
betweenness(g)


## Hubs and autorithies
hs <- hub_score(g, weights=NA)$vector
as <- authority_score(g, weights=NA)$vector
par(mfrow=c(1,2))
plot(g, vertex.size=hs*50, main="Hubs")
plot(g, vertex.size=as*30, main="Authorities")

## Distance
mean_distance(g)
shortest_paths(g, 'Mr Hi', 'John A')

## Cliques
cliques(g) 

## Community

ceb <- cluster_edge_betweenness(g)
plot(ceb, g)

clp <- cluster_label_prop(g)
plot(clp, g)

## Movies

actors <- read.csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Actors.csv")
movies <- read.csv("https://raw.githubusercontent.com/OPER682-Tucker/Social-Network-Analysis/master/Movies.csv")
actorNetwork <- graph_from_data_frame(d=movies, vertices=actors, directed=F)
plot(actorNetwork)

E(actorNetwork)$color <- ifelse(E(actorNetwork)$Movie == "Forest Gump", "green", 
                                ifelse(E(actorNetwork)$Movie == "Apollo 13", "black",
                                       "orange"))
plot(actorNetwork)

V(actorNetwork)$color <- ifelse(V(actorNetwork)$BestActorActress == "Winner", "gold",
                                ifelse(V(actorNetwork)$BestActorActress == "Nominated","grey",
                                       "lightblue"))

plot(actorNetwork)
degree(actorNetwork, mode="all")
closeness(actorNetwork, mode="all", weights=NA, normalized=T)
betweenness(actorNetwork, directed=F, weights=NA, normalized = T)
distances(actorNetwork, v=V(actorNetwork)["Kevin Bacon"], to=V(actorNetwork), weights=NA)

## StarWars

edges <- read.csv("/Users/carlos/Desktop/Aulas/SNA/star-wars-network-edges.csv")
head(edges)
nodes <- read.csv("/Users/carlos/Desktop/Aulas/SNA/star-wars-network-nodes.csv")
head(nodes)

g <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)
g

V(g)
E(g)
E(g)$weight

par(mar=c(0,0,0,0))
plot(g)


V(g)$label <- ifelse( strength(g)>=10, V(g)$name, NA )
par(mar=c(0,0,0,0))
plot(g)


dark_side <- c("DARTH VADER", "MOTTI", "TARKIN")
light_side <- c("R2-D2", "CHEWBACCA", "C-3PO", "LUKE", "CAMIE", "BIGGS",
                "LEIA", "BERU", "OWEN", "OBI-WAN", "HAN", "DODONNA",
                "GOLD LEADER", "WEDGE", "RED LEADER", "RED TEN", "GOLD FIVE")
other <- c("GREEDO", "JABBA")

V(g)$color <- NA
V(g)$color[V(g)$name %in% dark_side] <- "red"
V(g)$color[V(g)$name %in% light_side] <- "gold"
V(g)$color[V(g)$name %in% other] <- "grey20"
vertex_attr(g)

par(mar=c(0,0,0,0))
plot(g)

par(mar=c(0,0,0,0))
plot(g)
legend(x=.75, y=.75, legend=c("Dark side", "Light side", "Other"), 
       pch=21, pt.bg=c("red", "gold", "grey20"), pt.cex=2, bty="n")


degree(g, mode="all")
closeness(g, mode="all", weights=NA, normalized=T)
betweenness(g, directed=F, weights=NA, normalized = T)
distances(g, v=V(g)["DARTH VADER"], to=V(g), weights=NA)




