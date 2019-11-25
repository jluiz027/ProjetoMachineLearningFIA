load('/Users/carlos/Downloads/Aula17/titanic.raw.rdata')

library(arules)
# Regras de associação gerais
rules <- apriori(titanic.raw)
inspect(rules)


# Regras de associação somente com survived
rules <- apriori(titanic.raw,
                 parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                   default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)




### Jester Dataset

library(recommenderlab)
library(reshape2)
library(ggplot2)
# carregando arquivo de treino
df_train<-read.table("/Users/carlos/Downloads/Aula17/jester_ratings.dat",
                   header=F)
names(df_train) <- c('UserId', 'ItemId', 'Rating')
head(df_train)


## Montando matriz de notas
ratings<-acast(df_train, UserId ~ ItemId)

R<-as.matrix(ratings)

# Converetendo R para uma estrutura do tipo realRatingMatrix 
# realRatingMatrix é uma estrutura matrix sparsa do recommenderlab 
r <- as(R, "realRatingMatrix")
r

# Normalizando as notas
r_m <- normalize(r)
r_m

## Estimando a user based e um item based
## Não rode o primeiro
rec=Recommender(r[1:nrow(r)],method="IBCF",
                param=list(normalize = "Z-score",method="Jaccard"))

print(rec)
names(getModel(rec))

## TopN recomendações
recom <- predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom
as(recom, "list")


### ALS

rec=Recommender(r[1:nrow(r)],method="ALS", param=list(normalize = "Z-score"))

as(recom, "list")





