## Clustering

## K-Means

dados <- read.table("base_gastos_cartao.csv", sep=",", header=T)

set.seed(432)
id <- sample(1:nrow(dados), nrow(dados)*0.7)
dados.des <- dados[id,]
dados.test <- dados[-id,]

help(kmeans)

par(mfrow=c(2,2))
for(i in 1:4){
  set.seed(56)
  teste <- kmeans(dados.des[, c("Gastos_Cartao", "Renda")], 
                  3, nstart=1, iter.max=i)
  plot(dados.des[,"Gastos_Cartao"], dados.des[, "Renda"], 
       col=teste$cluster,
       pch=16, xlab="Gastos_Cartao", ylab="Renda",
       main=paste0("Iter: ", i))
}

grupos <- array()
for(i in 1:100){
  set.seed(i)
  grupos[i] <- kmeans(dados.des[, c("Gastos_Cartao", "Idade",
                                    "Renda", "Impostos")], 
                      3, nstart=1)$tot.with
}
plot(grupos, pch=16, xlab="Iteração", ylab="Soma variância total")


set.seed(31)
agrupamento = kmeans(dados.des[, c("Gastos_Cartao", "Idade",
                                   "Renda", "Impostos")], 
                     3, nstart=100)

table(dados.des$Segmento, agrupamento$cluster)


## WARD

dados <- read.table("base_gastos_cartao.csv", sep=",", header=T)

set.seed(432)
id <- sample(1:nrow(dados), nrow(dados)*0.7)
dados.des <- dados[id,]
dados.test <- dados[-id,]

distancias <- dist(dados.des[, c("Gastos_Cartao", "Idade",
                                 "Renda", "Impostos")])

fit_hclust <- hclust(distancias, method="ward.D2")

par(mfrow=c(1,1))
plot(fit_hclust)

agrupamento <- cutree(fit_hclust, 3)

table(dados.des$Segmento, agrupamento)

agrupamento <- cutree(fit_hclust, 4)

table(dados.des$Segmento, agrupamento)


### PCA

df1 <- read.csv('winequality-white.csv', sep=';')
df1[,'tipo'] = 'white'
df2 <- read.csv('winequality-red.csv', sep=';')
df2[,'tipo'] = 'red'

df <- rbind(df1,df2)

set.seed(432)
id <- sample(1:nrow(df), nrow(df)*0.7)
df_train <- df[id,]
df_val <- df[-id,]

cor(df_train[,1:11])
heatmap(cor(df_train[,1:11]))

pca <- prcomp(df_train[,1:11],
              center = TRUE,
              scale. = TRUE) 

print(pca)
plot(pca, type = "l")

summary(pca)

df_train_pca <- cbind(predict(pca, newdata=df_train[,1:11]),
                      df_train[,12])

df_val_pca <- cbind(predict(pca, newdata=df_val[,1:11]),
                      df_val[,12])

plot(df_train_pca[,1], df_train_pca[,2])

cor(df_train_pca[,1:11])
heatmap(cor(df_train_pca[,1:11]))

library(randomForest)
rf <- randomForest(x = df_train_pca[,1:4], y=df_train_pca[,12],
                  ntree = 200)
MSE <- sum((df_val_pca[,12] - 
             predict(rf, newdata = df_val_pca[,1:4]))^2)/nrow(df_val_pca)

