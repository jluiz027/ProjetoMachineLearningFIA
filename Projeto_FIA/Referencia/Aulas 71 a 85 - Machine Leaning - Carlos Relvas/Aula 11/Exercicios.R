## Exercícios 

## KMeans

dados <- read.table("students.txt", sep="\t", header=T)

set.seed(121)
id <- sample(1:nrow(dados), nrow(dados)*0.8)
dados.des <- dados[id,]
dados.test <- dados[-id,]

plot(dados.des[,"LPR"], dados.des[,"PEG"], pch=16, 
     xlab="LPR", ylab="PEG")


agrupamento = kmeans(dados.des[, c("LPR", "PEG")], 
                     4, nstart=100)

table(dados.des$UNS, agrupamento$cluster)

agrupamento = kmeans(dados.des[, c("STG","SCG","STR","LPR", "PEG")], 
                     4, nstart=100)

table(dados.des$UNS, agrupamento$cluster)


## Ward

dados <- read.table("students.txt", sep="\t", header=T)

set.seed(121)
id <- sample(1:nrow(dados), nrow(dados)*0.8)
dados.des <- dados[id,]
dados.test <- dados[-id,]

distancias <- dist(dados.des[, c("STG","SCG","STR","LPR", "PEG")])

fit_hclust <- hclust(distancias, method="ward.D2")

par(mfrow=c(1,1))
plot(fit_hclust)

agrupamento <- cutree(fit_hclust, 3)

table(dados.des$UNS, agrupamento)

agrupamento <- cutree(fit_hclust, 4)

table(dados.des$UNS, agrupamento)



### PCA

df <- read.csv('boston.csv', sep=',')

set.seed(432)
id <- sample(1:nrow(df), nrow(df)*0.7)
df_train <- df[id,]
df_val <- df[-id,]

cor(df_train[,2:14])
heatmap(cor(df_train[,2:14]))

pca <- prcomp(df_train[,2:14],
              center = TRUE,
              scale. = TRUE) 

print(pca)
plot(pca, type = "l")

summary(pca)

df_train_pca <- data.frame(cbind(predict(pca, newdata=df_train[,2:14]),
                      df_train[,15]))

df_val_pca <- data.frame(cbind(predict(pca, newdata=df_val[,2:14]),
                    df_val[,15]))

plot(df_train_pca[,1], df_train_pca[,2])

cor(df_train_pca[,1:13])
heatmap(cor(df_train_pca[,1:13]))

fit <- lm("V14~PC1+PC2", data=df_train_pca)
MSE <- sum((df_val_pca[,14] - 
              predict(fit, newdata = df_val_pca))^2)/nrow(df_val_pca)


