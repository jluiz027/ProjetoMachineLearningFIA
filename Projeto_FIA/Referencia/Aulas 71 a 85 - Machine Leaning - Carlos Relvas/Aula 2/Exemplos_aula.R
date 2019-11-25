## Metricas

# Regression 

df <- read.csv('base_gastos_cartao.csv',
               header = T)

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]

formula <- "Gastos_Cartao ~ Idade+Renda+
Impostos+Segmento"

fit <- lm(as.formula(formula), 
          data = df_train)

SQ <- function(obs, prev){
  return(sum((obs-prev)^2))
}

RMSE <- function(obs, prev){
  return(sqrt(SQ(obs, prev)/length(obs)))
}

MAE <- function(obs, prev){
  return(sum(abs(obs-prev))/length(obs))
}

R_square <- function(obs, prev, media){
  return(1 - SQ(obs, prev)/SQ(obs, media))
}

previsto <- predict(fit, newdata=df_test)
RMSE(df_test$Gastos_Cartao, previsto)
MAE(df_test$Gastos_Cartao, mean(df_train$Gastos_Cartao))
R_square(df_test$Gastos_Cartao,
         previsto, 
         mean(df_train$Gastos_Cartao))

# Classification

df <- read.table("train_titanic.csv",
                 stringsAsFactors = F,
                 header=T, sep=",")

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]

for(i in c('Parch','SibSp','Pclass')){
  cat(paste0(i, "  : ", 
             sum(is.na(df_train[,i]))), "\n")
}

formula <- "as.factor(Survived) ~ Parch+
SibSp+Pclass"

fit <- glm(as.formula(formula),
           data = df_train, 
           family="binomial")

prob <- predict(fit, 
                newdata=df_test,
                type='response')
prev <- ifelse(prob > 0.5, 1, 0)

# Confusion Matrix

table(df_test$Survived, prev)

install.packages("ROCR")
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
## Acuracia
perf <- performance(metrics, measure='acc')
plot(perf)

## False positive rate
perf <- performance(metrics, measure='fpr')
plot(perf)

## False negative rate
perf <- performance(metrics, measure='fnr')
plot(perf)

## F 
perf <- performance(metrics, measure='f')
plot(perf)

## ROC
perf <- performance(metrics, measure="tpr",
                    x.measure="fpr")
plot(perf)
abline(a=0,b=1)

## AUC
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))




## Stepwise

## Reading Data
dados_des <- read.csv("adult.data", header = F, 
                      stringsAsFactors = F)
dados_test <- read.csv("adult.test", header = F, 
                       stringsAsFactors = F)
## Defining header
nomes <- c("age", "workclass", "fnlwgt", "education", 
           "education_num", "marital_status", "occupation",
           "relationship", "race", "sex", "capital_gain",
           "capital_loss", "hours_per_week", "native_country",
           "income")
names(dados_des) <- nomes
names(dados_test) <- nomes

## Descriptive analysis
vars_categ <- c("workclass", "education", 
                "marital_status", "occupation",
                "relationship", "race", "sex", "native_country")
vars_cont <-  c("age", "education_num", "capital_gain",
                "capital_loss", "hours_per_week")

## Defining formula with all features
formula <- paste0("as.factor(income) ~ ", paste0(vars_cont, collapse = "+"), "+",
                  paste0("as.factor(", vars_categ, ")", collapse = "+"))

## Fitting the logistic regression
fit <- glm(as.formula(formula), family="binomial", data=dados_des)

## Applying Stepwise
fit_step <- step(fit)

## Evaluate results in test data set
dados_test[,"prob"] <- predict(fit_step, newdata = dados_test)

dados_test <- dados_test[dados_test$education!="",]
dados_test[,"prob"] <- predict(fit_step, newdata = dados_test)

for(i in vars_cont){
  dados_test[,i] <- as.numeric(dados_test[,i])
}

dados_test[,"prob"] <- predict(fit, newdata = dados_test, type="response")

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"], dados_test$income)
## Acuracia
perf <- performance(metrics, measure='acc')
plot(perf)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))










## Lasso

dados_des <- read.csv("adult.data", header = F, 
                      stringsAsFactors = F)
dados_test <- read.csv("adult.test", header = F, 
                       stringsAsFactors = F)
## Defining header
nomes <- c("age", "workclass", "fnlwgt", "education", 
           "education_num", "marital_status", "occupation",
           "relationship", "race", "sex", "capital_gain",
           "capital_loss", "hours_per_week", "native_country",
           "income")
names(dados_des) <- nomes
names(dados_test) <- nomes


install.packages("glmnet", repos="https://cran.rstudio.com/", dependencies = T)
library(glmnet)

set.seed(42)
id_train <- sample(1:nrow(dados_des), nrow(dados_des)*0.7)

dados_des[,"dataset"] <- 0
dados_test[,"dataset"] <- 1
dados_total <- rbind(dados_des, dados_test)
X = model.matrix(as.formula(paste0(formula, "+dataset")), data=dados_total)
X_des = X[X[,"dataset"]==0,]
X_test = X[X[,"dataset"]==1,]
X_train = X_des[id_train,]
X_val = X_des[-id_train,]

nrow(X_train)
nrow(X_val)
nrow(X_test)

fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
              alpha=1, family="binomial")
plot(fit)

aucs <- array()
cont <- 1
for(l in c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1)){
  fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
                alpha=1, family="binomial", lambda = l)
  prob <- predict(fit, X_val, type="response")[,1]
  
  metrics <- prediction(prob,dados_des[-id_train,'income'])
  perf <- performance(metrics, measure="auc")@y.values
  
  aucs[cont] <- perf
  cont <- cont+1
}

aucs
fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
              alpha=1, family="binomial", lambda = 0.001)

## Evaluate results in test data set
dados_test <- na.omit(dados_test)
dados_test[,"prob"] <- predict(fit, newx=X_test, type="response")[,1]

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"],dados_test[,'income'])
performance(metrics, measure="auc")@y.values













## Ridge

dados_des <- read.csv("adult.data", header = F, 
                      stringsAsFactors = F)
dados_test <- read.csv("adult.test", header = F, 
                       stringsAsFactors = F)
## Defining header
nomes <- c("age", "workclass", "fnlwgt", "education", 
           "education_num", "marital_status", "occupation",
           "relationship", "race", "sex", "capital_gain",
           "capital_loss", "hours_per_week", "native_country",
           "income")
names(dados_des) <- nomes
names(dados_test) <- nomes


install.packages("glmnet", repos="https://cran.rstudio.com/", dependencies = T)
library(glmnet)

set.seed(42)
id_train <- sample(1:nrow(dados_des), nrow(dados_des)*0.7)

dados_des[,"dataset"] <- 0
dados_test[,"dataset"] <- 1
dados_total <- rbind(dados_des, dados_test)
X = model.matrix(as.formula(paste0(formula, "+dataset")), data=dados_total)
X_des = X[X[,"dataset"]==0,]
X_test = X[X[,"dataset"]==1,]
X_train = X_des[id_train,]
X_val = X_des[-id_train,]

nrow(X_train)
nrow(X_val)
nrow(X_test)

fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
              alpha=0, family="binomial")
plot(fit, xvar="lambda")


aucs <- array()
cont <- 1
for(l in c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1)){
  fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
                alpha=0, family="binomial", lambda = l)
  prob <- predict(fit, X_val, type="response")[,1]
  
  metrics <- prediction(prob,dados_des[-id_train,'income'])
  perf <- performance(metrics, measure="auc")@y.values
  
  aucs[cont] <- perf
  cont <- cont+1
}

aucs
fit <- glmnet(x = X_train, y=as.factor(dados_des[id_train,'income']),
              alpha=0, family="binomial", lambda = 0.001)

## Evaluate results in test data set
dados_test <- na.omit(dados_test)
dados_test[,"prob"] <- predict(fit, newx=X_test, type="response")[,1]

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"],dados_test[,'income'])
performance(metrics, measure="auc")@y.values






