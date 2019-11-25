## Métricas

df <- read.csv('cancer.data.txt', header = F)

names(df) <- c('Id', 'Clump_Thick','Cell_Size', 'Cell_Shape',
               'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
               'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses', 'Class')

for(i in names(df)){
  cat(paste0(i, "    : ", sum(is.na(df[,i]))), "\n")
}

for(i in names(df)){
  cat(paste0(i, "    : ", sum(df[,i]=='?')), "\n")
}

df[df$Bare_Nuclei=='?','Bare_Nuclei'] = NA

for(i in names(df)){
  df[,i] <- as.numeric(df[,i])
}

df <- na.omit(df)

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]


formula <- paste0("as.factor(Class) ~ ",
                  paste0(names(df)[2:10], 
                         collapse = '+'))

fit <- glm(as.formula(formula), 
           data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test,
                type='response')


library(ROCR)
metrics <- prediction(prob, df_test$Class)
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
perf <- performance(metrics, measure="tpr", x.measure="fpr")
plot(perf)
abline(a=0,b=1)

## AUC
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))



# Logisti Regression

## Reading Data
dados_red <- read.csv("winequality-red.csv", header = T, 
                      stringsAsFactors = F, sep=";")
dados_white <- read.csv("winequality-white.csv", header = T, 
                        stringsAsFactors = F, sep=";")

dados <- rbind(dados_red, dados_white)

dados[,"quality_bin"] <- ifelse(dados[,"quality"]<=6,0,1)

set.seed(42)
id_train <- sample(1:nrow(dados), 0.7*nrow(dados))

dados_des <- dados[id_train,]
dados_test <- dados[-id_train,]


## Descriptive analysis
vars_cont <-  names(dados_des)[1:11]

## Check missing
for(i in names(dados_des)){
  cat(paste0(i, "   : ", sum(is.na(dados_des[,i])), "\n"))
}


## Relation between each continuous feature and the target
for(i in vars_cont){
  boxplot(dados_des[,i]~as.factor(dados_des[,"quality_bin"]),
          main=i)
}


## Defining formula with all features
formula <- paste0("as.factor(quality_bin) ~ ", 
                  paste0(vars_cont, collapse = "+"))

## Fitting the logistic regression
fit <- glm(as.formula(formula), family="binomial", data=dados_des)
## How interpret categorical features?
summary(fit)
names(fit)
names(summary(fit))

## Evaluate results in test data set
dados_test[,"prob"] <- predict(fit, newdata = dados_test, type="response")

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"],dados_test[,'quality_bin'])
performance(metrics, measure="auc")@y.values




fit_step <- step(fit)

dados_test[,"prob"] <- predict(fit_step, newdata = dados_test)
metrics <- prediction(dados_test[,"prob"],dados_test[,'quality_bin'])
performance(metrics, measure="auc")@y.values










## Lasso

## Reading Data
dados_red <- read.csv("winequality-red.csv", header = T, 
                      stringsAsFactors = F, sep=";")
dados_white <- read.csv("winequality-white.csv", header = T, 
                        stringsAsFactors = F, sep=";")

dados <- rbind(dados_red, dados_white)

dados[,"quality_bin"] <- ifelse(dados[,"quality"]<=6,0,1)
## Defining formula with all features
formula <- paste0("as.factor(quality_bin) ~ ", 
                  paste0(vars_cont, collapse = "+"))


library(glmnet)

set.seed(42)
id_train <- sample(1:nrow(dados), nrow(dados)*0.7)

X = model.matrix(as.formula(paste0(formula)), data=dados)
X_des = X[id_train,]
X_test = X[-id_train,]

dados_des <- dados[id_train,]
dados_test <- dados[-id_train,]

set.seed(84)
id_val <- sample(1:nrow(dados_des), nrow(dados_des)*0.3)

X_train = X_des[-id_val,]
X_val = X_des[id_val,]

dados_train <- dados_des[-id_val,]
dados_val <- dados_des[id_val,]

nrow(X_train)
nrow(X_val)
nrow(X_test)

fit <- glmnet(x = X_train, y=as.factor(dados_train$quality_bin),
              alpha=1, family="binomial")
plot(fit, xvar="lambda")

aucs <- array()
cont <- 1
for(l in c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1)){
  fit <- glmnet(x = X_train, y=as.factor(dados_train[,'quality_bin']),
                alpha=1, family="binomial", lambda = l)
  prob <- predict(fit, X_val, type="response")[,1]
  
  metrics <- prediction(prob,dados_val[,'quality_bin'])
  perf <- performance(metrics, measure="auc")@y.values
  
  aucs[cont] <- perf
  cont <- cont+1
}
aucs

fit <- glmnet(x = X_train, y=as.factor(dados_train$quality_bin),
              alpha=1, family="binomial", lambda = 0)

## Evaluate results in test data set
dados_test <- na.omit(dados_test)
dados_test[,"prob"] <- predict(fit, newx=X_test, type="response")[,1]

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"],dados_test[,'quality_bin'])
performance(metrics, measure="auc")@y.values




## Ridge
fit <- glmnet(x = X_train, y=as.factor(dados_train$quality_bin),
              alpha=0, family="binomial")
plot(fit, xvar="lambda")

aucs <- array()
cont <- 1
for(l in c(0, 0.0001, 0.001, 0.01, 0.05, 0.1, 0.5, 1)){
  fit <- glmnet(x = X_train, y=as.factor(dados_train[,'quality_bin']),
                alpha=0, family="binomial", lambda = l)
  prob <- predict(fit, X_val, type="response")[,1]
  
  metrics <- prediction(prob,dados_val[,'quality_bin'])
  perf <- performance(metrics, measure="auc")@y.values
  
  aucs[cont] <- perf
  cont <- cont+1
}
aucs


fit <- glmnet(x = X_train, y=as.factor(dados_train$quality_bin),
              alpha=1, family="binomial", lambda = 0)

## Evaluate results in test data set
dados_test <- na.omit(dados_test)
dados_test[,"prob"] <- predict(fit, newx=X_test, type="response")[,1]

hist(dados_test[,"prob"])

metrics <- prediction(dados_test[,"prob"],dados_test[,'quality_bin'])
performance(metrics, measure="auc")@y.values
