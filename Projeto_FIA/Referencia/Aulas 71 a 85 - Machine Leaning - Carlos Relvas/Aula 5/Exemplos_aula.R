### Cross-Validation

cv <- function(df,
               formula_model, 
               folds,
               target){
  id_perm <- sample(1:nrow(df), nrow(df))
  df_cv <- df[id_perm,]
  fold_size <- floor(nrow(df)/folds)
  ini <- 1
  end <- fold_size
  for(i in 1:folds){
    df_cv[ini:(min(end, nrow(df_cv))),'fold'] <- i
    ini <- end+1
    end <- end+fold_size
  }
  AUC <- array()
  for(i in 1:folds){
    fit <- glm(as.formula(formula_model), 
               data=df_cv[df_cv$fold != i,],
               family='binomial')
    prob <- predict(fit, 
                    newdata=df_cv[
                      df_cv$fold == i,], 
                    type='response')
    metrics <- prediction(prob,
                          df_cv[
                            df_cv$fold == i,
                            target])
    perf <- performance(metrics, measure="auc")
    AUC[i] = perf@y.values
  }
  return(AUC)
}

df <- read.table("train_titanic.csv", stringsAsFactors = F, header=T, sep=",")

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]

AUCS <- cv(df_train, 
           "as.factor(Survived) ~ Parch+SibSp+
           Pclass", 
            5, 'Survived')

hist(unlist(AUCS))
mean(unlist(AUCS))

AUCS <- cv(df_train, 
           "as.factor(Survived) ~ Parch+SibSp+Pclass", 
           10, 'Survived')

hist(unlist(AUCS))
mean(unlist(AUCS))

df_train[,'Age_imp'] <- df_train[,'Age']
df_train[is.na(df_train$Age),
         'Age_imp'] <- median(df_train$Age,na.rm=T)

AUCS <- cv(df_train, 
           "as.factor(Survived) ~ 
           Parch+SibSp+Pclass+Age_imp", 
           10, 'Survived')

hist(unlist(AUCS))
mean(unlist(AUCS))

cv_imp <- function(df, formula_model, 
                   folds, target){
  id_perm <- sample(1:nrow(df), nrow(df))
  df_cv <- df[id_perm,]
  fold_size <- floor(nrow(df)/folds)
  ini <- 1
  end <- fold_size
  for(i in 1:folds){
    df_cv[ini:(min(end, nrow(df_cv))),'fold'] <- i
    ini <- end+1
    end <- end+fold_size
  }
  AUC <- array()
  for(i in 1:folds){
    df_cv[,'Age_imp'] <- df_cv[,'Age']
    df_cv[is.na(df_cv$Age),
          'Age_imp'] <- median(df_cv[df_cv$fold != i,'Age'],
                               na.rm=T)
    fit <- glm(as.formula(formula_model), data=df_cv[df_cv$fold != i,], family='binomial')
    prob <- predict(fit, 
                    newdata=df_cv[df_cv$fold == i,], type='response')
    metrics <- prediction(prob, df_cv[df_cv$fold == i,target])
    perf <- performance(metrics, measure="auc")
    AUC[i] = perf@y.values
  }
  return(AUC)
}

AUCS <- cv_imp(df_train, 
           "as.factor(Survived) ~ Parch+SibSp+Pclass+Age_imp", 
           5, 'Survived')

hist(unlist(AUCS))
mean(unlist(AUCS))

AUCS <- cv_imp(df_train, 
           "as.factor(Survived) ~ Parch+SibSp+Pclass+Age_imp", 
           10, 'Survived')

hist(unlist(AUCS))
mean(unlist(AUCS))



### Caret

dados <- read.table("train_titanic.csv", stringsAsFactors = F, header=T, sep=",")

library(caret)
featurePlot(x = dados[, c("Age", "SibSp", "Parch", "Fare")],
            y = as.factor(dados$Survived),
            plot = "pairs", auto.key = list(columns = 2))

featurePlot(x = dados[, c("Age", "SibSp", "Parch", "Fare")],
            y = as.factor(dados$Survived), layout = c(4,1 ),
            plot = "box", auto.key = list(columns = 2),
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)))

featurePlot(x = dados[, c("Age", "SibSp", "Parch", "Fare")],
            y = as.factor(dados$Survived), layout = c(4,1 ),
            plot = "density", auto.key = list(columns = 2),
            scales = list(y = list(relation="free"),
                          x = list(relation = "free")))


library(caret)
set.seed(1234)
inTraining <- createDataPartition(dados$Survived, p=0.6, 
                                  list=FALSE)
training.set <- dados[inTraining,]
Totalvalidation.set <- dados[-inTraining,]
inValidation <- createDataPartition(Totalvalidation.set$Survived, 
                                    p=0.5, list=FALSE)
testing.set <- Totalvalidation.set[inValidation,]
validation.set <- Totalvalidation.set[-inValidation,]


dummies <- dummyVars(Survived ~ Sex, data = dados)
head(predict(dummies, newdata = dados))

nearZeroVar(dados, saveMetrics= TRUE)

descrCor <-  cor(dados[, c("Age", "SibSp", "Parch", "Fare")],
                 use = "complete.obs")
descrCor
findCorrelation(descrCor, cutoff = .75)


preProcValues <- preProcess(dados[, c("Age", "SibSp", "Parch", "Fare")],
                            method = c("center", "scale"))
Transformed <- predict(preProcValues, 
                       dados[, c("Age", "SibSp", "Parch", "Fare")])

aux = dados[, c("Age", "SibSp", "Parch", "Fare")]
preProcValues <- preProcess(aux,
                            method = c("medianImpute"))
aux[6,]
predict(preProcValues, aux[6,])

aux <- training.set[, c("Age", "SibSp", "Parch", "Fare")]
preProcValues <- preProcess(aux,method = c("medianImpute"))
aux_imp <- predict(preProcValues, newdata=aux)
rf <- train(y=as.factor(training.set$Survived), 
            x=aux_imp, 
            method = "rf",trControl = trainControl(method = "oob"),
            importance = TRUE, verbose = TRUE, 
            tuneGrid = data.frame(mtry = 3))
varImp(rf)

aux_val <- validation.set[, c("Age", "SibSp", "Parch", "Fare")]
pre_val <- predict(preProcValues, newdata=aux_val)
pred_var <- predict(rf, newdata = pre_val)
confusionMatrix(pred_var, validation.set$Survived)






aux <- training.set[, c("Age", "SibSp", "Parch", "Fare")]
aux_val <- validation.set[, c("Age", "SibSp", "Parch", "Fare")]
preProcValues <- preProcess(aux,method = c("center", "scale", "medianImpute"))
dados_train <- predict(preProcValues, newdata=aux)
dados_val <- predict(preProcValues, newdata=aux_val)
dados_train <- cbind(dados_train, 
                     training.set[,c("Survived", "Sex", "Pclass", "Embarked")])
dados_val <- cbind(dados_val, 
                     validation.set[,c("Survived", "Sex", "Pclass", "Embarked")])

for(i in c('Survived', 'Pclass', 'Embarked')){
  dados_train[,i] <- as.factor(dados_train[,i])
  dados_val[,i] <- as.factor(dados_val[,i])
}


feature.names=names(dados_train)

for (f in feature.names) {
  if (class(dados_train[[f]])=="factor") {
    levels <- unique(c(dados_train[[f]]))
    dados_train[[f]] <- factor(dados_train[[f]],
                         labels=make.names(levels))
    dados_val[[f]] <- factor(dados_val[[f]],
                             labels=make.names(levels))
  }
}

formula <- "Survived ~ Age+SibSp+Parch+Fare+Sex+Pclass+Embarked"

fitControl <- trainControl(method = "cv", number = 4, classProbs = T)
rfGrid <-  expand.grid(mtry=c(1,2,3,4,5,6,7))

nrow(rfGrid)


set.seed(825)
rfFit <- train(as.formula(formula), data = dados_train,
                 method = "rf", trControl = fitControl,
                 verbose = FALSE,tuneGrid = rfGrid)

rfFit
plot(rfFit)
sum(predict(rfFit, newdata=dados_val)==dados_val$Survived)/nrow(dados_val)
confusionMatrix(data = predict(rfFit, newdata=dados_val), 
                reference = dados_val$Survived)
matrix_prev <- data.frame(dados_val$Survived,
                          predict(rfFit, newdata=dados_val),
                          predict(rfFit, newdata=dados_val, type='prob')[,1],
                          predict(rfFit, newdata=dados_val, type='prob')[,2])
names(matrix_prev) <- c('obs', 'pred', 'X1', 'X2')
twoClassSummary(data = matrix_prev, lev = levels(matrix_prev$obs))
mnLogLoss(matrix_prev, lev = levels(matrix_prev$obs))




df <- read.csv('base_gastos_cartao.csv', header = T)
library(caret)
set.seed(1234)
inTraining <- createDataPartition(df$Gastos_Cartao, p=0.7, 
                                  list=FALSE)
df_train <- df[inTraining,]
df_test <- df[-inTraining,]


formula <- "Gastos_Cartao ~ Idade+Renda+Impostos+Segmento"

fitControl <- trainControl(method = "cv", number = 4)
rfGrid <-  expand.grid(mtry=c(1,2,3,4))

nrow(rfGrid)

set.seed(42)
rfFit <- train(as.formula(formula), data = df_train,
               method = "rf", trControl = fitControl,
               verbose = FALSE, tuneGrid = rfGrid)

rfFit
plot(rfFit)

postResample(pred = predict(rfFit, newdata=df_test), obs = df_test$Gastos_Cartao)

