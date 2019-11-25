### Cross-Validation

df <- read.csv('cancer.data.txt', header = F)

names(df) <- c('Id', 'Clump_Thick','Cell_Size', 'Cell_Shape',
               'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
               'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses', 'Class')


df[df$Bare_Nuclei=='?','Bare_Nuclei'] = NA

for(i in names(df)){
  df[,i] <- as.numeric(df[,i])
}

df[,"Class"] <- as.factor(df[,"Class"])

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]


cv_imp <- function(df_cross, formula_model, 
                   folds, target){
  id_perm <- sample(1:nrow(df_cross), nrow(df_cross))
  df_cv <- df_cross[id_perm,]
  fold_size <- round(nrow(df_cross)/folds)
  ini <- 1
  end <- fold_size
  for(i in 1:folds){
    df_cv[ini:(min(end, nrow(df_cv))),'fold'] <- i
    ini <- end+1
    end <- end+fold_size
  }
  AUC <- array()
  for(i in 1:folds){
    df_cv[,'Bare_Nuclei_imp'] <- df_cv[,'Bare_Nuclei']
    df_cv[is.na(df_cv$Bare_Nuclei),'Bare_Nuclei_imp'] <- median(df_cv[df_cv$fold != i,
                                                              'Bare_Nuclei'], na.rm=T)
    fit <- randomForest(as.formula(formula_model), data=df_cv[df_cv$fold != i,])
    prob <- predict(fit, newdata=df_cv[df_cv$fold == i,], type='prob')[,2]
    metrics <- prediction(prob, df_cv[df_cv$fold == i,target])
    perf <- performance(metrics, measure="auc")
    AUC[i] = perf@y.values
  }
  return(AUC)
}


formula_cv <- paste0("Class ~ ", 
                  paste0(c(names(df_train)[c(2:6,8:10)], 'Bare_Nuclei_imp'),
                         collapse = '+'))

AUCS <- cv_imp(df_train, 
               formula_cv, 
               10, 'Class')

hist(unlist(AUCS))
mean(unlist(AUCS))



### Caret

df <- read.csv('cancer.data.txt', header = F)

names(df) <- c('Id', 'Clump_Thick','Cell_Size', 'Cell_Shape',
               'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
               'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses', 'Class')


df[df$Bare_Nuclei=='?','Bare_Nuclei'] = NA

for(i in names(df)){
  df[,i] <- as.numeric(df[,i])
}


library(caret)
featurePlot(x = df[, c('Clump_Thick','Cell_Size', 'Cell_Shape',
                          'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
                          'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses')],
            y = as.factor(df$Class),
            plot = "pairs", auto.key = list(columns = 2))

featurePlot(x = df[, c('Clump_Thick','Cell_Size', 'Cell_Shape',
                       'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
                       'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses')],
            y = as.factor(df$Class), layout = c(4,1 ),
            plot = "box", auto.key = list(columns = 2),
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)))

featurePlot(x = df[, c('Clump_Thick','Cell_Size', 'Cell_Shape',
                       'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
                       'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses')],
            y = as.factor(df$Class), layout = c(4,1 ),
            plot = "density", auto.key = list(columns = 2),
            scales = list(y = list(relation="free"),
                          x = list(relation = "free")))


library(caret)
set.seed(1234)
inTraining <- createDataPartition(df$Class, p=0.7, 
                                  list=FALSE)
df_train <- df[inTraining,]
df_test <- df[-inTraining,]


aux <- df_train[,2:10]
preProcValues <- preProcess(aux,method = c("medianImpute"))
aux_imp <- predict(preProcValues, newdata=aux)


rf <- train(y=as.factor(df_train$Class), 
            x=aux_imp, 
            method = "rf",trControl = trainControl(method = "oob"),
            importance = TRUE, verbose = TRUE, 
            tuneGrid = data.frame(mtry = 3))
varImp(rf)

aux_val <- df_test[,2:10]
pre_val <- predict(preProcValues, newdata=aux_val)
pred_var <- predict(rf, newdata = pre_val)
confusionMatrix(pred_var, df_test$Class)




