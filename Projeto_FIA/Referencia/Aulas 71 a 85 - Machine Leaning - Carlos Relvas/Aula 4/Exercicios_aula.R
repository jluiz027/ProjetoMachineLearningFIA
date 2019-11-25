### Random Forest

df <- read.csv('cancer.data.txt', header = F)

df <- na.omit(df)

set.seed(42)
id <- sample(1:nrow(df), nrow(df), replace = F)
df <- df[id,]
df_train <- df[1:(nrow(df)*0.6),]
df_val <- df[(nrow(df)*0.6):(nrow(df)*0.8),]
df_test <- df[(nrow(df)*0.8):(nrow(df)*1),]

nomes <- names(df)
formula <- paste0("as.factor(", nomes[11], ") ~ ", paste0(nomes[2:10], collapse="+"))

rf <- randomForest(as.formula(formula), data=df_train, ntree=100)

plot(rf)

fit.val = predict(rf, newdata=df_val)
fit.test = predict(rf, newdata=df_test)

sum(ifelse(fit.val!=df_val[,11],1,0))/nrow(df_val)
sum(ifelse(fit.test!=df_test[,11],1,0))/nrow(df_test)


grid <- c(1,2,5,10)
output <- matrix(nr=length(grid),nc=2)
colnames(output) <- c("m", "perf")
cont <- 1
for(m in grid){
  rf <- randomForest(as.formula(formula), data=df_train, ntree=100, mtry=m)
  fit.val <- predict(rf, newdata=df_val)
  output[cont,1] <- m
  output[cont,2] <- sum(ifelse(fit.val!=df_val[,11],1,0))/nrow(df_val)
  cont <- cont+1
  print(paste0("M: ", m))
}

output

rf <- randomForest(as.formula(formula), data=df_train, ntree=100, mtry=1)

fit.val = predict(rf, newdata=df_val)
fit.test = predict(rf, newdata=df_test)

sum(ifelse(fit.val!=df_val[,11],1,0))/nrow(df_val)
sum(ifelse(fit.test!=df_test[,11],1,0))/nrow(df_test)


### Missing

df <- read.csv('cancer.data.txt', header = F)

names(df) <- c('Id', 'Clump_Thick','Cell_Size', 'Cell_Shape',
               'Marginal_Adhesion', 'Epi_Cell_Size', 'Bare_Nuclei',
               'Bland_Chromatin', 'Normal_Nucleoli', 'Mitoses', 'Class')


df[df$Bare_Nuclei=='?','Bare_Nuclei'] = NA

for(i in names(df)){
  df[,i] <- as.numeric(df[,i])
}

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]

## Mediana

df_train[,'Bare_Nuclei_imp'] <- df_train[,'Bare_Nuclei']
df_test[,'Bare_Nuclei_imp'] <- df_test[,'Bare_Nuclei']
mediana = median(df_train[,'Bare_Nuclei'], na.rm=T)
df_train[is.na(df_train$Bare_Nuclei),'Bare_Nuclei_imp'] = mediana
df_test[is.na(df_test$Bare_Nuclei),'Bare_Nuclei_imp'] = mediana

formula <- paste0("as.factor(Class) ~ ", 
                  paste0(names(df_train)[c(2:6,8:10,12)], collapse = '+'))

fit <- glm(as.formula(formula), data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Class)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))



## Modelo

fit_bare <- lm(as.formula(paste0("Bare_Nuclei ~ ", 
                          paste0(names(df_train)[c(2:6,8:10)], 
                                 collapse = '+'))), data=df_train)

df_train[,'Bare_Nuclei_imp'] <- ifelse(is.na(df_train$Bare_Nuclei),
                               predict(fit_bare, newdata=df_train),
                               df_train$Bare_Nuclei)
df_test[,'Bare_Nuclei_imp'] <- ifelse(is.na(df_test$Bare_Nuclei),
                              predict(fit_bare, newdata=df_test),
                              df_test$Bare_Nuclei)

formula <- paste0("as.factor(Class) ~ ", 
                  paste0(names(df_train)[c(2:6,8:10,12)], collapse = '+'))

fit <- glm(as.formula(formula), data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Class)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))


## -9999

df_train[,'Bare_Nuclei_imp'] <- df_train[,'Bare_Nuclei']
df_test[,'Bare_Nuclei_imp'] <- df_test[,'Bare_Nuclei']
df_train[is.na(df_train$Bare_Nuclei),'Bare_Nuclei_imp'] = -9999
df_test[is.na(df_test$Bare_Nuclei),'Bare_Nuclei_imp'] = -9999

formula <- paste0("as.factor(Class) ~ ", 
                  paste0(names(df_train)[c(2:6,8:10,12)], collapse = '+'))

fit <- ctree(as.formula(formula), data = df_train, 
             controls = ctree_control(maxdepth=5))

prob <- unlist(lapply(predict(fit, newdata=df_test, type='prob'), function(x) x[[2]]))
library(ROCR)
metrics <- prediction(prob, df_test$Class)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))


fit <- randomForest(as.formula(formula), data = df_train, 
                    controls = ctree_control(maxdepth=5))

prob <- predict(fit, newdata=df_test, type='prob')[,2]
library(ROCR)
metrics <- prediction(prob, df_test$Class)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))







