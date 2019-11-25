## Random Forest

install.packages("randomForest")
library(randomForest)
dados = read.table("spambase.data", sep=",", header=F)
nomes = c("word_freq_make",  "word_freq_address",  "word_freq_all",  "word_freq_3d",  
          "word_freq_our",	"word_freq_over",	"word_freq_remove",	"word_freq_internet",
          "word_freq_order",	"word_freq_mail",	"word_freq_receive",	"word_freq_will",
          "word_freq_people",	"word_freq_report",	"word_freq_addresses",	"word_freq_free",	
          "word_freq_business",	"word_freq_email",	"word_freq_you",	"word_freq_credit",
          "word_freq_your",	"word_freq_font",	"word_freq_000",	"word_freq_money",
          "word_freq_hp",	"word_freq_hpl",	"word_freq_george",	"word_freq_650",
          "word_freq_lab",	"word_freq_labs",	"word_freq_telnet",	"word_freq_857",
          "word_freq_data",	"word_freq_415",	"word_freq_85",	"word_freq_technology",
          "word_freq_1999",	"word_freq_parts",	"word_freq_pm",	"word_freq_direct",	
          "word_freq_cs",	"word_freq_meeting",	"word_freq_original",	"word_freq_project",
          "word_freq_re",	"word_freq_edu",	"word_freq_table",	"word_freq_conference",	
          "char_freq_pvir",	"char_freq_par",	"char_freq_bra",	"char_freq_exc",
          "char_freq_dolar", "char_freq_num",	"capital_run_length_average",
          "capital_run_length_longest", "capital_run_length_total",	"SPAM")
names(dados) = nomes

set.seed(432)
id <- sample(1:nrow(dados), nrow(dados)*0.8)
id.des <- sample(id, nrow(dados)*0.7)
id.val <- id[!(id %in% id.des)]
dados.des <- dados[id.des,]
dados.val <- dados[id.val,]
dados.test <- dados[-id,]
formula <- paste0("as.factor(", nomes[58], ") ~ ", paste0(nomes[1:57], collapse="+"))
help(randomForest)
rf <- randomForest(as.formula(formula), data=dados.des, ntree=500)

plot(rf)

fit.val = predict(rf, newdata=dados.val)
fit.test = predict(rf, newdata=dados.test)

sum(ifelse(fit.val!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
sum(ifelse(fit.test!=dados.test[,"SPAM"],1,0))/nrow(dados.test)


grid <- c(1,10,20,30,40,50)
output <- matrix(nr=length(grid),nc=2)
colnames(output) <- c("m", "perf")
cont <- 1
for(m in grid){
  rf <- randomForest(as.formula(formula), data=dados.des, ntree=500, mtry=m)
  fit.val <- predict(rf, newdata=dados.val)
  output[cont,1] <- m
  output[cont,2] <- sum(ifelse(fit.val!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
  cont <- cont+1
  print(paste0("M: ", m))
}

output

rf <- randomForest(as.formula(formula), data=dados.des, ntree=500, mtry=10)

fit.val = predict(rf, newdata=dados.val)
fit.test = predict(rf, newdata=dados.test)

sum(ifelse(fit.val!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
sum(ifelse(fit.test!=dados.test[,"SPAM"],1,0))/nrow(dados.test)




### Missing Data


df <- read.table("train_titanic.csv",
                 stringsAsFactors = F,
                 header=T, sep=",")

set.seed(42)
id <- sample(1:nrow(df), 0.7*nrow(df))
df_train <- df[id,]
df_test <- df[-id,]

for(i in c('Parch','SibSp','Pclass', 'Age')){
  cat(paste0(i, "  : ", sum(is.na(df_train[,i]))), "\n")
}

formula <- "as.factor(Survived) ~ 
Parch+SibSp+Pclass+Age"

fit <- glm(as.formula(formula), 
           data = df_train, 
           family="binomial")

## Mediana

df_train[,'Age_imp'] <- df_train[,'Age']
df_test[,'Age_imp'] <- df_test[,'Age']
mediana = median(df_train[,'Age'], na.rm=T)
df_train[is.na(df_train$Age),
         'Age_imp'] = mediana
df_test[is.na(df_test$Age),
        'Age_imp'] = mediana

formula <- "as.factor(Survived) ~ 
Parch+SibSp+Pclass+Age_imp"

fit <- glm(as.formula(formula), 
           data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))


## Step Functions

plot(tapply(df_train$Survived, 
            df_train$Age, mean), pch=16,
     xlab="idade", 
     ylab='Proporção de sobrevinventes')


df_train[,'Age_cat'] <- cut(df_train[,'Age'],
                            breaks = c(-Inf,20,40,60,Inf), 
                            labels = FALSE )
df_test[,'Age_cat'] <- cut(df_test[,'Age'],
                           breaks = c(-Inf,20,40,60,Inf), 
                           labels = FALSE )
df_train[is.na(df_train$Age),'Age_cat'] = 'Missing'
df_test[is.na(df_test$Age),'Age_cat'] = 'Missing'

formula <- "as.factor(Survived) ~ 
Parch+SibSp+Pclass+Age_cat"

fit <- glm(as.formula(formula), data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))


## Modelo

fit_age <- lm(as.formula("Age ~ Parch+SibSp+Pclass"),
              data=df_train)

df_train[,'Age_imp'] <- ifelse(is.na(df_train$Age),
                               predict(fit_age, newdata=df_train),
                               df_train$Age)
df_test[,'Age_imp'] <- ifelse(is.na(df_test$Age),
                              predict(fit_age, newdata=df_test),
                              df_test$Age)

formula <- "as.factor(Survived) ~ 
Parch+SibSp+Pclass+Age_imp"

fit <- glm(as.formula(formula), data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))


## -9999

df_train[,'Age_imp'] <- df_train[,'Age']
df_test[,'Age_imp'] <- df_test[,'Age']
df_train[is.na(df_train$Age),'Age_imp'] = -9999
df_test[is.na(df_test$Age),'Age_imp'] = -9999

formula <- "as.factor(Survived) ~
Parch+SibSp+Pclass+Age_imp"

fit <- glm(as.formula(formula), data = df_train, family="binomial")

prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))

library(party)
fit <- ctree(as.formula(formula),
             data = df_train, 
             controls = ctree_control(maxdepth=5))
plot(fit)

prob <- unlist(lapply(
  treeresponse(fit,newdata=df_test),
  function(x) x[[2]]))
##prob <- predict(fit, newdata=df_test, type='response')
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))

library(randomForest)
fit <- randomForest(as.formula(formula),
                    data = df_train, 
                    controls = ctree_control(maxdepth=5))

prob <- predict(fit, newdata=df_test, type='prob')[,2]
library(ROCR)
metrics <- prediction(prob, df_test$Survived)
perf <- performance(metrics, measure="auc")
print(paste0("AUC: ", perf@y.values))
