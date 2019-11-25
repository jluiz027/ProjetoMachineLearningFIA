### Linear Regression
setwd("C:/Users/DATA MINING T12/Desktop/Data Mining T12/Aulas 71 a 85 - Machine Leaning - Carlos Relvas/Aula 1")
install.packages("car")
library(car)
dados <- read.table("base_gastos_cartao.csv", sep=",", header=T)
head(dados)

scatterplotMatrix(~ Gastos_Cartao + Idade + Renda +
                    Impostos, data=dados, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  by.groups=TRUE, diagonal="none")

scatterplotMatrix(~ Gastos_Cartao + Idade + Renda +
                    Impostos | Segmento, data=dados, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  by.groups=TRUE, diagonal="none")

nrow(dados)
set.seed(432)
id <- sample(1:nrow(dados), nrow(dados)*0.7)
dados.des <- dados[id,]
dados.test <- dados[-id,]

fit <- lm(Gastos_Cartao~Renda, data = dados.des)
summary(fit)
fit.val <- predict(fit, newdata=dados.test)

head(fit.val)
head(dados.test$Gastos_Cartao)

plot(dados.des[,"Renda"], dados.des[,"Gastos_Cartao"], pch=16,
     xlab="Renda", ylab="Gastos Cart?o")
abline(a=fit$coefficients[1],b=fit$coefficients[2], col="red", lwd=3)

plot(dados.test[, "Gastos_Cartao"], fit.val, pch=16,
     xlab="Observado", ylab="Ajustado")
abline(a=0,b=1)

## Error
error.test <- dados.test[, "Gastos_Cartao"]-fit.val
error.des <- dados.des[, "Gastos_Cartao"]-fit$fitted.values

hist(error.test, main="Histograma Erro Teste")
hist(error.des, main="Histograma Erro Desenvolvimento")

## Mean Square Error
MSE1.test <- sum((error.test)^2)/nrow(dados.test)
MSE1.des <- sum((error.des)^2)/nrow(dados.des)
## Mean Absolute Error
MAE1.test <- sum(abs(error.test)/nrow(dados.test))
MAE1.des <- sum(abs(error.des)/nrow(dados.des))


fit <- lm(Gastos_Cartao~Idade+Renda+Impostos, data = dados.des)
summary(fit)
fit.val2 <- predict(fit, newdata=dados.test)

## Error
error.test2 <- dados.test[, "Gastos_Cartao"]-fit.val2
error.des2 <- dados.des[, "Gastos_Cartao"]-fit$fitted.values

hist(error.test2, main="Histograma Erro Teste")
hist(error.des2, main="Histograma Erro Desenvolvimento")

## Mean Square Error
MSE2.test <- sum((error.test2)^2)/nrow(dados.test)
MSE2.des <- sum((error.des2)^2)/nrow(dados.des)
## Mean Absolute Error
MAE2.test <- sum(abs(error.test2)/nrow(dados.test))
MAE2.des <- sum(abs(error.des2)/nrow(dados.des))

par(mfrow=c(2,1))
plot(dados.test[, "Gastos_Cartao"], fit.val, pch=16,
     xlab="Observado", ylab="Ajustado", main="Modelo 1")
abline(a=0,b=1)
plot(dados.test[, "Gastos_Cartao"], fit.val2, pch=16,
     xlab="Observado", ylab="Ajustado", main="Modelo 2")
abline(a=0,b=1)



error.test2 <- dados.test[, "Gastos_Cartao"]-fit.val2
error.test <- dados.test[, "Gastos_Cartao"]-fit.val

MSE2.test <- sum((error.test2)^2)/nrow(dados.test)
MAE2.test <- sum(abs(error.test2)/nrow(dados.test))
MSE1.test <- sum((error.test)^2)/nrow(dados.test)
MAE1.test <- sum(abs(error.test)/nrow(dados.test))
MSE1.test
MSE2.test
MAE1.test
MAE2.test

par(mfrow=c(2,1))
hist(error.test, breaks=seq(-150,150,by=10),
     main="Histograma Erro Teste - Modelo 1")
hist(error.test2, breaks=seq(-150,150,by=10),
     main="Histograma Erro Teste - Modelo 2")






## TREE

install.packages("party")
library(party)
dados = read.table("spambase.data", sep=",", header=F)
nomes = c("word_freq_make",  "word_freq_address",  "word_freq_all",	"word_freq_3d",	
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

set.seed(432)
#Pegar uma amostra da base inteira embaralhada
id <- sample(1:nrow(dados), nrow(dados))
#pega todos os dados com a sequencia embaralhada
dados <- dados[id,]
dados.des <- dados[1:floor(0.6*nrow(dados)),]
dados.val <- dados[0.6*nrow(dados):(0.8*nrow(dados)),]
dados.test <- dados[0.8*nrow(dados):nrow(dados),]
formula = paste0("as.factor(", nomes[58], ") ~ ", paste0(nomes[1:57], collapse="+"))
help(ctree)
tree = ctree(as.formula(formula), data=dados.des, controls = ctree_control(maxdepth = 3))
print(tree)
plot(tree)
plot(tree, type="simple")

## Ser? que profundidade 3 ? o melhor dos casos?

tree = ctree(as.formula(formula), data=dados.des, controls = ctree_control(maxdepth = 3))
fit.val.prof3 = predict(tree, newdata=dados.val)
sum(ifelse(fit.val.prof3!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
tree10 = ctree(as.formula(formula), data=dados.des, controls = ctree_control(maxdepth = 10))
fit.val.prof10 = predict(tree10, newdata=dados.val)
sum(ifelse(fit.val.prof10!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
tree11 = ctree(as.formula(formula), data=dados.des, controls = ctree_control(maxdepth = 11))
fit.val.prof11 = predict(tree11, newdata=dados.val)
sum(ifelse(fit.val.prof11!=dados.val[,"SPAM"],1,0))/nrow(dados.val)

fit.des = predict(tree10, newdata=dados.des)
fit.val = predict(tree10, newdata=dados.val)
fit.test = predict(tree10, newdata=dados.test)

sum(ifelse(fit.des!=dados.des[,"SPAM"],1,0))/nrow(dados.des)
sum(ifelse(fit.val!=dados.val[,"SPAM"],1,0))/nrow(dados.val)
sum(ifelse(fit.test!=dados.test[,"SPAM"],1,0))/nrow(dados.test)

