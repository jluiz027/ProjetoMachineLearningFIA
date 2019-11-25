#Importacao da tabela "Emprestimo Bancario.xslx"
install.packages("readxl")
library(readxl)
empbanc<-read_excel("Emprestimo_Bancario.xlsx", sheet ="Plan1")
View(empbanc)

#Agrupando cada variavel em classes (nesse caso, 4 classes)
install.packages("arules")
library(arules)
idade_cl<-discretize(empbanc$idade, "frequency", breaks =  4)
experiencia_cl<-discretize(empbanc$experiencia, "frequency", breaks = 4)
tempend_cl<-discretize(empbanc$tempo_endereco, "frequency", breaks = 4)
renda_cl<-discretize(empbanc$renda, "frequency", breaks = 4)
empbanc<-data.frame(empbanc, idade_cl, experiencia_cl,tempend_cl,renda_cl)
View(empbanc)

#Cruzando cada variavel com o target "classif"
install.packages("descr")
library(descr)
CrossTable(empbanc$idade_cl, empbanc$classif, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(empbanc$educacao, empbanc$classif, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(empbanc$experiencia_cl, empbanc$classif, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(empbanc$tempend_cl, empbanc$classif, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)
CrossTable(empbanc$renda_cl, empbanc$classif, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Calculando o nivel de importancia de cada variavel para explicar "classif"
install.packages("Information")
library(Information)
empbanc_select<-empbanc[,c(3,7,8,9,10,11)]
View(empbanc_select)
IV <- create_infotables(data = empbanc_select, y = "classif", ncore = 2)
print(head(IV$Summary, 10), row.names = FALSE)
print(IV$Tables$experiencia_cl, row.names = FALSE) #Apenas para exemplificar

#Calculo da correlacao entre as variaveis
matriz_correl <- round(cor(empbanc[,2:7]), 2)
matriz_correl
install.packages("corrplot")
library(corrplot)
matriz_correl_I<-corrplot(matriz_correl, method = "circle")
matriz_correl_I

#Analise do VIF para detectar potencial multicolinearidade
install.packages("HH")
library(HH)
model <- lm(classif ~ idade_cl+educacao+experiencia_cl+tempend_cl+
            renda_cl, data = empbanc)
summary(model)
vif(model)

#Importacao da tabela "Loan Payments Data"
loan<-read_excel("Loan_Payments_Data.xlsx", sheet ="Planilha1")
View(loan)
loan$classif<-ifelse(loan$loan_status == "COLLECTION", 1, 0)
View(loan)

#Categorizacao da variavel "age" em 15 bins
library(arules)
loan$age_cl<-discretize(loan$age, "frequency", breaks =  15)
View(loan)

#Calculo do Information Value e do WOE
library(Information)
IV <- create_infotables(data = loan, y = "classif", ncore = 2)
IV_valor<-data.frame(IV$Summary)
print(IV$Tables$age_cl, row.names=FALSE)
age_cl=data.frame(IV$Tables$age_cl)
View(age_cl)
plot_infotables(IV, "age_cl")

#Novo agrupamento de age_cl, agora em 5 grupos
loan$age_cl_I<-ifelse(loan$age<25, 1,
               ifelse(loan$age<27, 2,
               ifelse(loan$age<32, 3,
               ifelse(loan$age<33, 4,
               ifelse(loan$age>=33, 5, 0)))))
View(loan)
IV <- create_infotables(data = loan, y = "classif", ncore = 2)
IV$Summary
IV_valor<-data.frame(IV$Summary)
View(IV_valor)
print(IV$Tables$age_cl_I, row.names=FALSE)
age_cl_I=data.frame(IV$Tables$age_cl_I)
View(age_cl_I)
plot_infotables(IV, "age_cl_I")



#Teste do optimal binning para a variavel Idade, da base Emprestimo Bancario
install.packages("smbinning")
library(smbinning)
empbanc<-as.data.frame(empbanc)
class(empbanc)
idade_optbin<-smbinning(df=empbanc, y="classif", x="idade", p=0.05)
idade_optbin
empbanc$idade_optbin_cl<-ifelse(empbanc$idade<=26,1,
                        ifelse(empbanc$idade<=31,2,
                               ifelse(empbanc$idade<=40,3,
                                      ifelse(empbanc$idade>40,4,0))))

empbanc$idade_quartis<-discretize(empbanc$idade, "frequency", breaks =  4)
View(empbanc)
IV <- create_infotables(data = empbanc, y = "classif", ncore = 2)
IV$Summary

#Importacao da base de dados de clientes do produto cartao de credito
loanpred<-read.csv("Loan_Prediction_Data_train.csv", sep = ",")
View(loanpred)
#Checagem de missing values
install.packages("Amelia")
library(Amelia)
missmap(loanpred)
sum(is.na(loanpred$Credit_History))/nrow(loanpred)

#Substituição de missing values das variáveis Credit History, Loan Amount e Loan Amount Term por 0
install.packages("anchors")
library(anchors)
sum(is.na(loanpred$Credit_History))/nrow(loanpred) #% de missing values de Credit History
loanpred_I<- replace.value(loanpred,c("Credit_History", "LoanAmount", "Loan_Amount_Term"))
View(loanpred_I)
sum(is.na(loanpred_I$Credit_History))/nrow(loanpred) #% de missing values de Credit History
sum(is.na(loanpred_I$LoanAmount))/nrow(loanpred) #% de missing values de Loan Amount
sum(is.na(loanpred_I$Loan_Amount_Term))/nrow(loanpred) #% de missing values de Loan Amount Term

#Convertendo o target em numérico e calculando o IV das variáveis
loanpred_I$Loan_Status<-ifelse(loanpred_I$Loan_Status=="Y", 0, 1)
table(loanpred_I$Loan_Status)
library(Information)
IV<-create_infotables(data = loanpred_I, y = "Loan_Status", ncore = 2)
IV$Summary

#Analisando a variável "Credit History"
library(descr)
CrossTable(loanpred_I$Credit_History, loanpred_I$Loan_Status, prop.r = FALSE, prop.c = TRUE,
           prop.t = FALSE, prop.chisq = FALSE)
View(loanpred_I)
str(loanpred_I)

#Modelo de regressão logística com as variáveis da base Loan Prediction, exceto Loand_ID
modelo<-glm(Loan_Status ~ . , family = binomial (link='logit'), data = loanpred_I[,2:13])
summary(modelo)

#Estatísticas descritivas das variáveis Married, Self-Employed and Gender 
summary(loanpred$Gender)
summary(loanpred$Self_Employed)
summary(loanpred$Married)

#Substituindo os missing values das variáveis Married, Self-Employed and Gender pelo fator mais frequente
summary(loanpred_I$Married)
levels(loanpred_I$Married)[levels(loanpred_I$Married) == ""] <- "Yes"
summary(loanpred_I$Married)

summary(loanpred_I$Self_Employed)
levels(loanpred_I$Self_Employed)[levels(loanpred_I$Self_Employed) == ""] <- "No"
summary(loanpred_I$Self_Employed)

summary(loanpred_I$Gender)
levels(loanpred_I$Gender)[levels(loanpred_I$Gender) == ""] <- "Male"
summary(loanpred_I$Gender)

#Modelo de regressão logística com as variáveis da base Loan Prediction, exceto Loand_ID
modelo<-glm(Loan_Status ~ . , family = binomial (link='logit'), data = loanpred_I[,2:13])
summary(modelo)

CrossTable(loanpred_I$Gender, loanpred_I$Loan_Status, prop.r = FALSE,
           prop.t = FALSE, prop.chisq = FALSE)

#Redefinindo categoria de referência da variável Gender e atualizando o modelo
loanpred_I$Gender <- relevel(loanpred_I$Gender, ref = "Female")
modelo<-glm(Loan_Status ~ . , family = binomial (link='logit'), data = loanpred_I[,2:13])
summary(modelo)

modelo_pred = predict(modelo, loanpred_I, type="response")
loanpred_I<-data.frame(loanpred_I, modelo_pred)
View(loanpred_I)

#Avaliando sinais de multicolinearidade 
install.packages("sjPlot")
library(sjPlot)
loanpred_I[,c(2,3,4,5,6,12)] <- lapply(loanpred_I[,c(2,3,4,5,6,12)],as.numeric)
View(loanpred_I)
str(loanpred_I)
sjp.corr(loanpred_I[,2:13])
sjt.corr(loanpred_I[,2:13])

#Cálculo do VIF
install.packages("HH")
library(HH)
vif(modelo)

#Cálculo de métricas preditivas do modelo para a base Loan Prediction
#Cálculo do K-S (Kolmogorov Smirnov)
modelo_sat<-glm(Loan_Status ~ . , family = binomial (link='logit'), data = loanpred_I[,2:13])
summary(modelo_sat)
install.packages("dgof")
library(dgof)
modelo_sat_pred = predict(modelo_sat, loanpred_I, type="response")
loanpred_I<-data.frame(loanpred_I, modelo_sat_pred)
View(loanpred_I)

ks.test(loanpred_I$modelo_sat_pred[loanpred_I$Loan_Status==0],
        loanpred_I$modelo_sat_pred[loanpred_I$Loan_Status==1])

#Cálculo do % de classificação correta
loanpred_I$Loan_Status_pred<-ifelse(loanpred_I$modelo_sat_pred>0.5,1,0)
View(loanpred_I)
CrossTable(loanpred_I$Loan_Status_pred, loanpred_I$Loan_Status, prop.r = FALSE, prop.t = FALSE,
           prop.chisq = FALSE)

#Cálculo do AUC (Area Under Curve) e desenho da curva ROC
install.packages("ROCR")
install.packages("pROC")
library(ROCR)
library(pROC)
ROCRpred <- prediction(modelo_sat_pred, loanpred_I$Loan_Status)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
roc_obj <- roc(loanpred_I$Loan_Status, loanpred_I$modelo_sat_pred)
auc(roc_obj)

#Segmentação de uma base de funcionarios de uma empresa
HR<-read_excel("HR_Analytics_Cluster.xlsx", sheet = "HR_Analytics_Cluster")
View(HR)

#Criação modelo de risco único
modelo<-glm(left ~ . , family = binomial (link='logit'), data = HR[,2:11])
summary(modelo)
HR_pred = predict(modelo, HR, type="response")
HR<-data.frame(HR, HR_pred)
View(HR)
ks.test(HR$HR_pred[HR$left==0],
        HR$HR_pred[HR$left==1])

#Modelos específicos para cada um dos 3 clusters

HR_cluster_I<-subset(HR, HR$QCL_1==1)
HR_cluster_II<-subset(HR, HR$QCL_1==2)
HR_cluster_III<-subset(HR, HR$QCL_1==3)

modelo_cluster_I<-glm(left ~ . , family = binomial (link='logit'), data = HR_cluster_I[,2:11])
modelo_cluster_II<-glm(left ~ . , family = binomial (link='logit'), data = HR_cluster_II[,2:11])
modelo_cluster_III<-glm(left ~ . , family = binomial (link='logit'), data = HR_cluster_III[,2:11])

HR_pred_cluster_I<-predict(modelo_cluster_I, HR_cluster_I, type = "response")
HR_cluster_I<-data.frame(HR_cluster_I,HR_pred_cluster_I )
View(HR_cluster_I)
str(HR_cluster_I)

HR_pred_cluster_II<-predict(modelo_cluster_II, HR_cluster_II, type="response")
HR_cluster_II<-data.frame(HR_cluster_II,HR_pred_cluster_II )
View(HR_cluster_II)

HR_pred_cluster_III<-predict(modelo_cluster_III, HR_cluster_III, type="response")
HR_cluster_III<-data.frame(HR_cluster_III,HR_pred_cluster_III )
View(HR_cluster_III)

#K-S modelo único
ks.test(HR$HR_pred[HR$left==0],
        HR$HR_pred[HR$left==1])

#K-S modelo cluster I
ks.test(HR_cluster_I$HR_pred_cluster_I.1[HR_cluster_I$left==0],
        HR_cluster_I$HR_pred_cluster_I.1[HR_cluster_I$left==1])

#K-S modelo cluster II
ks.test(HR_cluster_II$HR_pred_cluster_II[HR_cluster_II$left==0],
        HR_cluster_II$HR_pred_cluster_II[HR_cluster_II$left==1])

#K-S modelo cluster III
ks.test(HR_cluster_III$HR_pred_cluster_III[HR_cluster_III$left==0],
        HR_cluster_III$HR_pred_cluster_III[HR_cluster_III$left==1])

