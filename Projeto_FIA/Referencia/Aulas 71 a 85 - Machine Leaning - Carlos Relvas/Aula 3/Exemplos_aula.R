
setwd('C:/Users/DATA MINING T12/Desktop/Data Mining T12\Aulas 71 a 85 - Machine Leaning - Carlos Relvas\Aula 3')
library(MASS)
data(Boston)
df <- Boston

library(car)
scatterplotMatrix(~ medv + crim + rm + age + dis + black, 
                  data=df, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)

set.seed(42)
id_train <- sample(1:nrow(df), nrow(df)*0.70)
df_train <- df[id_train,]
df_other <- df[-id_train,]
id_val <- sample(rownames(df_other), nrow(df_other)*0.5)
df_val <- df_other[id_val,]
df_test <- df_other[setdiff(rownames(df_other),id_val),]

fit_lin <- lm(medv ~ crim + rm + age + dis + black, data=df_train)
pred_test <- predict(fit_lin, newdata = df_test)
MAE_lin <- sum(abs(df_test$medv-pred_test))/nrow(df_test)

## Polinomial regression

fit_pol <- lm(medv ~ crim + I(crim^2) + rm + I(rm^2) + age + I(age^2) +
                dis + I(dis^2) + black + I(black^2), data=df_train)
pred_test <- predict(fit_pol, newdata = df_test)
MAE_pol <- sum(abs(df_test$medv-pred_test))/nrow(df_test)

## Step Functions

for(i in c("crim", "rm", "age", "dis", "black")){
  percentis = quantile(df_train[,i], probs = c(0.2, 0.4, 0.6, 0.8))
  df_train[,paste0(i, "_cat")] = cut(df_train[,i], c(-Inf,percentis, Inf))
  df_val[,paste0(i, "_cat")] = cut(df_val[,i], c(-Inf,percentis, Inf))
  df_test[,paste0(i, "_cat")] = cut(df_test[,i], c(-Inf,percentis, Inf))
}

fit_categ <- lm(medv ~ as.factor(crim_cat) + as.factor(rm_cat) + 
                  as.factor(age_cat) + as.factor(dis_cat) + as.factor(black_cat),
                data=df_train)
pred_test <- predict(fit_categ, newdata = df_test)
MAE_categ <- sum(abs(df_test$medv-pred_test))/nrow(df_test)


## Splines 

scatterplotMatrix(~ medv + crim + rm + age + dis + black, 
                  data=df, smooth=T,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)

library(splines)

fit_lin <- lm(medv~rm, data=df_train)
pred_test <- predict(fit_lin, newdata = df_test)
MAE_lin_ <- sum(abs(df_test$medv-pred_test))/nrow(df_test)

fit_spline <- lm(medv~bs(rm,,degree=3), data=df_train)
pred_test <- predict(fit_spline, newdata = df_test)
MAE_spline <- sum(abs(df_test$medv-pred_test))/nrow(df_test)


## GAM

install.packages("gam")
library(gam)

fit_gam <- gam(medv ~ s(crim,4) + s(rm,4) + s(age,4) + dis + s(black,3),
               data=df_train)
pred_test <- predict(fit_gam, newdata = df_test)
MAE_gam <- sum(abs(df_test$medv-pred_test))/nrow(df_test)

plot.gam(fit_gam)


fit_gam <- gam(medv ~ s(crim,4) + s(rm,4) + s(age,4) + s(dis, 4) + s(black, 4),
               data=df_train)
pred_test <- predict(fit_gam, newdata = df_test)
MAE_gam <- sum(abs(df_test$medv-pred_test))/nrow(df_test)

plot(fit_gam)
