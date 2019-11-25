library(datasets)
data(airquality)
df <- airquality

df <- na.omit(df)

library(car)
scatterplotMatrix(~ Ozone+Solar.R+Wind+Temp, 
                  data=df, smooth=FALSE,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)

set.seed(42)
id_train <- sample(1:nrow(df), nrow(df)*0.70)
df_train <- df[id_train,]
df_test <- df[-id_train,]


fit_lin <- lm(Ozone~Solar.R+Wind+Temp, data=df_train)
pred_test <- predict(fit_lin, newdata = df_test)
MAE_lin <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)

## Polinomial regression

fit_pol <- lm(Ozone ~ Solar.R + I(Solar.R^2) + Wind + I(Wind^2) + Temp + I(Temp^2)
              , data=df_train)
pred_test <- predict(fit_pol, newdata = df_test)
MAE_pol <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)

## Step Functions

for(i in c("Solar.R","Wind", "Temp")){
  percentis = quantile(df_train[,i], probs = c(0.2, 0.4, 0.6, 0.8))
  df_train[,paste0(i, "_cat")] = cut(df_train[,i], c(-Inf,percentis, Inf))
  df_test[,paste0(i, "_cat")] = cut(df_test[,i], c(-Inf,percentis, Inf))
}

fit_categ <- lm(Ozone ~ as.factor(Solar.R_cat) + as.factor(Wind_cat) + 
                  as.factor(Temp_cat),
                data=df_train)
pred_test <- predict(fit_categ, newdata = df_test)
MAE_categ <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)


## Splines 

scatterplotMatrix(~ Ozone+Solar.R+Wind+Temp, 
                  data=df, smooth=T,
                  reg.line=FALSE, ellipse=FALSE,
                  diagonal="none", pch=16)

library(splines)

fit_lin <- lm(Ozone~Wind, data=df_train)
pred_test <- predict(fit_lin, newdata = df_test)
MAE_lin_ <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)

fit_spline <- lm(Ozone~bs(Wind,degree=3), data=df_train)
pred_test <- predict(fit_spline, newdata = df_test)
MAE_spline <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)


## GAM

install.packages("gam")
library(gam)

fit_gam <- gam(Ozone ~ s(Wind,4) + s(Solar.R,4) + s(Temp,4),
               data=df_train)
pred_test <- predict(fit_gam, newdata = df_test)
MAE_gam <- sum(abs(df_test$Ozone-pred_test))/nrow(df_test)

plot(fit_gam)

