idade <- rnorm(100, 45, 10)
risco <- idade*20 - idade^2*0.2 + rnorm(100, 0, 25)

plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco")

fit <- lm(risco~idade)
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Ajuste Linear")
abline(a = fit$coefficients[1], b=fit$coefficients[2], col='red', lwd=3)

fit <- lm(risco~idade+I(idade^2))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Ajuste Quadrático")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 


fit <- lm(risco~idade+I(idade^2)+I(idade^3)+I(idade^4)+I(idade^5)+I(idade^6)+I(idade^7)+I(idade^8)+I(idade^9)+I(idade^10))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Grau 10")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 

idade_menor30 <- idade<=30
idade_30_40 <- idade<=40 & idade>30
idade_40_50 <- idade<=50 & idade>40
idade_50_60 <- idade<=60 & idade>50
idade_60 <- idade>60

fit <- lm(risco~idade_menor30+idade_30_40+idade_40_50+idade_50_60+idade_60)
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Step Function")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 

library(splines)
fit <- lm(risco~bs(idade,knots=40,degree=1))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Linear Spline - Knot 40")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 


fit <- lm(risco~bs(idade,knots=c(35,50),degree=1))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Linear Spline - Knots 35, 50")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 


fit <- lm(risco~bs(idade,knots=40,degree=3))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Cubic Spline - Knot 40")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 


fit <- lm(risco~bs(idade,knots=c(35,50),degree=3))
plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Cubic Spline - Knots 35, 50")
lines(sort(idade), fitted(fit)[order(idade)], col='red', type='l', lwd=3) 


plot(idade, risco, pch=16, xlab="Idade", ylab="Score de risco", main="Smoothing spline")
f <- smooth.spline(idade, risco, all.knots = T)
lines(f, col='red', type='l', lwd=3) 

