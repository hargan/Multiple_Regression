ad=read.csv('Advertising.csv')
head(ad)
tv=ad$TV
radio=ad$Radio
newspaper=ad$Newspaper
sales=ad$Sales

plot(tv,sales,cex.lab=2,cex.axis=1.2)
plot(radio,sales,cex.lab=2,cex.axis=1.2)
title("Advertising data",cex.main = 2,font.main= 4, col.main= "blue")
plot(newspaper,sales,cex.lab=2,cex.axis=1.2)

#simple Linear Regression

lm.radio=lm(sales~radio)
lm.tv=lm(sales~tv)
lm.newspaper=lm(sales~newspaper)

par ( mfrow =c(1 ,3) )
plot(tv,sales,cex.lab=2,cex.axis=1.2)
abline(lm.tv, col="blue", lty=1, lwd=2)

plot(radio,sales,cex.lab=2,cex.axis=1.2)
abline(lm.radio, col="blue", lty=1, lwd=2)

plot(newspaper,sales,cex.lab=2,cex.axis=1.2)
abline(lm.newspaper, col="blue", lty=1, lwd=2)

summary(lm.tv)
confint (lm.tv )

#Multiple Linear Regression

lm.full=lm(sales~tv+radio+newspaper)
summary(lm.full)
confint (lm.full)
plot(lm.full)

cor(ad)

predict (lm(sales~tv+radio+newspaper),data.frame(tv=100,radio=20,newspaper=50),interval ="prediction")
