#second part of DIPLOM

which(rownames(nooutlier)==130)
nrow(nooutlier)
length(nooutlier$y3)

nrow(nooutlier[-c(87,178,265,19,195,237,259,98,2),])


boxplot(data$location, data$chol)
boxplot(data$gender,data$chol)
nrow(data)

boxplot(data$bp.1s)

#dlya togo chtob sravnyat razmer  peremennix(chislo nablyudeniy)
shortest <- min(length(x1), length(x2),length(x3),length(x4),length(x5), 
                length(x6),length(x7),length(x8),length(x9),length(x10),
                length(x11),length(x12))
x1 <- tail(x1, shortest)
x2 <- tail(x2, shortest)
x3 <- tail(x3, shortest)
x4 <- tail(x4, shortest)
x5 <- tail(x5, shortest)
x6 <- tail(x6, shortest)
x7 <- tail(x7, shortest)
x8 <- tail(x8, shortest)
x9 <- tail(x9, shortest)
x10 <- tail(x10, shortest)
x11 <- tail(x11, shortest)
x12 <-tail(x12,shortest)
x13 <-tail(x13,shortest)


nrow(xyz)


x1=remove_outliers(x1)
x2=remove_outliers(x2)
x3=remove_outliers(x3)
x4=remove_outliers(x4)
x5=remove_outliers(x5)
x6=remove_outliers(x6)
x7=remove_outliers(x7)
x8=remove_outliers(x8)
x9=remove_outliers(x9)
x10=remove_outliers(x10)
x11=remove_outliers(x11)
x12=remove_outliers(x12)

xyz<-data.frame(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13)

xyz<-xyz[complete.cases(xyz), ]
#/////////////////////////////////////////////////////////









detect(data[,1])


#check distributions
#graphical overview

par(mfrow=c(1, 1))
hist((log(data$chol)), xlab = "Glycosolated Hemoglobin", col = "darkorange", freq = F) 
lines(density(log(data$chol)), type="l", col="blue", lwd = 3)





#outliers
#For a given continuous variable, outliers are those observations that lie outside 1.5 * IQR, where IQR, the 'Inter Quartile Range' is the difference between 75th 
#and 25th quartiles. Look at the points outside the whiskers in below box plot.
#extreme of Y is outlier, extreme of X is leverage

#Univariate

#boxplot
boxplot(data$glyhb)
length(data$glyhb[which(data$glyhb %in% boxplot.stats(data$glyhb)$out)])#51 outliers
abcd=data[-which(data$glyhb %in% boxplot.stats(data$glyhb)$out),]#315 without out



#ANALYSIS ONLY FOR DEPENDENT VARIABLE without outlier(ind may have outlier)
data1<-data[which(x3 %in% data$stab.glu),]# data for glyhb without outlier
data1<-data1[complete.cases(data1),]
data1<-data1[,-1]
colnames(data1)
#data1<-data[which(xyz$x13 %in% data$id),]#reduced dependent variable from outliers

#Regression
summary(ols<-lm(data1$chol ~ ., data=data1[,c(1,2,3,5,7,14)]))

summary(ols<-lm(log(data1$chol) ~ ., data=data1[,c(1,2,3,5,7,14)]))

summary(ols2<-lm(log(data$chol) ~ ., data=data[,-c(1,5,9,12,13)]))

#/////////////////////////////////////////////

#######Robust regression///////////////////////////////////////////
library(robustbase)
library(MASS)

summary(rob<-rlm(data$chol ~ ., data=data[,c(3,4,6,8,15)]))

summary(l<-lm(data1$chol ~ ., data=data1[,c(1,2,3,5,7,14)]))

#2 weights
#Huber weights
summary(rob<-rlm(data$chol ~ ., data=data[,c(3,4,6,8,15)]))


#/////////////////////////////////////////////////////////////////////



plot(rob)
plot(l)

colnames(data)

hist(log(x))

qqnorm(x)
qqline(x)






#multicollinearity check
#correlogram:if higher than 0.8 can be multicollinearity 
# vif(mymodel) -variance inflation factor, if >10 then X has high collinearity


#out
par(mfrow=c(1, 2))
plot(data$chol, abcd, xlim=c(0, 500), ylim=c(0, 20), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(x ~ chol, data=data), col="blue", lwd=3, lty=2)

par(mfrow=c(1, 2))
plot(y, a, xlim=c(100, 300), ylim=c(0, 10), main="With Outliers", xlab="speed", ylab="dist", pch="*", col="red", cex=2)
abline(lm(a ~ y, data=ay), col="blue", lwd=3, lty=2)




#Regression
summary(ols<-lm(log(abc$c) ~ ., data=abc))
summary(ols2<-lm(log(data$glyhb) ~ ., data=data[,-c(8,11)]))


termplot(ols2)


#homogeneity residual diagnostics(scal location graph)
plot(ols)

#homosgeneity test
ncvTest(ols)# p < .05, suggesting that our data is not homoscedastic.


#plots 
opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols2, las = 1)
##############

abc=abc[-c(74,228,21),]

which(rownames(abc)==31)


#another way to check outliers
#model.metrics %>% 
#filter(abs(.std.resid) > 3) %>%
#  as.data.frame()



nrow(abc)






boxplot(h)
boxplot(a)

a<-x$glyhb
cor(a,y)



length(y)

boxplot(x$stab.glu)





data[which(data$glyhb > (mean(data$glyhb, na.rm = T) + 3*sd(data$glyhb))),5,drop=F]


boxplot(data)



install.packages("car")
library(car)
dataEllipse(data$glyhb,data$chol, levels=c(0.5, 0.975))

dataEllipse()

#practice
boxplot(data$glyhb)

qqnorm(log(data$glyhb))
qqline(log(data$glyhb))

qqnorm(log(x))
qqline(log(x))


hist(y)
qqnorm(log(y))
qqline(log(y))

boxplot.stats(x)$out
length(y)

y=x[-which(x%in% boxplot.stats(x)$out)]

boxplot(y)
x=data[-which(data$glyhb %in% boxplot.stats(data$glyhb)$out),5]

#to see one column name and ids
data[,5, drop=FALSE]
#//////////////////////////////

























#------------------
install.packages("ggridges")

library(ggplot2)
library(ggridges)


par(mfrow=c(1,1))
ggplot(data)








hist(data$chol)
plot(density(na.omit(data$glyhb[boxplot.stats(data$glyhb)$out])))

boxplot(data$glyhb)

(boxplot.stats(data$glyhb)$out)









hist(data[-which(data$glyhb %in% boxplot.stats(data$glyhb)$out),6])######
plot(density(na.omit(data[-which(data$glyhb %in% boxplot.stats(data$glyhb)$out),6])))

boxplot(na.omit(data[-which(data$glyhb %in% boxplot.stats(data$glyhb)$out),6]))

data$glyhb[data$glyhb %in% boxplot.stats(data$glyhb)$out]


boxplot.stats(data$glyhb)$out

data$glyhb[boxplot.stats(data$glyhb)$out]

summary(data$glyhb)

!is.na(data$chol)



#check if outlier is influential, leverage effect

boxplot(data$glyhb ~ data$location, main = "", col = "orange")



#EDA
#multicollinearity
#homogeneity
#residuals
#lasso
#ridge
#Model Qualiyu
#Prediction

#plan: no outlier, no extreme values, with outlier, with robust regr
