#continue....

#install required packages
install.packages("UsingR")
install.packages("psych")
install.packages("car")
install.packages("chemometrics")
library(chemometrics)
library(car)
library(psych)
library(ggplot2)
library(DescTools)
library(corrplot)
library(ggpubr)
library(cluster)
library(MASS)
library(plyr)
library(nortest)
library(sandwich)
library(lmtest)
library(robustbase)


#CASE 3. We use dataset with all outliers to regress them

#initial data analysis//////////////////////////////////////////////////
setwd("C:/Users/User/Desktop")

data <- read.csv("Diabetes.csv")

data <- data.frame(data[,-1], row.names = data[,1])#id

colnames(data)
nrow(data) <- data.frame(data[, c(2,4,5,10,18)])#frame, time, bp2s, not needed


data<-data[complete.cases(data), ]

nrow(data)#384 patients

colnames(data)
#Normality assumption of dependent variable
par(mfrow=c(1, 1))
hist((data$chol), xlab = "Chol", col = "darkorange", freq = F) 
lines(density(data$chol), type="l", col="blue", lwd = 3)
#by histogram we see that distribution has long tails, this is
#because of outliers

qqnorm(data$chol, col = "darkorange") 
qqline(data$chol, col = "darkblue", lwd = 2)

lillie.test(data2$chol)#pvalue is lower 0.05- not normal
#target variable has not normal distribution, therefore we will
#make transformation

#check for multicollinearity by correlation matrix
corrplot.mixed(cor(data), tl.col="black")
#in this case only stab.glu with glyhb and weight with waist,
#weight with hip, waist with hip has high 
#collinearity, so we can use only on of pair for building model

##Lets check the model.
##lets include all variables into model and check for multicollinearity
##Backward elimination
summary(ols3<-lm(data$chol ~ ., data=data))
vif(ols3)#based on vif only weight, waist and hip has high collinearity
#if we use them at the same time
plot(ols3)

shapiro.test(studres(ols3))
#homoscedasticity of residuals assumptions meet, p>0.05
bptest(ols3)#Breusch-Pagan
ncvTest(ols3)# non-constant error variance test
#lets make F test of significance
anova(ols3)#all p values are less than 0.05, so they significant


#cross validation:
set.seed(777)
ind<-sample(2,nrow(data),replace = T, prob = c(0.8,0.2))
train3<-data[ind==1,]
test3<-data[ind==2,]

results3<-lm(train3$chol~., data=train3)
summary(results3)#all coef are significant

pred3<-predict(results3,test3, interval = "predict")
head(round(pred3))
head(cbind(test3$chol,round(pred3)))
b=as.data.frame(cbind(test3$chol,round(pred3)))

colnames(b)[1]<-"actual"

pred3<-predict(results3,test3)

#ACCURACY TEST
library(broom)
R2 <- function (x, y) cor(x, y)^2
#first model
data.frame( R2 = R2(pred3, test3$chol),
            RMSE = RMSE(pred3, test3$chol),
            MSE=MSE(pred3, test3$chol),
            MAE = MAE(pred3, test3$chol),
            MAPE = MAPE(pred3,test3$chol),
            AIC=AIC(results3),
            BIC=BIC(results3))
glance(results3)
#we see the model is not so good

#CASE 4: Remove only influential outliers(Cooks distance, Mahalanobis)

#we again use the same model, however we will try to remove influential
#points
ols4<-lm(new$chol~., data=new)
#Cook distance
plot(ols4,4)
plot(ols4,5)

new<-data[-c(62,46,227),]
data[46,]
data[227,]
#Cook distance

cooksd <- cooks.distance(ols4)
plot(cooksd, pch="*", cex=2, main="Influential Patients by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
#now we can see cooks influential observations and list them:
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
View(influential)
which(rownames(new) %in% influential) 
new[as.character(influential),]#list of observations, that has influential
#value in at least one column
data.cook<-new[-which(rownames(new) %in% influential),]
nrow(new)
nrow(data.cook)
#cook ols
summary(ols4<-lm(data$chol~., data=data))
plot(ols4)

shapiro.test(studres(ols4))

#cross validation:
set.seed(777)
ind<-sample(2,nrow(data.cook),replace = T, prob = c(0.8,0.2))
train4<-data.cook[ind==1,]
test4<-data.cook[ind==2,]



results4<-lm(train4$chol~., data=train4)
summary(results4)#all coef are significant


pred4<-predict(results4,test4, interval = "predict")
head(round(pred4))
head(cbind(test4$chol,round(pred4)))
b=as.data.frame(cbind(test4$chol,round(pred4)))

colnames(b)[1]<-"actual"

pred4<-predict(results4,test4)

#ACCURACY TEST
library(broom)
R2 <- function (x, y) cor(x, y)^2
#first model
data.frame( R2 = R2(pred4, test4$chol),
            RMSE = RMSE(pred4, test4$chol),
            MSE=MSE(pred4, test4$chol),
            MAE = MAE(pred4, test4$chol),
            MAPE = MAPE(pred4,test4$chol),
            AIC=AIC(results4),
            BIC=BIC(results4))



#The Studentized residual 
# indicates the extent to which the dependent variable
#takes an atypical value, given the values of the explanatory variables. 
#plot(ols4$fitted.values,studres(ols4))

n=nrow(data)
p=length(ols4$coefficients)

plot(student<-rstudent(ols4))
abline(h=-qt(1 - 0.05/(2*n), n - p - 1),col="red")
abline(h=qt(1 - 0.05/2, n - p - 1),col="green")
abline(h=-qt(1 - 0.05/2, n - p - 1),col="green")
library(olsrr)
install.packages("olsrr")
ols_plot_resid_stud(ols4)


## with Bonferroni correction (preferred) (upper)
which(student > qt(1 - 0.05/(2 * n), n - p - 1))
# or without Bonferroni correction  (Upper)
which(student > qt(1 - 0.05/2, n - p - 1))
#lower
which(student < -qt(1 - 0.05/2, n - p - 1))
#lower prefered
which(student <  - qt(1 - 0.05/(2 * n), n - p - 1))


new3<-data[-c(60,106,145,209,249,277,321,341,353,371,379,391,2,   4,  26,  46, 
             62,  68, 127, 174, 199, 201, 227, 247, 395),]
new3<-data[c(2,   4,  26,  46,  62,  68, 127, 174, 199, 201, 227, 247, 395),]
summary(ols4<-lm(new3$chol~., data=new3))
plot(ols4)

shapiro.test(studres(ols4))
set.seed(777)
ind<-sample(2,nrow(new2),replace = T, prob = c(0.8,0.2))
train4<-new2[ind==1,]
test4<-new2[ind==2,]


results4<-lm(train4$chol~., data=train4)
summary(results4)#all coef are significant


pred4<-predict(results4,test4, interval = "predict")
head(round(pred4))
head(cbind(test4$chol,round(pred4)))
b=as.data.frame(cbind(test4$chol,round(pred4)))

colnames(b)[1]<-"actual"

pred4<-predict(results4,test4)

#ACCURACY TEST
library(broom)
R2 <- function (x, y) cor(x, y)^2
#first model
data.frame( R2 = R2(pred4, test4$chol),
            RMSE = RMSE(pred4, test4$chol),
            MSE=MSE(pred4, test4$chol),
            MAE = MAE(pred4, test4$chol),
            MAPE = MAPE(pred4,test4$chol),
            AIC=AIC(results4),
            BIC=BIC(results4))
#/////////////////////////////////////////////////


student.data<-new3
#cross validation:
set.seed(777)
ind<-sample(2,nrow(student.data),replace = T, prob = c(0.8,0.2))
train4<-student.data[ind==1,]
test4<-student.data[ind==2,]


results4<-lm(train4$chol~., data=train4)
summary(results4)#all coef are significant

pred4<-predict(results4,test4, interval = "predict")
head(round(pred4))
head(cbind(test4$chol,round(pred4)))
b=as.data.frame(cbind(test4$chol,round(pred4)))

colnames(b)[1]<-"actual"

pred4<-predict(results4,test4)

data.frame( R2 = R2(pred4, test4$chol),
            RMSE = RMSE(pred4, test4$chol),
            MSE=MSE(pred4, test4$chol),
            MAE = MAE(pred4, test4$chol),
            MAPE = MAPE(pred4,test4$chol),
            AIC=AIC(results4),
            BIC=BIC(results4))


# which observations 'are' influential
infl <- influence.measures(ols4)
data2[-which(apply(infl$is.inf, 1, any)),]

################MAHAL

md.1 <- Moutlier(data, quantile = 0.99, plot = FALSE)
md.1$cutoff
which(md.1$md > md.1$cutoff)

plot(md.1$md)
abline(h=md.1$cutoff,col="red")
qqplot(md.1$md, data2$chol, plot.it = TRUE, xlab = "Mahalanobis' distance",
       ylab = "chol", main = "DF.2")
abline(v=md.1$cutoff,col="red")

new4<-data[-c(4,  46,  62,  68, 116, 131, 200, 227, 247, 280, 290, 371, 374, 390, 395),]

nrow(new4)

summary(ols4<-lm(new4$chol~., data=new4))


set.seed(777)
ind<-sample(2,nrow(new4),replace = T, prob = c(0.8,0.2))
train4<-new4[ind==1,]
test4<-new4[ind==2,]


results4<-lm(train4$chol~., data=train4)
summary(results4)#all coef are significant

pred4<-predict(results4,test4, interval = "predict")
head(round(pred4))
head(cbind(test4$chol,round(pred4)))
b=as.data.frame(cbind(test4$chol,round(pred4)))

colnames(b)[1]<-"actual"

pred4<-predict(results4,test4)

data.frame( R2 = R2(pred4, test4$chol),
            RMSE = RMSE(pred4, test4$chol),
            MSE=MSE(pred4, test4$chol),
            MAE = MAE(pred4, test4$chol),
            MAPE = MAPE(pred4,test4$chol),
            AIC=AIC(results4),
            BIC=BIC(results4))






shapiro.test(studres(ols4))

bptest(ols4)#Breusch-Pagan
ncvTest(ols4)

qqnorm(train4$chol)

#vibrat vector
#which(rownames(data2) %in% influential)

