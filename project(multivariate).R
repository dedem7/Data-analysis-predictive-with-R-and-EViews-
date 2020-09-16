#start
#install required packages
install.packages("car")
install.packages("wooldridge")
library("wooldridge")
library(car)
library(psych)
library(ggplot2)
library(DescTools)
library(corrplot)
library(ggpubr)
library(cluster)
library(MASS)
library(plyr)


setwd("C:/Users/User/Desktop/UNIVER/Multivariate st/datasets")
#------------------data preparation------------------------

?wage2
#delete lwage column
data<-wage2[,-17] 
#make a sample from data(population)(500 random observations)
#data<-data[sample(nrow(data),500), ]
#put in increasing order(not necessary)
#(data[order(as.numeric(rownames(data))),])

#descriptive statistics
head(data)

?wage2
View(data)
nrow(data)
ncol(data)
colnames(data)
str(data)


summary(data)

#####-----------------------work with NAs

#loop to detect which col has at least one NA
i=0
kd=c()
for(i in (1:ncol(data))){
  if(sum(is.na(data[,i]))>=1){
    print(c(colnames(data[i])))
  }
}

#check for NA
is.na(data)
which(is.na(data))
sum(is.na(data))




#delete all NAs
data<-data[complete.cases(data), ]
#reduced nrows from 935 to 663 after NAs deletion
nrow(data)


####------------------Outliers

#3sigma rule to check extreme functions
which(data$wage > (mean(data$wage, na.rm = T) + 3*sd(data$wage, na.rm = T)))
data$wage[data$wage > (mean(data$wage, na.rm = T) + 3*sd(data$wage, na.rm = T))]
which(data$inc86 < (mean(data$inc86, na.rm = T)- 3*sd(data$inc86, na.rm = T))) # none found

#check for outlier
boxplot(data$wage, data = data, col = rainbow(3))
boxplot(wage2$hours,id=list(n=Inf))


which(data$wage %in% boxplot(data$wage)$out)
boxplot(data$wage)$out



#delete rows of data with extreme values in wage
data<-data[-which(data$wage > (mean(data$wage, na.rm = T) + 3*sd(data$wage, na.rm = T))),]


# boxplot
boxplot(data$wage, data = data, col = rainbow(3))
boxplot(wage2$hours,id=list(n=Inf))



#---------------------Normality and homoscedasticity

#check for normality by histogramm(4 basic variables)

par(mfrow=c(2,2))

m <- mean(data$hours, na.rm = T)
std <- sd(data$hours, na.rm = T) 
hist(data$hours, prob=TRUE, col = "green")
curve(dnorm(x, mean=m, sd=std), col="brown", lwd=2, add=TRUE, yaxt="n")

m <- mean(data$wage, na.rm = T)
std <- sd(data$wage, na.rm = T) 
hist(data$wage, prob=TRUE, col = "green")
curve(dnorm(x, mean=m, sd=std), col="brown", lwd=2, add=TRUE, yaxt="n")

m <- mean(data$IQ, na.rm = T)
std <- sd(data$IQ, na.rm = T) 
hist(data$IQ, prob=TRUE, col = "green")
curve(dnorm(x, mean=m, sd=std), col="brown", lwd=2, add=TRUE, yaxt="n")

m <- mean(data$KWW, na.rm = T)
std <- sd(data$KWW, na.rm = T) 
hist(data$KWW, prob=TRUE, col = "green")
curve(dnorm(x, mean=m, sd=std), col="brown", lwd=2, add=TRUE, yaxt="n")

#graph shows normal distribution


#QQ plot(4 important variables)

par(mfrow=c(2,2))

qqnorm(data$wage, col = "brown")
qqline(data$wage, col = "green", lwd = 2)

qqnorm(data$IQ, col = "brown")
qqline(data$IQ, col = "green", lwd = 2)
# QQ shows almost normal distribution

#parametric tests for normality
shapiro.test(data$wage)
LillieTest(data$IQ)
#both p value less than 0,05  reject H0 hypothesis, that means non-normality


#transformation to normality
data$ln_wage <- log(data$wage)
shapiro.test(data$ln_wage)
data$ln_iq<-log(data$IQ)
LillieTest(data$ln_iq)



#check for homoscedastisity(if variance are equal)
#using boxplot

par(mfrow=c(1,1))
factor.age<- as.factor(cut(data$age, breaks = 2))

boxplot(data$wage~factor.age, main = "",  col = "darkorange")
bartlett.test(data$wage ~ factor.age)
#p >0.05 is homoscedastic as on graph (variances are equal)





##--------------DEPENDENCY ANALYSIS AND REGRESSION

##categorical variables(dependence analysis)
##chi square test

#choose categorical variables
category<-data[,9:12]
head(data)

#factor used for analysis
cut.wage<-as.factor(cut(data$wage, breaks = 3))
cut.iq<-as.factor(cut(data$IQ, breaks = 3))


#check dependency between married and urban categories

chisq.test(table(category$urban,cut.iq),correct = T)
#p value is higher 0.05, so we do not H0, are independent
chisq.test(table(category$black,cut.wage),correct = T)
#p value is less 0.05, so we reject H0
#that means color is dependent on wage

#probability(goodness of fit)
#expect value means that 3 categories has equal distribution(freq values)
CHI<-chisq.test(table(cut.wage))
plot(cut.wage)
attributes(CHI)
CHI$p.value<0.05
#reject the null hypothesis that categories of cut.wage are equally distributed

#check the strength of dependies
ContCoef(table(cut.wage,category$black),correct = T)
CramerV(table(cut.wage,category$black))
#medium dependence assosiation 

#ANOVA

tapply(data$wage, data$black, mean)#calculate mean for each category(black and non black)
tapply(data$wage, data$black, var) #calculate variance for each category(black and non black)

fit <- aov(data$wage ~ data$black,data=data) 
summary(fit)
#p value is less than 5% there is a statistical differenc in groups(means are not equal:black and non black)

#to see the difference of means



eta_sq(fit)#2.6 percent of variance assosiated by black variable

#Correlation
#table of correlation coefficients
corr <- round(cor(data[ ,1:8], use = "pairwise.complete.obs"),3)
corr
corrplot.mixed(corr, upper = "ellipse")


shapiro.test(data$meduc)#not normal dist, because p < 0.05 reject H0 of normality
shapiro.test(data$feduc)#the same as for meduc

#if we assume not normality, then spearmon coefficient will be used
cor.test(data$meduc,data$feduc, use = "complete.obs", method = "spearman")
#0.57 is a strong correlation between meduc and feduc


#----------------------------NEW---------------------------
#the goal is to classify and predict wages using pca 
#as well as make prediction by clusters using regression model
#divide on below average and higher average
data$cut.wage1<-as.factor(cut(data$wage, breaks=c(0, mean(data$wage), max(data$wage)), include.lowest=TRUE))
#revalue to higher and lower wage
data$cut.wage1<-revalue(data$cut.wage1,c("(969,2.16e+03]"="higher than average","[0,969]"="lower than average"))
data$cut.wage<-cut.wage



plot(data$cut.wage1,ylim = c(0,500))#barplot of 2 types of wage

#number of lower average
length(data$wage)-sum(data$wage>=mean(data$wage))
#number of higher average
sum(data$wage>=mean(data$wage))



tail(with(data, data[order(cut.wage1),]))



set.seed(7777)
train<-sample(nrow(data),nrow(data)*0.7)


data.train<-data[train,]

#[,c(1,3,4,5,6,7,8,19)]
data.test<-data[-train,]
#[,c(1,3,4,5,6,7,8,19)]
nrow(data.train)

colnames(data)
pairs.panels(data.train[,c(1,3,4,5,6,8)], hist.col = "#00AFBB",density = TRUE)

?wage2



####PCA Principal Component(Data reduction)

###Lets make correlation matrix
cor.matrix<-subset(data[,-c(1,2,9,10,11,12,17,18,19)])
(cor(cor.matrix))


##plot correlation matrix
corrplot(cor(cor.matrix))


#create principal components
data.pca <- prcomp(data.train[,-c(9,10,11,12,13,14,17,18,19,20)], center = T, scale. = T)
summary(data.pca)
#shows output of all components
colnames(data.train)
#lets plot a scree plot
plot(data.pca, type = "l", ylim = c(0,4), main = "")
abline(a=1, b=0, type = "l", lty = 2, col = "dark orange")
#scree plot shows that 4 components can be used in general

head(data)

data.pca$rotation[,1:3]#component loadings
data.pca$x[ ,1 :3] #component values(scores)

pairs.panels(data.pca$x,gap=0,pch=21)
library(nnet)

trg<-predict(data.pca,data.train)
trg<-data.frame(trg,data.train[19])

tst<-predict(data.pca,data.test)
tst<-data.frame(tst,data.test[19])

#mymodel<-glm(cut.wage1~PC1+PC2+PC3,data=trg,family="binomial")
mymodel<-multinom(cut.wage1~PC1+PC2+PC3,data=trg)

p1<-predict(mymodel,trg, type = "response")#probabilities that it is higher than average

head(p1)

pred1<-ifelse(p1>0.5,"higher than average","lower than average")

tab<-table(pred1,trg$cut.wage1)[1:2,2:1]

summary(mymodel)
1-sum(diag(tab))/sum(tab)
#----test data

p2<-predict(mymodel,tst, type = "response")#probabilities that it is higher than average

head(p1)

pred2<-ifelse(p2>0.5,"higher than average","lower than average")

tab2<-table(pred2,tst$cut.wage1)[1:2,2:1]
1-sum(diag(tab2))/sum(tab2)




set.seed(180806)

library(randomForest)

rf.fit<-randomForest(cut.wage1~IQ+KWW+educ+exper+tenure+age+married+black+south+urban+sibs+brthord+meduc+feduc,data=data.train,ntree=5000)

trg$black<-revalue(black,1="yes",0="no")


length(trg$cut.wage)

lda.fit<-lda(cut.wage1~IQ+KWW+educ+exper+tenure+age+married+black+south+urban+sibs+brthord+meduc+feduc,data=data.train)
lda.predict<-predict(lda.fit,data.test)
(lda.prob <- head(lda.predict$posterior))
lda.class <- lda.predict$class

table(lda.class,data.test$cut.wage1)

mean(data.train$cut.wage1 == lda.class)

clu_sum(data.test, lda.class)
clu_sum(data.test, data.test$cut.wage1)


glm.fit<-glm(black~PC1+PC2,data=trg,family = "binomial")
glm.prob<-predict(glm.fit,trg,type="response")
colnames(data.train)
p<-predict(glm.fit,trg)
tab<-table(p,data.train$black)


mod<-multinom(black~PC1+PC2,data=trg)
p<-predict(mod,trg)
table(p,trg$black)
mean(data.train$cut.wage == p)

sum(trg$black==0)

?wage2

table(glm.prob,data.test$cut.wage1)
colnames(data)
#points of first two components
biplot(data.pca)
#shows pc scores and loadings(very dense)


#Clustering
#---------------------------END(NEW)------------------------
#REGRESSION

simple <- lm(wage ~ IQ, data = data) 

summary(simple)
anova(simple)

#p value less 0.05 reject H0, regression model fits better than model without ind variable
#determination coef( r sq) is 0.11 means that 11% of variation explained by fitted regression line 
#which is not high

# scatter plot with a regression line
plot(wage ~ IQ, data = data)
abline(simple, col = "darkorange", lwd = 2)


plot(simple)
#there is no cook distance that show that some points can affect to model



#lets predict what wage will be for people with higher or lower IQ
prediction <- data.frame(IQ = c(50,53 , 150, 160, 170, 200))
prediction$wage <- predict(simple,prediction, interval = c("confidence"),level = 0.95)
prediction


#to see correlation table
pairs.panels(data[,1:4], hist.col = "#00AFBB",density = TRUE)


####PCA Principal Component(Data reduction)

###Lets make correlation matrix
cor.matrix<-subset(data[,-c(1,2,9,10,11,12,17,18,19)])
(cor(cor.matrix))


##plot correlation matrix
corrplot(cor(cor.matrix))


#create principal components
data.pca <- prcomp(data.train[,-c(2,9,10,11,12,17,18,19,20)], center = T, scale. = T)
summary(data.pca)
#shows output of all components

#lets plot a scree plot
plot(data.pca, type = "l", ylim = c(0,4), main = "")
abline(a=1, b=0, type = "l", lty = 2, col = "dark orange")
#scree plot shows that 4 components can be used in general



data.pca$rotation[ ,1:2] #component loadings
data.pca$x[ ,1:2] #component values(scores)

pairs.panels(data.pca$x,gap=0,pch=21)

trg<-predict(data.pca,data.train)
trg<-data.frame(trg,data.train[10])

tst<-predict(data.pca,data.test)
tst<-data.frame(tst,data.test[10])



set.seed(180806)

library(randomForest)

rf.fit<-randomForest(cut.wage1~IQ+KWW+educ+exper+tenure+age+married+black+south+urban+sibs+brthord+meduc+feduc,data=data.train,ntree=5000)

trg$black<-revalue(black,1="yes",0="no")


length(trg$cut.wage)

lda.fit<-lda(cut.wage1~IQ+KWW+educ+exper+tenure+age+married+black+south+urban+sibs+brthord+meduc+feduc,data=data.train)
lda.predict<-predict(lda.fit,data.test)
(lda.prob <- head(lda.predict$posterior))
lda.class <- lda.predict$class

table(lda.class,data.test$cut.wage1)

mean(data.train$cut.wage1 == lda.class)

clu_sum(data.test, lda.class)
clu_sum(data.test, data.test$cut.wage1)

  
glm.fit<-glm(black~PC1+PC2,data=trg,family = "binomial")
glm.prob<-predict(glm.fit,trg,type="response")
colnames(data.train)
p<-predict(glm.fit,trg)
tab<-table(p,data.train$black)


mod<-multinom(black~PC1+PC2,data=trg)
p<-predict(mod,trg)
table(p,trg$black)
mean(data.train$cut.wage == p)

sum(trg$black==0)

?wage2

table(glm.prob,data.test$cut.wage1)
colnames(data)
#points of first two components
biplot(data.pca)
#shows pc scores and loadings(very dense)


#Clustering

#creating subset with the 1;8 and 12 colums
clus.data<-subset(data[,c(1:8,12)],data$urban==1)
head(clus.data)

?agnes
#make a clusters using agnes by using manhttan
wigh.clus <- agnes(clus.data[ ,1:8],metric = "manhattan", method = "weighted")
ward.clus <- agnes(clus.data[ ,c(1,3,5:8)],metric = "manhattan", method = "ward")	

#dendograms for each method
plot(wigh.clus, main = "")
plot(ward.clus, main = "")
#ward method shows better cluster dendogram with agglomerative coef = 1, which is precise enough

#from dendogram it look that there are 4 clusters
rect.hclust(as.hclust(ward.clus), k = 4, border = "green")

clu_sum <- function(data, groups) { aggregate(data, list(groups), function(x) mean(as.numeric(x))) }
clus.4<- cutree(ward.clus, 4)

#4 clusters with values
clu_sum(clus.data[ ,c(1,3,5:8)], clus.4)[,-1]

#boxplot characterization
par(mfrow=c(1,2))
boxplot(clus.data$wage ~ clus.4, ylab = "Wage", 
        xlab = "Clusters", col = "orange")

boxplot(clus.data$IQ ~ clus.4, ylab = "IQ", 
        xlab = "Clusters", col = "orange")
par(mfrow=c(1,1))
#boxplot shows that wages in different clusters are different, however IQ for all groups not very differs

#plot a cluster
set.seed(100)

clusplot(clus.data[ ,c(1,3,5:8)], clus.4, color = T, 
         shade = T, labels = 4, lines = 0, main = "")


 
ward.clus$diss
 