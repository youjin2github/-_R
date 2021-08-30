?InsectSprays

# data 가져오기
str(InsectSprays)
attach(InsectSprays)

# data 확인
spray
count
table(spray)

# 그림

# box plot

plot(count~spray)

# histogram

summary(count)

par(mfrow=c(6,1))
par(mfrow=c(3,2))
hist(count[spray=="A"],xlim=c(0,30))
abline(v=mean(count[spray=="A"]),col="red")
hist(count[spray=="B"],xlim=c(0,30))
abline(v=mean(count[spray=="B"]),col="red")
hist(count[spray=="C"],xlim=c(0,30))
abline(v=mean(count[spray=="C"]),col="red")
hist(count[spray=="D"],xlim=c(0,30))
abline(v=mean(count[spray=="D"]),col="red")
hist(count[spray=="E"],xlim=c(0,30))
abline(v=mean(count[spray=="E"]),col="red")
hist(count[spray=="F"],xlim=c(0,30))
abline(v=mean(count[spray=="F"]),col="red")

par(mfrow=c(1,1))

# 통계량

tapply(count,spray,mean)
tapply(count,spray,var)

Photoperiod<-ordered(spray,levels=c("F","B","C","D","E","A"))
tapply(count,Photoperiod,mean)

# 분산분석

oneway.test(count~spray)

aov <- aov(count~spray)
summary(aov)

# 다중비교

TukeyHSD(aov)

# (F-B-A)-(D-E-C)

spray<-ordered(spray,levels=c("F","B","A","D","E","C"))
pairwise.t.test(count,spray)

pairwise.t.test(count,spray,pool.sd=F)



# 오차에 대한 가정 검정

plot(aov)

# 분산의 동일성
bartlett.test(count~spray)

# 정규성 검정

resid <- aov$residuals

hist(resid)

hist(resid,breaks=20)

library(moments)

agostino.test(resid)
anscombe.test(resid)

qqnorm(resid)
qqline(resid)

library(nortest)

ad.test(resid)
lillie.test(resid)

# 비모수 검정
kruskal.test(count~spray)


# iris data

str(iris)

table(iris$Species)

attach(iris)

summary(Sepal.Width)

# 그림 

# boxplot

plot(Sepal.Width~Species)
is.factor(Species)

# histogram

par(mfrow=c(3,1))
hist(Sepal.Width[Species=="setosa"],freq=F,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="setosa"]),col="red")
hist(Sepal.Width[Species=="versicolor"],freq=F,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="versicolor"]),col="red")
hist(Sepal.Width[Species=="virginica"],freq=F,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="virginica"]),col="red")


# 겹쳐 그리기
par(mfrow=c(1,1))
hist(Sepal.Width[Species=="setosa"],freq=F,xlim=c(1,5),ylim=c(0,1.5),col=rgb(1,0,0,0.5))
hist(Sepal.Width[Species=="versicolor"],freq=F,xlim=c(1,5),ylim=c(0,1.5),add=T,col=rgb(0,1,0,0.5))
hist(Sepal.Width[Species=="virginica"],freq=F,xlim=c(1,5),ylim=c(0,1.5),add=T,col=rgb(0,0,1,0.5))


# density plot

d1<-density(Sepal.Width[Species=="setosa"],bw=0.17)
plot(d1)
d2<-density(Sepal.Width[Species=="versicolor"],bw=0.16)
plot(d2)
d3<-density(Sepal.Width[Species=="virginica"],bw=0.13)
plot(d3)

par(mfrow=c(3,1))
plot(d1,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="setosa"]),col="red")
plot(d2,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="versicolor"]),col="red")
plot(d3,xlim=c(1,5))
abline(v=mean(Sepal.Width[Species=="virginica"]),col="red")

d1
d2
d3
par(mfrow=c(1,1))
plot(d1,xlim=c(1,5),ylim=c(0,1.5),col="red")
lines(d2,col="blue")
lines(d3,col="green")
abline(v=mean(Sepal.Width[Species=="setosa"]),lty=2,col="red")
abline(v=mean(Sepal.Width[Species=="versicolor"]),lty=2,col="blue")
abline(v=mean(Sepal.Width[Species=="virginica"]),lty=2,col="green")

# cdf를 통한 비교

c1<- ecdf(Sepal.Width[Species=="setosa"])
c2<- ecdf(Sepal.Width[Species=="versicolor"])
c3<- ecdf(Sepal.Width[Species=="virginica"])

plot(c1,xlim=c(1,5),col="red")
lines(c2,col="blue")
lines(c3,col="green")

plot(c1, verticals=TRUE, pch=46, col="red")
lines(c2,verticals=TRUE, pch=46, col="blue")
lines(c3,verticals=TRUE, pch=46, col="green")

# q-q plot

qqplot(Sepal.Width[Species=="setosa"],Sepal.Width[Species=="versicolor"])
abline(a=0,b=1)

qqplot(Sepal.Width[Species=="setosa"],Sepal.Width[Species=="virginica"])
abline(a=0,b=1)

qqplot(Sepal.Width[Species=="versicolor"],Sepal.Width[Species=="virginica"])
abline(a=0,b=1)

# 통계량
tapply(Sepal.Width,Species,summary)
tapply(Sepal.Width,Species,mean)
tapply(Sepal.Width,Species,var)


# 분산의 동일성

install.packages("car")
library(car)

install.packages("lawstat")
library(lawstat)

bartlett.test(Sepal.Width~Species)
levene.test(Sepal.Width,Species)
fligner.test(Sepal.Width~Species)



# 정규성 검정

qqnorm(Sepal.Width[Species=="setosa"])
qqline(Sepal.Width[Species=="setosa"])

qqnorm(Sepal.Width[Species=="versicolor"])
qqline(Sepal.Width[Species=="versicolor"])

qqnorm(Sepal.Width[Species=="virginica"])
qqline(Sepal.Width[Species=="virginica"])

# 왜도, 첨도

agostino.test(Sepal.Width[Species=="setosa"])
anscombe.test(Sepal.Width[Species=="setosa"])
agostino.test(Sepal.Width[Species=="versicolor"])
anscombe.test(Sepal.Width[Species=="versicolor"])
agostino.test(Sepal.Width[Species=="virginica"])
anscombe.test(Sepal.Width[Species=="virginica"])

# 정규성 검정
ad.test(Sepal.Width[Species=="setosa"])
ad.test(Sepal.Width[Species=="versicolor"])
ad.test(Sepal.Width[Species=="virginica"])

# 분산분석

oneway <- oneway.test(Sepal.Width~Species)

aov <- aov(Sepal.Width~Species)
summary(aov)

aov$coefficients

# 다중비교

TukeyHSD(aov(Sepal.Width~Species))
pairwise.t.test(Sepal.Width,Species)



# 정규성 검정
hist(resid)

agostino.test(resid)
anscombe.test(resid)

qqnorm(resid)
qqline(resid)

ad.test(aov$residuals)



# 잔차 검정
resid<-residuals(aov)
pred<-fitted(aov)

plot(resid~pred)
abline(h=0)

plot(aov)
