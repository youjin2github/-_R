# 이변량 실습 
# 연속형 vs 연속형 

setwd("C:/Users/김유진/Desktop/4학년 1힉기/통계분석방법/기말고사 공부")
size = read.csv("size_korea_2015.csv")
str(size)

#키
hist(size$height, freq = F, main = "키의 히스토그램",
     xlab = "키", ylab = "밀도", breaks = 50)
lines( density(size$height, bw = 16) )
#평균 선 그리기
abline(v=mean(size$height),col="red",lwd=3)
#중앙값 그리기
abline( v= median(size$height), col = "blue", lwd = 3)
legend("topright", c("평균","중앙값"),lty = c(1,1), lwd = 3
       , col = c("red","blue"))

#몸무게
hist(size$weight, freq = F, ylim = c(0,0.035), main = "몸무게의 히스토그램"
, xlab = "몸무게", ylab = "밀도")
dw = density(size$weight)
dw
lines(density(size$weight,bw=2))
abline(v=mean(size$weight),col="red",lwd=3)
abline(v=median(size$weight),col="blue",lwd=3)
legend("topright",c("평균","중앙값"),lty=c(1,1),lwd=3,col=c("red","blue"))

#연속형 범주형
#범주형 변수 단변량 분석
barplot(table(data))
hm=data[which(data=="")]
hist(hm,freq = F)
summary(data)
hist(hm,freq=F,xlim=c(),main="",xlab="")
abline(v=mean(hm),col="red",lwd=3)
#범주별로 분포가 좌우대칭인지, 중심 위치, 퍼진정도 차이 확인
par(mfrow=c(1,1))
boxplot(연속형~범주형)
#겹쳐서 hist 그리는거 hist(data, add = T,freq=F, col=)
d1=density(hm,bw=15)
plot(d1)
#empirical cdf
plot(ecdf(hm),add=T,col="",xlim = c(),pch=NA_integer_,lwd=2)
abline(v=mean(hm),col="",lwd=,lty=)
#정규성 검정
agostino.test(hm)
ansari.test(hm)
qqnorm(hm)
qqline(hm)
ad.test(hm)
#가설검정
describe(hm)
anova(lm(연속형변수 ~ 범주형변수))
ks.test(data,data)


#범주형 범주형 이변량 데이터 분석
str(data)
table_data = table(data)
pie(table_data)
barplot(table_data)
#분할표
table_he = table(data,data)
margin.table(table_he,margin = 1)
par(mfrow = c(행,열))
barplot(table_he, main = "")
#위치측도의 검정
hist(data,breaks = 10)
boxplot(data)
summary(data)
library(psych)
describe(data)
library(moments)
agostino.test()
anscombe.test()
qqnorm()
qqline()
library(nortest)
shapiro.test()
lillie.test()
ad.test()
#t분포를 이용한 신뢰구간
mean = mean()
sd = sd()
n = length()
lcl = mean - qt(1-0.05/2,n-1,lower.tail = F)*sd/sqrt
#검정통계량
tvalue = (mean -0)/(sd/sqrt(n))
#기각역
t = qt(0.05/2,n-1,lower.tail = F)
abs(tvalue)>t
#pvalue구하기
pvalue = 2*pt(abs(tvalue),n-1,lower.tail = F)
pvalue
t.test(data,mu=0)
wilcox.test()
