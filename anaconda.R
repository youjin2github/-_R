setwd("C:/Users/김유진/Desktop/4학년 1힉기/통계분석방법/기말고사 공부")

# 하나의 변수에 다른 변수들이 어떠한 영향을 미치는지 분석 

# anaconda data
library(moments)
anaconda <- read.table("anaconda.txt",header=T)

head(anaconda)

# weight

hist(anaconda$weight)
hist(log(anaconda$weight))

skewness(anaconda$weight)
skewness(log(anaconda$weight))

# length

hist(anaconda$length)
skewness(anaconda$length)
skewness(log(anaconda$length))

# gender
plot(anaconda$gender)
table(anaconda$gender)

# length vs. weight
plot(anaconda$length,anaconda$weight)
lines(smooth.spline(anaconda$length,anaconda$weight,spar=1.2),col="blue")
abline(lm(weight~length,data=anaconda),col="red")

plot(log(anaconda$length),log(anaconda$weight))
lines(smooth.spline(log(anaconda$length),log(anaconda$weight),spar=1.2),col="blue")
abline(lm(log(weight)~log(length),data=anaconda),col="red")

# gender vs. weight
plot(log(anaconda$weight)~anaconda$gender)

hist(anaconda$weight[anaconda$gender=="F"])
hist(log(anaconda$weight[anaconda$gender=="F"]))

hist(anaconda$weight[anaconda$gender=="M"])
hist(log(anaconda$weight[anaconda$gender=="M"]))

d1<-density(anaconda$weight[anaconda$gender=="F"])
plot(d1)
d2<-density(anaconda$weight[anaconda$gender=="M"])
plot(d2)
d1
d2
plot(d1,col="red",xlim=c(-20,120),ylim=c(0,0.2))
lines(d2,col="blue")


d1<-density(log(anaconda$weight[anaconda$gender=="F"]))
plot(d1)
d2<-density(log(anaconda$weight[anaconda$gender=="M"]))
plot(d2)
d1
d2
plot(d1,col="red",xlim=c(0.5,5.5),ylim=c(0,1.2))
lines(d2,col="blue")

# (length, gender) vs. weight

plot(anaconda$length,anaconda$weight,
	col=c("red","blue")[anaconda$gender],
	pch=c(17,20)[anaconda$gender]
)

plot(log(anaconda$length),log(anaconda$weight),
	col=c("red","blue")[anaconda$gender],
	pch=c(17,20)[anaconda$gender]
)
lines(smooth.spline(log(anaconda$length),log(anaconda$weight),spar=1.2),
	col="green",lwd=2)
legend("topleft",c("female","male"),pch=c(17,20),col=c("red","blue"))

plot(log(anaconda$length),log(anaconda$weight),
	col=c("red","blue")[anaconda$gender],
	pch=c(17,20)[anaconda$gender]
)
abline(lm(log(weight)~log(length),
	data=anaconda[anaconda$gender=="F",]),
	col="red")
abline(lm(log(weight)~log(length),
	data=anaconda[anaconda$gender=="M",]),
	col="blue")
abline(lm(log(weight)~log(length),
	data=anaconda),
	col="black")

female <- lm(log(weight)~log(length),
	data=anaconda[anaconda$gender=="F",])
summary(female)

male <- lm(log(weight)~log(length),
	data=anaconda[anaconda$gender=="M",])
summary(male)

# 기울기랑 절편의 차이를 분석

full <- lm(log(weight)~log(length)+gender+gender*log(length),
	data=anaconda)
summary(full)

full <- lm(log(weight)~log(length)+gender,
	data=anaconda)
summary(full)
