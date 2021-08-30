
Input <- ("
temp humidity power
35 74 214
28 79 152
31 63 176
27 67 129 
37 82 254
38 80 266
34 66 210
35 59 204
33 72 213
29 65 150
")

electric <- read.table(textConnection(Input),header=TRUE)

# 그림
plot(electric)

# 상관계수

with(electric,cor.test(temp,humidity))
with(electric,cor.test(temp,power))
with(electric,cor.test(humidity,power))

# 온도가 최대전력부하량에 미치는 영향

# 단순회귀분석

electric.lm <- lm(power~temp,data=electric)

summary(electric.lm)


# 잔차 검증

str(electric.lm)

pred <- predict(electric.lm)
resid <- residuals(electric.lm)

# 잔차그림

plot(pred,resid)
abline(h=0)

# 정규성

hist(resid)

# 정규확률그림

qqnorm(resid)
qqline(resid)

# 정규성 검정

shapiro.test(resid)

# 독립성 검정

resid0 <- resid[-length(resid)]
resid1 <- resid[-1]

resid
resid0
resid1

# 그림

plot(resid0,resid1)
abline(v=0,h=0)

cor.test(resid0,resid1)

# 모수에 대한 검정

electric.sum <- summary(electric.lm)


str(electric.sum)

electric.sum$coefficients

# H0: beta1=0 vs. H1: beta1!=0

tvalue <- (electric.sum$coefficients[2,1]-0)/electric.sum$coefficients[2,2]
pvalue <- 2*(1-pt(abs(tvalue),df=electric.sum$df[2]))
#value <- 2*(1-pt(abs(tvalue),df=electric.lm$df.residual))
pvalue

# H0: beta1=0 vs. H1: beta1>0
pvalue <- 1-pt(tvalue,df=electric.sum$df[2])
#value <- pt(tvalue,df=electric.sum$df[2],lower.tail=F)

# H0: beta1=11 vs. H1: beta1>11
tvalue <- (electric.sum$coefficients[2,1]-11)/electric.sum$coefficients[2,2]
pvalue <- 1-pt(tvalue,df=electric.sum$df[2])
tvalue;pvalue


# 예측값에 대한 검정

newdata <- data.frame(temp=c(33))

predict(electric.lm,newdata,lower=0.85,interval="confidence")

predict(electric.lm,newdata,lower=0.85,interval="prediction")


pred.frame <- data.frame(temp=seq(25,40,0.2))

pc <- predict(electric.lm,newdata=pred.frame,int="c")
pp <- predict(electric.lm,newdata=pred.frame,int="p")

head(pc)
head(pp)

plot(electric$temp,electric$power,xlim=c(25,40),ylim=c(100,300))
matlines(pred.frame$temp,pc,lty=c(1,2,2),col="blue")
matlines(pred.frame$temp,pp,lty=c(1,2,2),col="red")


# temp, humdity vs. power

electric.lm2 <- lm(power~temp+humidity,data=electric)

electric.sum2 <- summary(electric.lm2)
electric.sum2

anova(electric.lm2)
anova(electric.lm)

# H0: beta1=10 vs. beta1>10

tvalue <- (electric.sum2$coefficients[2,1]-10)/electric.sum2$coefficients[2,2]
pvalue <- 1-pt(tvalue,df=electric.sum$df[2])
tvalue;pvalue

# H0: beta2=0 vs. beta1!=0

tvalue <- (electric.sum2$coefficients[3,1]-0)/electric.sum2$coefficients[3,2]
pvalue <- 2*(1-pt(abs(tvalue),df=electric.sum$df[2]))
tvalue;pvalue

# humidity vs. power
summary(lm(power~humidity,data=electric))

# 잔차 그림


# 잔차 검증

pred <- predict(electric.lm2)
resid <- residuals(electric.lm2)

# 잔차그림

plot(pred,resid)
abline(h=0)

# 정규성

hist(resid)

# 정규확률그림

qqnorm(resid)
qqline(resid)

# 정규성 검정

shapiro.test(resid)

# 독립성 검정

resid0 <- resid[-length(resid)]
resid1 <- resid[-1]

# 그림

plot(resid0,resid1)
abline(v=0,h=0)

cor.test(resid0,resid1)

# 중회귀분석 연습

mtcars

# 분석 목표: 배기량(disp), 마력(hp), 무게(wt)가 연비(mpg)에 
# 미치는 영향 분석

cars <- mtcars[,c("mpg","disp","hp","wt")]

# 그림

pairs(cars)
plot(cars)

# 배기량 vs. 연비

plot(cars$disp,cars$mpg)
lines(lowess(cars$mpg~cars$disp,f=0.8))
abline(lm(cars$mpg~cars$disp))

plot(cars$disp,cars$mpg)
lines(smooth.spline(cars$disp,cars$mpg,spar=0.9))
abline(lm(cars$mpg~cars$disp))


# 마력 vs. 연비

plot(cars$hp,cars$mpg)
lines(lowess(cars$mpg~cars$hp,f=0.8))
abline(lm(cars$mpg~cars$hp))


# 무게 vs. 연비

plot(cars$wt,cars$mpg)
lines(lowess(cars$mpg~cars$wt,f=0.8))
abline(lm(cars$mpg~cars$wt))

# 비선형 관계를 위한 파생변수
cars$disp2 <- cars$disp^2
cars$hp2 <- cars$hp^2
cars$wt2 <- cars$wt^2


# correlation

cor(cars)

cor.test(cars$mpg,cars$hp)


# 중회귀분석
lm1 <- lm(mpg~disp+hp+wt,data=cars)
summary(lm1)	

lm2 <- lm(mpg~hp+wt,data=cars)
summary(lm2)	

# 잔차검증

pred <- lm2$fitted.values
resid <- lm2$residuals

pred <- predict(lm2)
resid <- residuals(lm2)

# 잔차그림

plot(pred,resid)
abline(h=0)
lines(lowess(resid~pred))

plot(cars$disp,resid)
abline(h=0)
lines(lowess(resid~cars$disp,f=0.8))

plot(cars$hp,resid)
abline(h=0)
lines(lowess(resid~cars$hp,f=0.9))

plot(cars$wt,resid)
abline(h=0)
lines(lowess(resid~cars$wt,f=0.9))

# 비선형을 고려한 파생변수를 포함한 모형

lm3 <- lm(mpg~disp+disp2+hp+hp2+wt+wt2,data=cars)
summary(lm3)

lm4 <- lm(mpg~hp+hp2+wt+wt2,data=cars)
summary(lm4)


# 잔차검증

pred <- predict(lm4 )
resid <- residuals(lm4 )

# 잔차그림

plot(pred,resid)
abline(h=0)
lines(lowess(resid~pred,f=0.9))

plot(cars$disp,resid)
abline(h=0)
lines(lowess(resid~cars$disp,f=0.8))

plot(cars$hp,resid)
abline(h=0)
lines(lowess(resid~cars$hp,f=0.9))

plot(cars$wt,resid)
abline(h=0)
lines(lowess(resid~cars$wt,f=0.9))

# 정규성 검정

hist(resid)

library(moments)

skewness(resid)
agostino.test(resid)

kurtosis(resid)
anscombe.test(resid)

cars
qq<-qqnorm(resid)
qqline(resid)
identify(qq$x,qq$y,labels=names(qq$y))

shapiro.test(resid)
