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


