
# 범주형 변수 단변량 분석 연습 


# 사망원인에 대한 분석 

death <- c(2,1,2,4,2,5,3,3,5,6,3,8,3,
           3,6,3,6,5,3,5,2,6,2,3,4,3,
           2,9,2,2,3,2,7,3,2,10,6,2,3,
           1,2,3,3,4,3,2,6,2,2,3,2,3,
           4,3,2,3,5,2,5,5,3,4,3,6,2,
           1,2,3,2,6,3,3,6,3,2,3,6,4,
           6,5,3,5,6,2,6,3,2,3,2,6,2,
           6,3,3,2,6,9,6,3,6,6,2,3,2,
           3,5,3,5,2,3,2,3,3,1,3,3,2,
           3,3,4,3,6,6,3,3,3,2,3,3,6)

# 내용 확인 하기
death

# type 확인하기
typeof(death)
class(death)

# double을 integer로 바꾸기
death<-as.integer(death)
typeof(death)


# 도수분포표
table(death)
table<-table(death)

class(table)

# 막대그래프
barplot(height=table,main="사망원인에 대한 막대그래프",
	ylab="빈도수",xlab="사망원인", col=4)

# 범례가 있는 막대그래프
cause<-c("감염성 질환","암","순환기 질환","호흡기 질환","소화기 질환",
	"사고사","비뇨기 질환","정신병","노환","신경계 질환")
barplot(height=table,main="사망원인에 대한 막대그래프",
	ylab="빈도수",xlab="사망원인", col=rainbow(10), legend.text=cause)

# 순환기 질환이 가장 많은 도수를 가지고 있고 그 다음은 암과 사고사이다. 비뇨기 질환,
# 정신병, 노환, 신경계 질환의 빈도는 미미하다.

# 원형 그래프
pie(table,label=cause,main="사망원인에 대한 원형그래프",cex=1)

# 순환기 질환과 암이 50% 이상 차지하고 있고 사고사까지 포함하면 전체에 3/4 가까이
# 차지하는 것으로 보인다.


# 파레토 그래프

install.packages("qcc")
library(qcc)

pareto.chart(table,main="사망원인에 대한 파레토 그림",xlab="질병 종류",ylab="도수",
	ylab2="누적상대도수",legend.text=cause)

# 3(순환기 질환), 2(암), 6(사고사)가 전체 사망의 75% 이상 차지하는 것을 알 수 있다.




# 연속형 변수 실습


drink <- c(101.8,101.5,101.8,102.6,101,96.8,102.4,100,98.8,98.1,
           98.8,98,99.4,95.5,100.1,100.5,97.4,100.2,101.4,98.7,
           101.4,99.4,101.7,99,99.7,98.9,99.5,100,99.7,100.9,
           99.7,99,98.8,99.7,100.9,99.9,97.5,101.5,98.2,99.2,
           98.6,101.4,102.1,102.9,100.8,99.4,103.7,100.3,100.2,101.1,
           101.8,100,101.2,100.5,101.2,101.6,99.9,100.5,100.4,98.1,
           100.1,101.6,99.3,96.1,100,99.7,99.7,99.4,101.5,100.9,
           101.3,99.9,99.1,100.7,100.8,100.8,101.4,100.3,98.4,97.2)

# 히스토그램

# 적당한 계급구간수 정하기
hist(drink,breaks=40)
hist(drink,breaks=20)
hist(drink,breaks=10)

hist<-hist(drink,breaks=10)
hist

# 돗수다각형 그리기
lines(x=hist$mids,y=hist$counts,type="b")
lines(x=hist$mids,y=hist$counts,type="b",pch=20)

# histogram with density plot
hist(drink,freq=FALSE,xlim=c(92,108)
lines(density(drink))
# 좌나 우로 쏠려 있지 않아 보이므로 대칭에 가까운 것으로 보인다.
# 단봉형으로 보인다.특이치나 이상치가 보이지 않는다

# 기술통계량

length(drink)
mean(drink)
sd(drink)
var(drink)
min(drink)
max(drink)
summary(drink)

# 백분위수
quantile(drink)
quantile(drink,c(0.25,0.75))

# 범위
range(drink)
q<-quantile(drink)
# max(drink)-min(drink)
# q(5)-q(1)

# 사분위수
IQR(drink)
q(4)-q(2)

# 기술통계량 한꺼번에 보기
install.packages("psych")
library(psych)

describe(drink)

# 평균과 중앙값을 볼 때 중심의 위치는 100에 가깝고
# 99와 101 사이에 관측값의 50% 가까이 있는 것으로 
# 보인다.

# 왜도 & 첨도
install.packages("moments")
library(moments)

skewness(drink)
agostino.test(drink)

# 왜도가 음수이므로 오른쪽으로 쏠리는 분포를 가지고 있는 것으로 보이지만
# 그 값이 작으므로 쏠린 정도가 심하지 않은 것으로 보인다. 특히, skewness
# test에서 p-값이 0.05보다 크므로 왜도가 0이라고 할 수 있다. 즉, 대칭인
# 판단할 수 있다.

kurtosis(drink)
anscombe.test(drink)

# 첨도가 3보다 약간 크지만 많이 크지 않으므로 꼬리의 두께가 정규분포와
# 비슷하다고 할 수 있다. 첨도에 대한 검정에서 p-값이 0.05보다 크므로
# 첨도가 3이 아니라는 대립가설을 기각할 수 있다. 따라서, 꼬리의 두께가
# 정규분포와 같다고 할 수 있다.


# 정규성 확인

# 정규확률그림

qqnorm(drink)
qqline(drink)

# 정규확률그림에서 약간 위로 볼록한 형태를 가지므로 약간 오른쪽으로 쏠린 분포를
# 갖는다는 것을 알 수 있다. 그런데 점들이 직선에 가까우므로 쏠린 정도가 심하지 
# 않아 보인다.

# 정규성 검정

shapiro.test(drink)

install.packages("nortest")
library(nortest)

lillie.test(drink)
ad.test(drink)

# 모든 검정의 p-값이 0.05보다 크므로 데이터는 정규분포로부터 나왔다고 할 수 있다.




# 실습 예제 : Pima Indian 당뇨병 데이터

install.packages("mlbench")
library(mlbench)
data(PimaIndiansDiabetes)

str(PimaIndiansDiabetes)

pedigree<-PimaIndiansDiabetes$pedigree

# 그림
hist(pedigree,breaks=40)
hist(pedigree,breaks=30)
hist(pedigree,breaks=20)
hist(pedigree)

boxplot(pedigree)

# 왼쪽으로 쏠린 분포를 하고 있다.

# 기술통계량

describe(pedigree)

# 평균은 0.5에 가깝고 중앙값은 4에 가까운 값을 가지고 있다.
# 평균이 중앙값보다 크고 왜도가 0보다 크므로 왼쪽으로 쏠린 분포를
# 하고 있다는 것을 알 수 있다.

agostino.test(pedigree)
anscombe.test(pedigree)

# 왜도에 대한 검정에서 p-값이 0.05보다 매우 작으므로 왜도가 0이라고
# 할 수 없다.

# 변수변환

hist(sqrt(pedigree))
agostino.test(sqrt(pedigree))

hist(log(pedigree))
agostino.test(log(pedigree))

# sqrt 변환은 왜도를 완화하지만 sqrt 변환에 대한 p-값이 0.05보다
# 작으므로 충분하지 않고 log 변환을 사용할 경우 왜도에 대한 p-값이
# 0.05	보다 크므로 log 변환을 사용하는 것이 적합한 것으로 보인다.

anscombe.test(log(pedigree))

# log를 사용할 경우 첨도가 2.5로 3보다 작으므로 꼬리가 정규분포보다 
# 얇아 보인다. 첨도에 대한 p-값이 0.05보다 작으므로 첨도가 3이라고 
# 할 수 없다.

# 정규성 확인

# 정규확률그림

qqnorm(log(pedigree))
qqline(log(pedigree))

# 약간 S자 형태의 정규확률그림을 보인다. 따라서, 꼬리가 얇다는 위에서의
# 주장을 지지하고 있다.

ad.test(log(pedigree)) 
lillie.test(log(pedigree))

# 두 검정 모두 p-값이 0.05보다 작으므로 정규분포를 따른다고 할 수 없다.

# 위의 결과를 종합하면, pedigree의 log 변환을 경우 좌우 대칭이기는 하나
# 꼬리의 두께가 얇아 정규분포로부터 추출된 데이터라고 할 수 없다.


# 실습 예 : 사망연령

dage<-scan(file="death_age.txt")

# 그림
hist(dage,freq=FALSE)
lines(density(dage))
boxplot(dage)

# 분포가 오른쪽으로 쏠려 있는 것을 알 수 있다.

# 통계량
describe(dage)

# 평균 사망연령을 79세 정도 이고 중앙값은 81이다. 중앙값이 평균보다 크고
# 왜도가 음수이므로 오른쪽으로 쏠린 분포라고 할 수 있다. 왜도가 음수이므로
# 분포가 오른쪽으로 쏠려 있다고 하는 주장을 지지한다.

agostino.test(dage)

# 왜도에 대한 p-값이 매우 작으므로 왜도가 0이라고 할 수 없다.

hist(log(dage+1))
skewness(log(dage+1))

# log 변환을 할 경우 쏠린 경향이 더 심해지는 것을 할 수 있다.

hist(dage^2)
skewness(dage^2)

hist(dage^3)
skewness(dage^3)

# 제곱의 경우 오른쪽으로 쏠린 정도를 완화해 주지만 아직 오른쪽으로 쏠려 있고
# 세제곱의 경우 왼쪽으로 쏠리는 경향을 보인다.

# 첨도
anscombe.test(dage^2)
anscombe.test(dage^3)

# 제곱과 세제곱 모두 꼬리가 정규분포보다 얇은 것을 알 수 있다.


# 정규확률그림

qqnorm(dage)
qqline(dage)

# 정규확률그림이 위로 볼록한 형태를 보인다. 즉, 전형적인 오른쪽으로 쏠린
# 분포를 형태를 보인다고 할 수 있다.

qqnorm(dage^2)
qqline(dage^2)

qqnorm(dage^3)
qqline(dage^3)

# 정규확률그림에서 제곱과 세제곱 모두 대칭에 가까우나 꼬리가 정규분포보다
# 얇은 분포의 형태를 보인다고 할 수 있다.

# 정규성 검정

ad.test(dage)
lillie.test(dage)

ad.test(dage^2)
lillie.test(dage^2)

ad.test(dage^3)
lillie.test(dage^3)

# 원데이터, 제곱, 세제곱 모두 p-값이 0.05보다 작으므로 정규분포를 따른다고
# 할 수 없다.

# 제곱이나 세제곱을 사용할 경우 정규분포를 따르지는 않으나 왜도를 완화시킴으로써
# 이상치의 효과를 약화시키고 극단의 영향을 줄일 수 있다.


# 다른 예 (old faithful 데이터)

faithful
str(faithful)

hist(faithful$eruptions,breaks=20,freq=FALSE,xlim=c(0,6))
lines(density(faithful$eruptions))

# old faithful 간혈천 데이터에서 분출량은 봉오리가 2개인 분포를 보인다.

