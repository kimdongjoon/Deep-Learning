setwd("c:/Rdata")
# View(attitude)
# with(attitude)
# cor.test
# plit(attitude)



data = read.csv("cars.csv")
data

out = lm(dist~speed, data = data)
summary(out)
plot(dist~speed, data= cars, col="blue")
abline(out, col= 'red')
out1 = lm(dist~speed+0, data = data))
summary(lm(dist~speed+0, data = data))
plot(out1)
par(mfrow = c(2,2))
plot(out)

shapiro.test(data$dist)
shapiro.test(log(data$dist))
shapiro.test(sqrt(data$dist))

out3= lm(sqrt(dist)~speed+0, data = data)
summary(out3)
plot(out3)



out3$fitted.values
cbind(data$speed,out3$fitted.values)
new = data.frame(speed=data$speed)

cbind(new$speed,predict(out3,new,interval = "confidence"))
cbind(new$speed,predict(out3,new,interval = "prediction"))

###다중회귀모형
# 예: Programmer 급여 조사
# 한 소프트웨어 회사가 프로그래머 20명에 대한 급여 자료를 수집하였다. 
# 그리고 급여가 경력연수나 직무적성 검사성적과 연관성을 갖는지를
# 결정하기 위하여 회귀분석이 사용될 수 있다는 제안이 있었다. 
# 
# 경력 연수와 직무적성검사 성적과 그에 상응하는 
# (연봉 단위는 $천)이 다음 슬라이더에 나타나 있다. 
 
data = read.csv("salary.csv")
head(data)

out = lm(salary~experience+score,data = data)
summary(out)
cbind(data$speed,predict(out3,new,interval = "prediction"))



out = lm(rating~.-critical, data= attitude)
summary(out)
anova(out)

# 백워드 방식으로 돌린 후 adjusted R-squared를 확인
backward=step(out,direction="backward",trace=FALSE) 
summary(backward)

# 포워드와 백워드중 성능이 좋은것을 사용. 결과를 비교했을 시 백워드 값과 동일하므로
# 백워드 성능이 해당 데이터에선 좋음을 나타냄.
both=step(out,direction="both",trace=FALSE) 
summary(both)



install.packages("leaps")
library(leaps)

leaps = regsubsets(rating~.,data=attitude, nbest = 5)
summary(leaps)

par(mfrow = c(1,1))
plot(leaps, scale='bic')
out_bic = lm(rating~complaints, data=attitude)
summary(out_bic)

plot(leaps,scale= 'Cp')
out_cp = lm(rating~complaints+learning,data=attitude)
summary(out_cp)
plot(leaps, scale = "adjr2")
out_adjr=lm(rating~complaints+learning+advance, data = attitude)
summary(out_adjr)

## 17일 진행 발표
## adjusted R-square가 최대인 Best Model을 찾으시오.

