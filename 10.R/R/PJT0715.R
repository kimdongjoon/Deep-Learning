setwd("c:/Rdata") # C드라이브 Rdata파일내
library(dplyr)    # dplyr패키지 로드
library(stringr)
data <- read.csv  ("winnerDrink.csv")  # winner팀의 winnerDrink.csv파일을 불러옴
head(data)        # 불러온 데이터 확인
str(data)         # CATEGORY값만이 chr 형식임을 확인
                  # 데이터는 10개의 변수와 166개의 object로 구성되어 있음
#View(data)       # 데이터 보기

rownames(data) = data[,1]     #X
data=subset(data,select=-X)
#rownames(data) = data[,2]     #YM

#data=subset(data,select=-X)
head(data)
# X        : 순번
# YM       : 판매년월 
# CATEGORY : 카테고리
# ITEM_CNT : 상품품목수
# QTY      : 판매량
# PRICE    : 가격
# MAXTEMP  : 기온
# SALEDAY  : 영업(판매)일수
# RAIN_DAY : 강우일수
# HOLY_DAY : 휴일 일수


#텍스트 인코딩 
#에너지 음료 = 0
#일반탄산음료 = 1
#차음료 = 2

data$CATEGORY <- ifelse(data$CATEGORY=="에너지음료",1,
                        ifelse(data$CATEGORY=="일반탄산음료",2,3))

#범주형 데이터 factor 진행
#data$X = as.factor(data$X)
data$YM = as.factor(data$YM)
data$YM = as.integer(data$YM)
data$CATEGORY = as.factor(data$CATEGORY)
data$CATEGORY = as.integer(data$CATEGORY)

str(data)
# X        : 순번
# YM       : 판매년월 
# CATEGORY : 카테고리
# ITEM_CNT : 상품품목수
# QTY      : 판매량
# PRICE    : 가격
# MAXTEMP  : 기온
# SALEDAY  : 영업(판매)일수
# RAIN_DAY : 강우일수
# HOLY_DAY : 휴일 일수                
                
#View(data)

data1= data%>%                        # 에너지음료 데이터 추출 46/166
  filter(CATEGORY == 1)
data2 = data%>%                       # 일반탄산음료 데이터 추출 60/166
  filter(CATEGORY == 2)
data3 = data%>%                       # 차음료 데이터 추출 60/166
  filter(CATEGORY == 3)



#stringr을 이용한 텍스트 분리(참고용)
# data1$CATEGORY = str_replace_all(data1$CATEGORY,"에너지음료","0")
# data1
# data2$CATEGORY = str_replace_all(data2$CATEGORY,"일반탄산음료","1")
# data2
# data3$CATEGORY = str_replace_all(data3$CATEGORY,"차음료","2")
# data3

#rownames(data) = data[,1]     #X
# rownames(data1) = data1[,1]     #YM
# rownames(data2) = data2[,1]     #YM
# rownames(data3) = data3[,1]     #YM



head(data1)

head(data2)

head(data3)

# 각각의 히스토그램
hist(data1$QTY)
hist(data2$QTY)
hist(data3$QTY)

# 정규성 테스트
shapiro.test(data1$QTY) # p-value = 0.0001
shapiro.test(data2$QTY) # p-value = 0.01
shapiro.test(data3$QTY) # p-value = 0.08
# -> 이를통해 유의확률 p-value가 어느정도 인지 판별할수 있음.

qqnorm(data1$QTY)
qqline(data1$QTY)

qqnorm(data2$QTY)
qqline(data2$QTY)

qqnorm(data3$QTY)
qqline(data3$QTY)

# 상관관계 분석을 위해 cha 값인CATEGORY를 제외하여 다시 저장

#종속 변수 / 독립 변수
#상관관계 분석에서 제품명은 종속변수 이므로 데이터프레임에서 제외
# data1=subset(data1,select=-X)
# data2=subset(data2,select=-X)
# data3=subset(data3,select=-X)


data1=subset(data1,select=-CATEGORY)
# data1=subset(data1,select=-YM)
# 
data2=subset(data2,select=-CATEGORY)
# data2=subset(data2,select=-YM)
# 
data3=subset(data3,select=-CATEGORY)
# data3=subset(data3,select=-YM)
str(data1)
# 판매량 상관관계 분석
cor(data1)
cor(data2)
cor(data3)
data1
# 각각의 데이터에 대해 회귀분석을 적용
out1=lm(QTY~.,data=data1)
out2=lm(QTY~.,data=data2)
out3=lm(QTY~.,data=data3)

# 각데이터의 회귀분석 그래프
 plot(out1)
 plot(out2)
 plot(out3)

# (모형간소화)변수선택 방법을 지정. 
both1=step(out1,direction="both",trcce=FALSE) # 최종 AIC=542.58   QTY ~ PRICE + MAXTEMP + SALEDAY
both2=step(out2,direction="both",trcce=FALSE) # 최종 AIC=761.21   QTY ~ X + ITEM_CNT + PRICE + MAXTEMP + SALEDAY
both3=step(out3,direction="both",trcce=FALSE) # 최종 AIC=654.38   QTY ~ ITEM_CNT + MAXTEMP + SALEDAY + RAIN_DAY

# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both1)
anova(both2)    #ITEM_CNT+PRICE
anova(both3)    #RAIN_DAY

 
#최적화 진행
out1_1=lm(QTY ~ PRICE + MAXTEMP + SALEDAY, data = data1)
out2_1=lm(QTY ~ ITEM_CNT + PRICE + MAXTEMP + SALEDAY, data = data2)
out3_1=lm(QTY ~ MAXTEMP + SALEDAY, data = data3)

#다중공선성 확인
# 다중공선성의 상태는 데이터 분석시 문제를 야기하는 하나의 특성임.
# 부정적인 영향을 미치는상태.
# 회귀분석 뿐 아니라 결정 트리에서도 다중공선성은 부정적인 영향을 미침.

# 다중공선성(Multicollinearity)은 입력변수들 간의 상관정도가 높은상태를 말한다.  
# VIF(Variation inilation Factor, 분산팽창지수)
# 일반적으로 VIF가 10 이상일 때 다중공선성이 존재한다고 판단한다. 
# 즉 모델 선정시 VIF가 높은 것은 제거를 해야 한다. 

library("car")
install.packages("psych")
library("psych")

pairs.panels(data1[names(data1)])
vif(out1_1)

pairs.panels(data2[names(data2)])
vif(out2_1)

pairs.panels(data3[names(data3)])
vif(out3_1)

#최적화 진행
# (모형간소화)변수선택 방법을 지정. both이므도 단계
both1=step(out1,direction="both",trcce=FALSE) # 최종 AIC=542.58         QTY ~ PRICE + MAXTEMP + SALEDAY
# 동일
both2=step(out2,direction="both",trcce=FALSE) # 최종 AIC=769.87         QTY ~ ITEM_CNT + PRICE + MAXTEMP + SALEDAY

# QTY ~ MAXTEMP + SALEDAY
both2_1=step(out2_1,direction="both",trcce=FALSE) # 최종 AIC=769.95     QTY ~ MAXTEMP + SALEDAY


# QTY ~ ITEM_CNT + MAXTEMP + SALEDAY + RAIN_DAY
both3=step(out3,direction="both",trcce=FALSE) # 최종 AIC=654.38         QTY ~ ITEM_CNT + MAXTEMP + SALEDAY + RAIN_DAY

# QTY ~ MAXTEMP + SALEDAY
both3_1=step(out3_1,direction="both",trcce=FALSE) # 최종 AIC=655.68     QTY ~ MAXTEMP + SALEDAY


# F분포를 통한 가설검정  F = (군간변동)/(군내변동)
anova(both1)

anova(both2)
anova(both2_1)

anova(both3)
anova(both3_1)


# 회귀식
summary(both1)

summary(both2)    
summary(both2_1)

summary(both3)   

summary(both3_1)

# 예측 실행
pre = predict(out3,newdata = data3)
pre = as.data.frame(pre)
pre

data3$predict = pre
data3$result = round(pre - data3["QTY"],0)

data3
summary(data3)
# 에너지음료 R-squared: 0.8229    판매량 = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY
# 일반탄산음료 R-squared: 0.8846  판매량 = 410.7761+68.6257(X)-73.2499(ITEM_CNT)-2.4391(PRICE)+93.8410(MAXTEMP)+0.0223(SALEDAY)
# 차음료 R-squared: 0.8814        판매량 = 407.0016-19.8398(ITEM_CNT)+50.1766(MAXTEMP)+0.0114(SALEDAY)-0.0065

# 회귀식을 통한 예측 판매량을 반올림하여 pred에 저장
pred1 = data1 %>%
  mutate(pred_QTY = -3323-0.9419*PRICE+28.49*MAXTEMP+0.02586*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))

pred2 = data2 %>%
  mutate(pred_QTY = 410.7761+68.6257*X-73.2499*ITEM_CNT-2.4391*PRICE+93.8410*MAXTEMP+0.0223*SALEDAY)%>%
  summarise(QTY,round(pred_QTY))

pred3 = data3 %>%
  mutate(pred_QTY = 407.0016-19.8398*ITEM_CNT+50.1766*MAXTEMP+0.0114*SALEDAY-0.0065*RAIN_DAY)%>%
  summarise(QTY,round(pred_QTY))


# 실제 판매량과 예측 판매량 비교
pred1
pred2
pred3

# 그래프를 통한 X=Y그래프에 유하삼을 볼 수 있음
plot(pred1)
plot(pred2)
plot(pred3)
