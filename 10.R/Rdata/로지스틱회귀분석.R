# 로지스틱 회귀 분석 
# 정의 :
#   로지스틱 회귀분석(logistic regreession)은 어떤
#   사건이 발생하는지 안하는지를 직접 예측하는 것이 아니라, 
#   그 사건이 발생할 확률을 예측한다. 
#   일반적으로 종속변수의 범주가 두개인 경우에 적용된다. 
# 
#   독립변수와 종속변수의 관계를 단순회귀분석과
#   대중회귀분석은 선형으로 가정하는데 비해, 
#   로지스틱 회귀분석은 S자형으로 가정한다 .

setwd("c:/Rdata")

data = read.csv("programming.csv")
data

model = glm(Success~Experience, data= data, family = binomial(logit))
summary(model)
cbind(data$Experience, model$fitted.values)
plot(Success~Experience, data = data)

points(model$fitted.values~data$Experience, col = 2)


#민감도 튜닝 Fit value를 조정하여 
#false의 데이터를 false로 True인 데이터를 True로 표현하는 최적값을 선택
#ex) 0.5인 경우 민감도는 (false를 false로 찾는값)
# FALSE TRUE
# 0    11    3
# 1     3    8
# 이므로 8 / (3+8)
# 특이도는 (True를 True로 찾는 값 )
# 11/14로 나타낼 수 있음. 즉
#민감도는 아래의 값으로 나타낼 수 있음. 

table(data$Success,model$fitted.values > 0.5)
c("민감도" = 8/11, "특이도" = 11/14)



#쿠폰---------------------------------------
coupon = read.csv("coupon.csv")
coupon
model2= glm(cbind(N_redeemed, N - N_redeemed)~Price_reduc, data = coupon, family = binomial(logit))
summary(model2)

# Call:
#   glm(formula = cbind(N_redeemed, N-N_redeemed) ~ Price_reduc, 
#       family = binomial(logit), data = coupon)

# Deviance Residuals: 
#   1        2        3        4  
# -0.8988   0.6677  -0.1837   0.7612  
# 5  
# -0.5477  
# 
# Coefficients:
#   Estimate Std. Error z value
# (Intercept) -2.044348   0.160977  -12.70
# Price_reduc  0.096834   0.008549   11.33
# Pr(>|z|)    
# (Intercept)   <2e-16 ***
#   Price_reduc   <2e-16 ***
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 149.4627  on 4  degrees of freedom
# Residual deviance:   2.1668  on 3  degrees of freedom
# AIC: 33.793
# 
# Number of Fisher Scoring iterations: 3


#Estimate std. / Price_reduc 값 참조
#0.096834
exp(0.096834)
exp
# 1.101677
# 쿠폰의 할인액이 1달러 증가할 때 
# 쿠폰을 사용할 Odds가 10%의 증가한다. 



# Exam:Disease Outbreak
# 모기에 의한 유행병의 전염을 연구하기 위해 두 지역에서
# 최근에 병에 걸린 사람들은 무작위 추출했다. 특정 증상을
# 보였는지 여부를 아래의 설명변수로 모형화한다. 

# 나이(X1) 
# ???사회경제적위치(X2=1??if??Middle,??X3=1??if??Lower)
# ???지역(X4=0??for??sector??1,??X4=1??for??sector??2)

disease = read.csv("disease.csv")
head(disease)
model3 = glm(disease~.,data = disease, family=binomial(logit))
summary(model3)

# > summary(model3)
# 
# Call:
#   glm(formula = disease ~ ., family = binomial(logit), data = disease)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q  
# -1.6552  -0.7529  -0.4788   0.8558  
# Max  
# 2.0977  
# 
# Coefficients:
#   Estimate Std. Error z value
# (Intercept)   -2.31293    0.64259  -3.599
# age            0.02975    0.01350   2.203
# status_middle  0.40879    0.59900   0.682
# status_lower  -0.30525    0.60413  -0.505
# sector         1.57475    0.50162   3.139
# Pr(>|z|)    
# (Intercept)   0.000319 ***          #P값이 0.05 이하인 경우 상관관계가 있음을 표현.
#   age           0.027577 *          #별이 있는 경우 독립변수이지만 상관관계에 있다로 표현. 
#   status_middle 0.494954    
# status_lower  0.613362    
# sector        0.001693 ** 
#   ---
#   Signif. codes:  
#   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05
# ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 122.32  on 97  degrees of freedom
# Residual deviance: 101.05  on 93  degrees of freedom
# AIC: 111.05
# 
# Number of Fisher Scoring iterations: 4


#위에서 상관관계를 확인 했으므로 새로운 모델을 생성
model4 = glm(disease~age+sector, data = disease, family = binomial(logit))
summary(model4)

# > summary(model4)
# 
# Call:
#   glm(formula = disease ~ age + sector, family = binomial(logit), 
#       data = disease)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7296  -0.7048  -0.4940   0.9870   2.0929  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.33515    0.51113  -4.569 4.91e-06 ***
#   age          0.02929    0.01317   2.224 0.026153 *  
#   sector       1.67345    0.48734   3.434 0.000595 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 122.32  on 97  degrees of freedom
# Residual deviance: 102.26  on 95  degrees of freedom
# AIC: 108.26
# 
# Number of Fisher Scoring iterations: 4


#model3과 model4를 구했으니 anova를 이용하여 두 모델을 비교한다. 
#model3은 풀 모델이고 model4은 리듀스 모델이다. 

anova(model3, model4, test = "Chisq")   #독립성 검증 알고리즘 "Chisq"

# Analysis of Deviance Table
# 
# Model 1: disease ~ age + status_middle + status_lower + sector
# Model 2: disease ~ age + sector
# Resid. Df Resid. Dev Df Deviance Pr(>Chi)
# 1        93     101.05                     
# 2        95     102.26 -2  -1.2052   0.5474   <-- 값이 0.5474는 독립적이다 라고 판단. 

#독립성인지 아닌지 근거를 제공하기 위한 P값으로 판단.  
# 그러므로 리듀스 모델을 적용한다. 
# 변수 X값이 들어갈 수록 비용증가, Accuracy의 감소로 나타날 수 있으므로.

table(disease$disease)
# > table(disease$disease)
# 
# 0  1 
# 67 31
31/(67+31)
kk4 = table(disease$disease, model4$fitted.values >0.3163265)

# FALSE TRUE
# 0    47   20
# 1     8   23

sum(kk4)
reduceModel = c("민감도"=23/31 ,"특이도"=47/(47+20))

# 풀 모델의 민감도 특이도 확인
kk3 = table(disease$disease, model3$fitted.values >0.3163265)
kk3

# FALSE TRUE
# 0    49   18
# 1     8   23

sum(kk4)
fullModel = c("민감도"=23/31 ,"특이도"=49/(49+18))

reduceModel
fullModel

err_m1 = 28/sum(kk4)
err_m2 = 26/sum(kk3)

install.packages("Deducer")
library("Deducer")
rocplot(model3)
rocplot(model4)

#auc값이 더 큰 model4가 적합함. 






