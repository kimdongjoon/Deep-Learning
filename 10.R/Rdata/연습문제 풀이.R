###연습문제 풀이
setwd('c:/Rdata')
data = read.csv("flushot.csv")
head(data)

log_model = glm(flushot~., data = data , family = binomial(logit))
summary(log_model)

# > summary(log_model)
# 
# Call:
#   glm(formula = flushot ~ ., family = binomial(logit), data = data)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.4037  -0.5637  -0.3352  -0.1542   2.9394  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept) -1.17716    2.98242  -0.395  0.69307   
# age          0.07279    0.03038   2.396  0.01658 * 
#   aware       -0.09899    0.03348  -2.957  0.00311 **
#   gender       0.43397    0.52179   0.832  0.40558              #성별의 경우 상관관계가 적으므로 채택하지 않아도 된다. 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 134.94  on 158  degrees of freedom
# Residual deviance: 105.09  on 155  degrees of freedom
# AIC: 113.09
# 
# Number of Fisher Scoring iterations: 6

# Estimate std age = beta 1 
# Estimate std aware = beta 2 
# Estimate std gender = beta 3 

beta1 = exp(0.07279)  
beta2 = exp(-0.09899)
beta3 = exp(0.43397)

beta1
#1.075505 나이가 1살 증가함에 따라 7%의 차이가 있다. 
beta2

beta3


data
log_model2 = glm(flushot~age+aware, data = data, family = binomial(logit))
log_model2

table(data$flushot)

24/(134+24)
                                                    # 0.1518987
tab_01 = table(data$flushot, log_model2$fitted.values>0.1)
tab_015 = table(data$flushot, log_model2$fitted.values>0.15)
tab_02 = table(data$flushot, log_model2$fitted.values>0.2)
c("민감도" = (19/19+5), "특이도" = 95/(95+40), "에러율" = (40+5)/sum(tab_01))
rocplot(log_model2)

tab_01  
tab_015
tab_02

res01 = c(민감도 = tab_01[2,2]/sum(tab_01[2,]), 
          특이도 = tab_01[1,1]/sum(tab_01[1,]),
          에러율 = (tab_01[1,2]+tab_01[2,1])/sum(tab_01)
          )
res01

res015 = c(민감도 = tab_015[2,2]/sum(tab_015[2,]), 
             특이도 = tab_015[1,1]/sum(tab_015[1,]),
             에러율 = (tab_015[1,2]+tab_015[2,1])/sum(tab_015)
)
res015

res02 = c(민감도 = tab_02[2,2]/sum(tab_02[2,]), 
             특이도 = tab_02[1,1]/sum(tab_02[1,]),
             에러율 = (tab_02[1,2]+tab_02[2,1])/sum(tab_02)
)
res02

res01
res015
res02

#에러율이 가장 낮은 res02를 적용.  여기서 에러율이란 fit value가 0.2 인 경우임. 

model4$fitted.values

# jang = function()
#   k = seq(0.01, 0.5,0.01)
# 
#   n = length(k)
#   
#   err_min= vector(length = n)
#   sens = vector(length = n)
#   spec = vector(length = n)
#   
#   for(i in 1:n){
#     tab = table (data$flushot, log_model2$fitted.values > k[i])
#     res = c(민감도 = tab[2,2]/sum(tab[2,]), 
#             특이도 = tab[1,1]/sum(tab[1,]),
#             에러율 = (tab[1,2]+tab_02[2,1])/sum(tab)
#     )        
#     spec[i]= tab[1,1]/sum(tab[1,])
#     print(res)
#   }
#   


