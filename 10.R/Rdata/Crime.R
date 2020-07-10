setwd("c:/Rdata")

crime = read.csv("http://datasets.flowingdata.com/crimeRatesByState-formatted.csv")
head(crime)

rownames(crime)= crime[,1]
rownames(crime)
stars(crime[,2:8],flip.labels = FALSE, key.loc = c(15,2))

stars(crime[,2:8],flip.labels = FALSE,
      draw.segments=TRUE, key.loc = c(15,2))

#체르노프 페이스
install.packages("aplpack")
library("aplpack")
faces(crime[,2:8])



education = read.csv("http://datasets.flowingdata.com/education.csv")
head(education)

library(lattice)
parallel(education[,2:7])
parallel(education[,2:7],horizontal.axis = FALSE, col = 1)

summary(education$reading)
color = education$reading>523 
color
color+1
parallel(education[,2:7],horizontal.axis = FALSE, col= color+1)
summary(education$dropout_rate)

colorDropout_rate = education$dropout_rate>5.3 
colorDropout_rate+1
parallel(education[,2:7],horizontal.axis = FALSE, col= colorDropout_rate+1)


#주성분 분석
# 본래의 변수들의 변이를 적은 수의 변환된 변수로 설명
# 무수희 많은 선형결합 가운데에서 그 중 가장 높은 설명력을 가지는
# 선형 결합 형태 
# 측정 변수는 줄지 않고 설명요인으로 묶임
# 목적
# 차원축약(Dimension reduction)
# 회귀분석 군집분석 등에서 변수를 제거하기 위한 분석으로 사용
# 자료탐색
# 이상치 판별
# 자료의 그룹화

data = read.csv("20140528_baseball.csv")
head(data)

model = prcomp(data[,2:6],scale = TRUE)
model
summary(model)
# > summary(model)
# Importance of components:
#                         PC1    PC2    PC3     PC4
# Standard deviation     1.7460 1.0357 0.7146 0.58385
# Proportion of Variance 0.6097 0.2145 0.1021 0.06818
# Cumulative Proportion  0.6097 0.8242 0.9264 0.99453
# PC5
# Standard deviation     0.16545
# Proportion of Variance 0.00547
# Cumulative Proportion  1.00000

# Cumulative Proportion 설명력
# 2차원이므로 PC1~2까지 만 적용.
# 즉 0.8242퍼센트만큼 설명력을 가짐.

plot(model)
head(data)
# 팀  승률  타율 장타율 출루율 평균자책
# 1  KIA 0.455 0.285  0.424  0.345     5.40
# 2   LG 0.372 0.276  0.380  0.358     5.08
# 3   NC 0.578 0.283  0.445  0.360     4.11
# 4   SK 0.444 0.275  0.409  0.347     5.59
# 5 넥센 0.545 0.283  0.471  0.372     5.44
# 6 두산 0.568 0.307  0.468  0.378     5.03

rownames(data) = data[,1]
head(data)

model = prcomp(data[,2:6],scale=TRUE)
biplot(model)
