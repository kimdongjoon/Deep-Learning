install.packages("dplyr")
library(dplyr) # dplyr 패키지 로드


df <- data.frame(sex = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))

df


is.na(df)         

table(is.na(df$sex))    # sex 결측치 빈도 출력

table(is.na(df$score))  # score 결측치 빈도 출력

df %>% filter(is.na(score))   # score가 NA인 데이터만 출력

df_nomiss <- df %>% filter(!is.na(score))  # score 결측치 제거
df_nomiss

# score, sex 결측치 제외
df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
df_nomiss  


df_nomiss2 <- na.omit(df)  # 모든 변수에 결측치 없는 데이터 추출
df_nomiss2                 # 출력

mean(df$score, na.rm = T)  # 결측치 제외하고 평균 산출
## [1] 4
sum(df$score, na.rm = T)   # 결측치 제외하고 합계 산출
## [1] 16

mean(df$score, na.rm =T)
sum(df$score, na.rm = T)



setwd("c:/Rdata")
exam <- read.csv("csv_exam.csv")            # 데이터 불러오기
exam

exam[c(3,8,15), "math"] = NA
exam           #임의의 결측 데이터 생성성

#원본 결과 확인
origin = exam %>% summarise(mean_math = mean(math, na.rm = T))  
origin
##57.45

exam %>% summarise(mean_math = mean(math))             # 평균 산출
##   mean_math
## 1        NA
exam %>% summarise(mean_math = mean(math, na.rm = T))  # 결측치 제외하고 평균 산출
##   mean_math
## 1  55.23529

exam %>% summarise(mean_math = mean(math, na.rm = T),      # 평균 산출
                   sum_math = sum(math, na.rm = T),        # 합계 산출
                   median_math = median(math, na.rm = T))  # 중앙값 산출
##   mean_math sum_math median_math
## 1  55.23529      939          50

mean(exam$math, na.rm = T)  # 결측치 제외하고 math 평균 산출
## [1] 55.23529

exam$math <- ifelse(is.na(exam$math), 55, exam$math)  # math가 NA면 55로 대체
table(is.na(exam$math))                               # 결측치 빈도표 생성
## 
## FALSE 
##    20
exam


exam = read.csv("csv_exam.csv")
exam[c(3,8,15),"math"] = NA
exam
exam%>%
  summarize(mean_math= round(mean(math, na.rm = T)))

kk = table(is.na(exam$math))
tt = barplot(kk,col= rainbow(2),ylim= c(0,20))
text(tt,kk,label=paste0(kk,"건", pos = 3))

exam

exam %>% filter(id == 3
               |id == 8
               |id == 15)


#요악
exam = read.csv("csv_exam.csv")
exam[c(3,8,15),"math"] = NA
exam

mean(exam$math)
mean(exam$math, na.rm = T)
exam$math= ifelse(is.na(exam$math), 55, exam$math)

mean(exam$math)

# 데이터 노이즈 확인
library(ggplot2)
mpg = ggplot2::mpg
mpg

# 데이터 노이즈 제거
mpg = as.data.frame(ggplot2::mpg)
mpg
View(mpg)
mpg[c(65,124,131,153,212), "hwy"] = NA
table(is.na(mpg$hwy))

df_mpg = mpg%>%
  group_by(drv)%>%
  summarize(mean_hwy = round(mean(hwy,na.rm = T),1))

df_mpg
ggplot(data=df_mpg,aes(x=drv, y=mean_hwy))+geom_col()
ggplot(data = mpg, aes(x=drv))+geom_bar()



# 혼자서 해보기
# 결측치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요.
# ???	Q1. drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.
# ???	Q2. filter()를 이용해 hwy 변수의 결측치를 제외하고, 어떤 구동방식의 hwy 평균이 높은지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.

# 힌트
# Q1. 빈도표를 만드는 table()과 결측치를 확인하는 is.na()를 조합해 보세요.
# Q2. filter()와 is.na()를 조합해 결측치를 제외하고, 집단별 평균을 구하는 코드를 %>%로 연결하면 됩니다.

# Q1.drv(구동방식)별로 hwy(고속도로 연비) 평균이 어떻게 다른지 알아보려고 합니다. 분석을 하기 전에 우선 두 변수에 결측치가 있는지 확인해야 합니다. drv 변수와 hwy 변수에 결측치가 몇 개 있는지 알아보세요.
# table(is.na(mpg$drv))  # drv 결측치 빈도표 출력
# ## 
# ## FALSE 
# ##   234
# table(is.na(mpg$hwy))  # hwy 결측치 빈도표 출력
# ## 
# ## FALSE  TRUE 
# ##   229     5

# mpg %>%
#   filter(!is.na(hwy)) %>%          # 결측치 제외
#   group_by(drv) %>%                # drv별 분리
#   summarise(mean_hwy = mean(hwy))  # hwy 평균 구하기
# ## # A tibble: 3 x 2
# ##     drv mean_hwy
# ##   <chr>    <dbl>
# ## 1     4 19.24242
# ## 2     f 28.20000
# ## 3     r 21.00000


# 이상치 제거하기 - 1. 존재할 수 없는 값
# ???	논리적으로 존재할 수 없으므로 바로 결측 처리 후 분석시 제외
# 이상치 포함된 데이터 생성 - sex 3, score 6
outlier <- data.frame(sex = c(1, 2, 1, 3, 2, 1),
                      score = c(5, 4, 3, 4, 2, 6))
outlier

# 이상치 확인하기
table(outlier$sex)
## 
## 1 2 3 
## 3 2 1
table(outlier$score)
## 
## 2 3 4 5 6 
## 1 1 2 1 1

# 결측 처리하기 - sex
# sex가 3이면 NA 할당
outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
outlier
##   sex score
## 1   1     5
## 2   2     4
## 3   1     3
## 4  NA     4
## 5   2     2
## 6   1     6

# 결측 처리하기 - score
# sex가 1~5 아니면 NA 할당
outlier$score <- ifelse(outlier$score > 5, NA, outlier$score)
outlier
##   sex score
## 1   1     5
## 2   2     4
## 3   1     3
## 4  NA     4
## 5   2     2
## 6   1    NA


#결측치 제외하고 분석
outlier %>%
  filter(!is.na(sex) & !is.na(score)) %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score))
## # A tibble: 2 x 2
##     sex mean_score
##   <dbl>      <dbl>
## 1     1          4
## 2     2          3


mpg = as.data.frame(ggplot2::mpg)
boxplot(mpg$hwy,horizontal = T, col =2)

summary(mpg$hwy)
IQR(mpg$hwy)

hist(mpg$hwy, probability = T)
lines(density(mpg$hwy),  col = 2  , type = 'h')

boxplot(mpg$hwy)$stats

mpg$hwy= ifelse(mpg$hwy<12 | mpg$hwy> 37 , NA , mpg$hwy)

kk = table(is.na(mpg$hwy))

tt= barplot(kk, col= rainbow(2), ylim = c(0,250))
text(tt,kk,label= paste0(kk,"건"), pos = 3)

mpg%>%
  group_by(drv)%>%
  summarise(mean_hwy = mean(hwy, na.rm = T))



# 혼자서 해보기
# mpg 데이터를 이용해서 분석 문제를 해결해 보세요.
# 우선 mpg 데이터를 불러와서 일부러 이상치를 만들겠습니다. drv(구동방식) 변수의 값은 4(사륜구동), f(전륜구동), r(후륜구동) 세 종류로 되어있습니다. 몇 개의 행에 존재할 수 없는 값 k를 할당하겠습니다. cty(도시 연비) 변수도 몇 개의 행에 극단적으로 크거나 작은 값을 할당하겠습니다.
mpg <- as.data.frame(ggplot2::mpg)                  # mpg 데이터 불러오기
mpg[c(10, 14, 58, 93), "drv"] <- "k"                # drv 이상치 할당
mpg[c(29, 43, 129, 203), "cty"] <- c(3, 4, 39, 42)  # cty 이상치 할당

# 혼자서 해보기
# 이상치가 들어있는 mpg 데이터를 활용해서 문제를 해결해보세요.
# 구동방식별로 도시 연비가 다른지 알아보려고 합니다. 분석을 하려면 우선 두 변수에 이상치가 있는지 확인하려고 합니다.
# ???	Q1. drv에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 다음 이상치가 사라졌는지 확인하세요. 결측 처리 할 때는 %in% 기호를 활용하세요.
# ???	Q2. 상자 그림을 이용해서 cty에 이상치가 있는지 확인하세요. 상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.
# ???	Q3. 두 변수의 이상치를 결측처리 했으니 이제 분석할 차례입니다. 이상치를 제외한 다음 drv별로 cty 평균이 어떻게 다른지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.

# 힌트
# Q1. drv가 정상적인 값이면 원래 값을 유지하고 그렇지 않으면 NA를 부여하는 코드를 작성하면 됩니다. 정상적인 값이 여러 개 있으니 %in%와 c()를 조합해 코드를 간결하게 만들어 보세요.
# Q2. 상자 그림을 만들 때 사용하는 다섯 가지 통계치를 출력해 정상 범위의 기준을 찾으세요. 그런 다음 filter()를 이용해 cty가 이 범위를 벗어날 경우 NA를 부여하면 이상치가 결측 처리 됩니다.
# Q3. filter()를 이용해 drv와 cty가 모두 결측치가 아닌 데이터를 추출한 후 집단별 평균을 구하면 됩니다.

#Q1. drv에 이상치가 있는지 확인하세요. 이상치를 결측 처리한 다음 이상치가 사라졌는지 확인하세요. 결측 처리 할 때는 %in% 기호를 활용하세요.
# 이상치 확인
table(mpg$drv)
## 
##   4   f   k   r 
## 100 106   4  24
# drv가 4, f, r이면 기존 값 유지, 그 외 NA할당
mpg$drv <- ifelse(mpg$drv %in% c("4", "f", "r"), mpg$drv, NA)

# 이상치 확인
table(mpg$drv)
## 
##   4   f   r 
## 100 106  24


#Q2. 상자 그림을 이용해서 cty에 이상치가 있는지 확인하세요. 상자 그림의 통계치를 이용해 정상 범위를 벗어난 값을 결측 처리한 후 다시 상자 그림을 만들어 이상치가 사라졌는지 확인하세요.
# 상자 그림 생성 및 통계치 산출
boxplot(mpg$cty)$stats
##      [,1]
## [1,]    9
## [2,]   14
## [3,]   17
## [4,]   19
## [5,]   26
# 9~26 벗어나면 NA 할당
mpg$cty <- ifelse(mpg$cty < 9 | mpg$cty > 26, NA, mpg$cty)

# 상자 그림 생성
boxplot(mpg$cty)

# Q3. 두 변수의 이상치를 결측처리 했으니 이제 분석할 차례입니다. 이상치를 제외한 다음 drv별로 cty 평균이 어떻게 다른지 알아보세요. 하나의 dplyr 구문으로 만들어야 합니다.
mpg %>%
  filter(!is.na(drv) & !is.na(cty)) %>%  # 결측치 제외
  group_by(drv) %>%                      # drv별 분리
  summarise(mean_hwy = mean(cty))        # cty 평균 구하기
## # A tibble: 3 x 2
##     drv mean_hwy
##   <chr>    <dbl>
## 1     4 14.24742
## 2     f 19.47000
## 3     r 13.95833



# 정리하기
# # 1.결측치 정제하기
# 
# # 결측치 확인
# table(is.na(df$score))
# 
# # 결측치 제거
# df_nomiss <- df %>% filter(!is.na(score))
# 
# # 여러 변수 동시에 결측치 제거
# df_nomiss <- df %>% filter(!is.na(score) & !is.na(sex))
# 
# # 함수의 결측치 제외 기능 이용하기
# mean(df$score, na.rm = T)
# exam %>% summarise(mean_math = mean(math, na.rm = T))

# 2.이상치 정제하기

# # 이상치 확인
# table(outlier$sex)
# 
# # 결측 처리
# outlier$sex <- ifelse(outlier$sex == 3, NA, outlier$sex)
# 
# # boxplot으로 극단치 기준 찾기
# boxplot(mpg$hwy)$stats
# 
# # 극단치 결측 처리
# mpg$hwy <- ifelse(mpg$hwy < 12 | mpg$hwy > 37, NA, mpg$hwy)




df_mpg

install.packages("plotly")
library(plotly)



#1. 배경 설정하기
# x축 displ, y축 hwy로 지정해 배경 생성
ggplot(data = mpg, aes(x = displ, y = hwy))

# 2. 그래프 추가하기
# 배경에 산점도 추가
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point()

#3. 축 범위를 조정하는 설정 추가하기
# x축 범위 3~6으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6)


#3. 축 범위를 조정하는 설정 추가하기
# x축 범위 3~6, y축 범위 10~30으로 지정
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point() + 
  xlim(3, 6) + 
  ylim(10, 30)

