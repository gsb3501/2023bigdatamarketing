
## 통계 실무 ##
getwd()
setwd("/Users/hsy/Desktop/bigdata marketing/R_exercise")
getwd()
# 여기서 워킹 디렉토리를 설정해줘야, 아래에서 csv 파일을 불러올 때 모든 주소 안 써도 된다. 


#df <- read.csv("Ashopping.csv", header = T, fileEncoding ="euc-kr")
# 또 다른 방법으로는, csv 파일을 utf-8로 저장하고, 인코딩은 utf-8로 설정 
df <- read.csv("Ashopping.csv", encoding = 'utf-8')

str(df)
# 행 = 관측값 (obs.), 열 = 변수 (variables)
# 변수명, 변수형태, 관측값 순으로 조회됨 

summary(df) 
library(psych)
describe(df)

#1번 문제 풀이
mean(df$총매출액) #평균
var(df$총매출액) #분산
sd(df$총매출액) #표준편차

#2번 문제 풀이
head(df$이탈여부, 30)
df_exit_1 <- df[df$이탈여부 ==1,] 
#df안에서, df$이탈여부라는 관측값이 1인 데이터를 df_exit_1이라는 변수에 저장
df_exit_0 <- df[df$이탈여부 ==0,]
str(df_exit_1)
str(df_exit_0)
# 1회 평균매출액에 따라 이탈여부가 결정된다. 이탈한 평균매출액이 안한 평균매출액보다 큰거 보니

t.test(df_exit_1$X1회.평균매출액, df_exit_0$X1회.평균매출액)
# 귀무 = 이탈여부 0인 집단, 1인 집단의 1회 평균매출액이 같다
# p-value = 3.364e-11 로, 0.05보다 작아서 귀무가설 기각 -> 두 집단간의 1회평균매출액이 다름

mean(df_exit_0$X1회.평균매출액)
mean(df_exit_1$X1회.평균매출액)

# 이탈여부에 따라 1회 평균매출액이 달랐다. 결과를 오바해서 말하자면...1회 평균매출액이 클 수록 이탈률이 높았다. 
# 즉, 한번에 많이 산 (매출액이 큰) 집단이 바로 이탈하였다. 

# paired t-test (동일한 집단 내에서 전후를 비교하고자 할 때)
df_exit_0_b <- df_exit_0[1:150, ]
df_exit_0_a <- df_exit_0[151:300, ]
t.test(df_exit_0_b$X1회.평균매출액, df_exit_0_a$X1회.평균매출액, paired = T)

# 3번 문제 풀이 ANOVA
# 귀무 : 구매유형에 따라 평균 구매주기가 같다. 
summary(df$구매유형)
help("aov")
str(df)
colnames(df) #df 의 열 가져오는 함수
anova <- aov(평균.구매주기 ~ 구매유형, data = df) # aov(종속변수 ~ 독립변수, 데이터)
summary(anova)
# anova 결과, p값이 0.0637로, 0.05보다 커서 귀무가설을 채택 -> 구매유형에 따라 평균 구매주기 차이가 없다 (즉, 같다)


# 4번 문제 풀이 chisq-test : 집단간의 연관성 비교
# 귀무 : 성별, 할인민감여부 관계가 없다
chisq.test(df$성별, df$할인민감여부)
df_gender_0 <- df[df$성별 == 0, ]
df_gender_1 <- df[df$성별 == 1, ]
str(df_gender_0)
str(df_gender_1)
mean(df_gender_0$할인민감여부)
mean(df_gender_1$할인민감여부)
# 할인민감여부에 민감한 집단(성별)에 할인 쿠폰을 더 줘야한다는 결론을 내릴 수 있음 




# 5번 문제풀이 correlation (공분산 활용) -> 연속형, 순위형 변수간의 관계를 확인
help(cor)
cor(df$방문빈도, df$총매출액, method = 'pearson') 
# 0.4 정도이기 때문에 양의 상관관계인데, 강한 상관관계는 아님 
# 0.7 기준으로 상관관계가 높다고 판단 
plot(df$방문빈도, df$총매출액)
abline(lm(df$총매출액~df$방문빈도), col ='blue')
reg <- lm(총매출액 ~ 방문빈도, data = df)
#reg <- lm(df$총매출액 ~ df$방문빈도)
summary(reg)

help(abline)
help(lm)

# 6번 문제풀이 - 단순회귀분석 (x가 하나)
colnames(df)
reg_1 <-lm(총매출액~총.할인.금액, data = df)
summary(reg_1)
mean(df$총매출액)

# residuals = 잔차 = 오차 = 에러
# Coefficients : 베타, Estimate 

#회귀식을 만들 수 있으려면, 
# 1) 가장 밑의 F-statistic / p-value 확인 
# F값은 0에서 1사이, 0에 가까울수록 평균에 가깝다. 즉, 회귀식이 평균과 거의 똑같다. 
# p-value 가 0.05보다 작아야함 -> 평균보다 회귀식이 더 잘 설명한다는 의미
# 즉, 회귀식을 구하는 의미가 없다 
# 2) 위에 있는 Coefficients 값들의 p-value 가 0.05보다 작아야함.
# 3) R-squared 값 확인 = 평균보다 얼마나 더 잘 설명하는 가 
## 0.5이상이면 유의미하다, 잘 설명함 


# 7번 문제 풀이 - 다중회귀분석 
# 다중이기 때문에 lm 전 우선 독립변수들끼리 상관성 분석
# 연속형 변수 / 순위형 변수끼리는 회귀분석하기 전에 먼저 상관분석을 시행해야함 

df_cor <-cbind(df$방문빈도, df$평균.구매주기, df$할인권.사용.횟수)
cor(df_cor)

# 회귀분석 
reg_2 <-lm(총매출액~방문빈도
                    +평균.구매주기
                    +할인권.사용.횟수
                    , data = df)
summary(reg_2)

## 8번 - 로지스틱 회귀
logit <- glm(이탈여부 ~ 방문빈도
            +평균.구매주기
            , data = df)
summary(logit)


## 9번 문제 풀이 - 다중 로지스틱 회귀분석
# y가 0 또는 1의 값을 가짐 
help(glm)
colnames(df)

logit <- glm(이탈여부~ 방문빈도                   
             +평균.구매주기
             +할인권.사용.횟수
             , data = df
             , family = "binomial") # binomial = 0 또는 1
summary(logit)

#바로 Coefficients 값 확인

df_고객ID_거주지역4 <- df[df$거주지역 == 4, 1]
df_고객ID_거주지역4 <- as.data.frame(df_고객ID_거주지역4)
df_거주지역 <- df[df$거주지역 == 4, ]
