# 1조 

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

#1번
mean(df$고객.나이대) #평균
var(df$고객.나이대) #분산
sd(df$고객.나이대) #표준편차

#2번 '성별' 에 따른 '총매출액' 차이가 있는 지 확인

df_sex_1 <- df[df$성별 ==1,] 
df_sex_0 <- df[df$성별 ==0,]
str(df_sex_1)
str(df_sex_0)

t.test(df_sex_1$총매출액, df_sex_0$총매출액)
# 귀무 : 성별이 0인 집단과 1인 집단의 총매출액 평균이 같다 
# p value 가 0.05보다 큰 0.4이기 때문에 귀무가설을 채택한다. 
# 즉, 여자이든, 남자이든 총매출액 평균에는 큰 차이가 없다 (?)

# 3번 고객 나이대에 따른 총매출액의 차이가 있는 지 ANOVA 사용하여 검정
# 귀무 : 고객 나이대에 따라 평균 총매출액이 같다 
colnames(df)
anova <- aov(총매출액 ~ 고객.나이대, data = df)
summary(anova)

# p 값이 0.05보다 작기 때문에 귀무가설을 기각한다.
# 즉, 고객 나이대에 따라 평균 총매출액이 다르다 

# 4번 카이제곱 검정, '이탈여부'와 '고객등급' 간의 관계가 있는 지 확인
# 귀무 : 이탈여부와 고객등급 간에 연관성이 없다 
chisq.test(df$고객등급, df$이탈여부)
# p 값이 0.05보다 크기 때문에 귀무가설을 채택한다.  
# 즉, 고객등급과 이탈여부 간에 연관성이 없다. 

# 5번 상관관계 분석, '구매주기'와 '나이대' 사이의 상관관계 
cor(df$평균.구매주기, df$고객.나이대, method = "spearman")
plot(df$평균.구매주기, df$고객.나이대)
abline(lm(df$평균.구매주기~df$고객.나이대), col = 'blue')
#plot(df$고객.나이대, df$평균.구매주기)
#abline(lm(df$고객.나이대~df$평균.구매주기), col = 'blue')

# 상관관계 분석, '방문빈도'와 '평균구매주기'의 상관관계 
cor(df$방문빈도, df$평균.구매주기, method = 'pearson')
plot(df$방문빈도, df$평균.구매주기)
abline(lm(df$평균.구매주기~df$방문빈도), col ='blue')
reg <- lm(평균.구매주기 ~ 방문빈도, data = df)
summary(reg)

# 방문빈도와 평균구매주기는 둘의 상관관계가 아주 강하지 않으나 음의 상관관계를 가진다.
# 많이 방문할수록, 구매주기가 짧다.

# 6번 다중회귀 : 
# 고객등급, 1회 평균매출액, 방문빈도를 독립변수로, 
# 총매출액을 종속변수로 하는 다중 회귀분석

df_cor <- cbind(df$고객등급, df$X1회.평균매출액, df$방문빈도) #column bind 
cor(df_cor)
reg <- lm (총매출액~ 고객등급
           +X1회.평균매출액
           +방문빈도, data = df)

summary(reg)
# y = -4.636e+06 + 1.971e+06x1 + 8.910e+00x2 + 1.881e+05x3 + error
# x1 = 고객등급, x2 = 1회 평균매출액, x3 = 방문빈도 
# r squared 는 0.4975 -> 약 0.5만큼 잘 설명하고 있다. 꽤나 유의미하다. 
# 결론 : 고객등급, 1회 평균매출액, 방문빈도는 총매출액에 영향을 주는 것으로 보인다.


# 7번 : 다중 로지스틱 회귀분석
# '클레임 접수 여부', '거래기간', '고객등급'을 사용하여 '이탈여부'를 예측
# 충성 고객을 얼마나 확보할 수 있는 지? 

colnames(df)
logit <- glm(이탈여부~ 클레임접수여부                 
             +거래기간
             +고객등급
             , data = df
             , family = "binomial") 
summary(logit)

# y = 1.676e+00 -3.711e-01x1 -6.316e-04x2
# 결론 : 클레임 접수여부랑와 거래기간은 이탈여부에 통계적으로 관련이 있다. 
# 고객 등급은 이탈여부에 영향을 미치지 않는 것으로 추정된다.
# 이탈율을 줄이기 위해서는 클레임을 줄이고, 단골고객을 유지하기 위한 
# 프로모션을 진행해야한다. 



## 다른 조꺼 확인
mean(df$할인권.사용.횟수)

df$구매금액대 <-as.factor(df$구매금액대)
df$성별 <- as.factor(df$성별)
reg_5 <- lm(할인권.사용.횟수~구매금액대+성별, data = df)
summary(reg_5)

df$할인권.사용.횟수_mean <-ifelse(df$할인권.사용.횟수 >16.027, 1, 0)
str(df$할인권.사용.횟수_mean)
df$할인권.사용.횟수_mean <-as.factor(df$할인권.사용.횟수_mean)

logit_5 <- glm(할인권.사용.횟수_mean~구매금액대+성별, data = df, family = 'binomial')
summary(logit_5)
