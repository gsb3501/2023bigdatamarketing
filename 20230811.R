## 회귀분석 ~ 빅데이터 분석 

# wd 지정
getwd()
setwd("/Users/hsy/Desktop/bigdata marketing/R_exercise")
getwd()

## data 

df <- read.csv("Ashopping.csv")
str(df)

## 평균검정 (t-test, ANOVA)
df_claim_0 <-df[df$클레임접수여부 ==0, ]
df_claim_1 <-df[df$클레임접수여부 ==1, ]
str(df_claim_0)
str(df_claim_1)

# 평균검정 전, 정규성 검정
# 귀무 : 정규분포이다 
shapiro.test(df_claim_0$총매출액)
shapiro.test(df_claim_1$총매출액)
# 결과 : 정규분포가 아님 -> t-test 불가

# wilcoxon
# 귀무 : 클레임 여부에 따른 총매출액의 평균이 같다 
wilcox.test(df_claim_0$총매출액, df_claim_1$총매출액)
wilcox.test(df_claim_0$총매출액, df_claim_1$총매출액, paired = F) 
# paired = T : 윌콕슨, paired = F : 맨휘트니 
median(df_claim_0$총매출액)
median(df_claim_1$총매출액)

help("wilcox.test")
# 결과 : 클레임 여부에 따라 총매출액 평균 차이가 있다 

# anova (3집단 이상)
colnames(df)
anova <- aov(총매출액 ~ 구매금액대, data = df)
summary(anova)

# 과제 : Kruskal-wallis 검정: 구매금액대에 따른 총매출액 
str(df$구매금액대)
df_purchase_range_0 <-df[df$구매금액대 ==0, ]
df_purchase_range_1 <-df[df$구매금액대 ==1, ]
df_purchase_range_2 <-df[df$구매금액대 ==2, ]
str(df_purchase_range_0)
str(df_purchase_range_1)
str(df_purchase_range_2)

shapiro.test(df_purchase_range_0$총매출액)
shapiro.test(df_purchase_range_1$총매출액)
shapiro.test(df_purchase_range_2$총매출액)
# 결과 : 세 집단 모두 정규분포가 아님 

help("kruskal.test")
kruskal.test(총매출액 ~ 구매금액대, data = df)

# 상관관계 
# 서열형 integerO , 연속형 numeric O , 
# 집단형=범주형 factor X!!
str(df)
cor(df$할인권.사용.횟수, df$총.할인.금액)
plot(df$할인권.사용.횟수, df$총.할인.금액)
abline(lm(df$총.할인.금액 ~ df$할인권.사용.횟수), col = "blue")
# 결과 : 할인권 사용횟수와 총 할인금액의 상관관계가 아주 높다. (0.8) 

# 회귀분석 
reg <- lm(총매출액 ~ 할인권.사용.횟수 + 총.할인.금액, data = df)
summary(reg)

# 할인권 사용횟수와 총 할인금액 사이의 상관관계가 0.8이기 때문에 
# 회귀분석 시, 두 독립변수 중 하나를 택 1 하여 진행해야함 
reg_1 <- lm(총매출액 ~ 총.할인.금액, data = df)
summary(reg_1)


# 로지스틱 회귀 
str(df)
df$이탈여부 <- as.factor(df$이탈여부)
str(df$이탈여부)

logit <- glm(이탈여부 ~ ., data = df, family = "binomial") 
# 독립변수를 .으로 넣으면 df 의 모든 변수를 독립으로 넣으라는 뜻 
summary(logit)

#ifelse(조건, true, false)
# df$이탈여부 <- ifelse(df$이탈여부 ==1, 0, 1) 
# df 이탈여부가 1이면 0, 아니면 1로 코딩하라는 뜻 

# 변수 변환
str(df)

# cg라는 변수에 아래 column 들을 combine 
cg <- c("이탈여부", "구매금액대", "고객등급", "구매유형", "클레임접수여부", 
        "구매카테고리수", "거주지역", "성별", "고객.나이대", "할인민감여부")

df[ ,cg] <- lapply(df[,cg], factor) #df[,cg]를 모두 factor 로 바꿔서 저장해라 
help(lapply)
str(df)

# [,] <- 인덱스 [행, 열]
df[1,1]

df
# 함수식 지정 {}
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
  return(x)
  }
minmax
# () <- 매개변수 입력

# numeric 변수로 지정
colnames(df)
num <- c("총매출액", "X1회.평균매출액" , "총.할인금액")
#df[,num] <- lapply(df[,num], as.numeric)
str(df)


install.packages("caret", type = 'binary', dependencies = T)
library(caret)
??caret

#결측값
colSums(is.na(df))
#이상치 확인
boxplot(df)


# 머신러닝, 빅데이터분석 (train -> f(x), test로 평가)

library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .70, list = FALSE) # 랜덤 샘플링 인덱스 지정 
training <- Sonar[ inTraining,]  #train set 
testing  <- Sonar[-inTraining,]  #test set 

# 교차검증 (범주형 데이터 - 범주형 데이터)
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)   

set.seed(825)
gbmFit1 <- train(Class ~ ., data = training, # 종속변수 = Class, 독립변수 = 다른 모든 변수 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1    #gbm 으로 구한 f(x)

pred_gbm <- predict(gbmFit1, newdata = testing) #function 에 test x 를 넣어서, y hat 이 구해졌음
str(testing$Class) # testing$Class = test x에 대한 실제 y 값 
str(pred_gbm)
pred_gbm <- as.data.frame(pred_gbm)
str(testing)
str(pred_gbm)
predic_gbm_y <- cbind(pred_gbm, testing$Class) # yhat 과 y의 값을 cbind 해서 묶어줌 
str(predic_gbm_y)
View(predic_gbm_y)

caret::confusionMatrix(predic_gbm_y$pred_gbm, 
                       predic_gbm_y$`testing$Class`, 
                       mode = "everything"
) # f1 score 확인하기

??caret::confusionMatrix
caret::varImp(gbmFit1)


## nnet
nnet <- train(Class ~ ., data = training, 
                 method = "mlp", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for mlp() that passes through
                 verbose = FALSE)
nnet    #nnet 으로 구한 f(x)

pred_nn <- predict(nnet, newdata = testing) #function 에 test x 를 넣어서, y hat 이 구해졌음
str(testing$Class) # testing$Class = test x에 대한 실제 y 값 
str(pred_nn)
pred_nn <- as.data.frame(pred_nn)
str(testing)
predic_nn_y <- cbind(pred_nn, testing$Class) # yhat 과 y의 값을 cbind 해서 묶어줌 
str(predic_nn_y)
View(predic_nn_y)

caret::confusionMatrix(predic_nn_y$pred_nn, 
                       predic_nn_y$`testing$Class`, 
                       mode = "everything"
) # f1 score 확인하기

caret::varImp(nnet)

# 과제 : Ashopping 파일로 위 예제 연습해보기 !!!!


## boosting 

df_exit_0 <-df[df$이탈여부 ==0, ]
df_exit_1 <-df[df$이탈여부 ==1, ]
str(df_claim_0)
str(df_claim_1)

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)  

bagging <- train(이탈여부 ~ ., data = df, 
              method = "AdaBag", 
              trControl = fitControl,
              verbose = FALSE)

boosting    #boosting  으로 구한 f(x)

pred_nn <- predict(nnet, newdata = testing) #function 에 test x 를 넣어서, y hat 이 구해졌음
str(testing$Class) # testing$Class = test x에 대한 실제 y 값 
str(pred_nn)
pred_nn <- as.data.frame(pred_nn)
str(testing)
predic_nn_y <- cbind(pred_nn, testing$Class) # yhat 과 y의 값을 cbind 해서 묶어줌 
str(predic_nn_y)
View(predic_nn_y)

caret::confusionMatrix(predic_nn_y$pred_nn, 
                       predic_nn_y$`testing$Class`, 
                       mode = "everything"
) # f1 score 확인하기

