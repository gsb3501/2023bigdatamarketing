getwd()
setwd("/Users/hsy/Desktop/bigdata marketing/R_exercise")
getwd()

df <- read.csv("Ashopping.csv")
str(df)
summary(df)
library(psych) #psych 가져와야 describe() 쓸 수 있음 
describe(df)

colnames(df)
df$총매출액 <- scale(df$총매출액)
# 특정 범위 안에 넣어주는 함수 scale 
summary(df$총매출액)

# cg라는 변수에 아래 column 들을 combine 
cg <- c("이탈여부", "구매금액대", "고객등급", "구매유형", "클레임접수여부", 
        "구매카테고리수", "거주지역", "성별", "고객.나이대", "할인민감여부")
colnames(df[,cg])
df[ ,cg] <- lapply(df[,cg], factor) #df[,cg]를 모두 factor 로 바꿔서 저장 
str(df)

# 결측값 확인 
colSums(is.na(df))

# boxplot (이상치 확인)
boxplot(df)


library(caret)
# 인덱스 생성
idx <- createDataPartition(df$총매출액, p = .70, list = FALSE)
train <- df[ idx,]  #train set 
test  <- df[-idx,]  #test set 
??caret

# 교차검증
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)   

# linear regression model 로 train 
lmFit <- train(
  총매출액 ~ .,
  data = train,
  method = "lm", 
  trControl = fitControl
)

summary(lmFit)
lmFit$modelInfo$label
lmFit$results

lmpred <- predict(lmFit, newdata = test)
lmpred
str(lmpred)
cor(lmpred, test$총매출액)   # y^, y의 상관관계 확인 
varImp(lmFit) # x의 중요도 확인 


# 만약, 실제값과 예측값을 한눈에 확인하고 싶다면 
lmpred <- as.data.frame(lmpred)
pred_y <- cbind(lmpred$lmpred, test$총매출액)
pred_y

# AIC 확인하는 함수 step()
step(lm(총매출액~., data = df), direction = 'forward')

# Neural Network 로 train 
Nnet <- train(
  총매출액 ~ .,
  data = train,
  method = "brnn", 
  trControl = fitControl
)

summary(Nnet)
Nnet$results

Nnetpred <- predict(Nnet, newdata = test)
Nnetpred
str(Nnetpred)
cor(Nnetpred, test$총매출액)   # y^, y의 상관관계 확인 
varImp(Nnet) # x의 중요도 확인 

#view : 한번에 표로 띄워서 보여줌
#summary : 요약 통계량. 중요 부분을 요약해서 보여줌
#describe : 요약 통계량. summary 보다 조금 더 자세 
#step () : 모델이 여러 개 뽑힘. 이 중 aic 값이 가장 작은 모델이 best


