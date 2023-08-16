df <- airquality # airquality 라는 내장 데이터
View(df)
str(df)

is.na(df)             #결측이 있으면 TRUE 반환
sum(is.na(df$Ozone)) #Ozone에 결측값 이 얼마나 있는 지 더해라 
sum(is.na(df))
colSums(is.na(df)) #column 별로, 결측값이 몇 개있는 지 세서 제시해라

df_naomit <- na.omit(df) #df에서 결측값을 없앤 것을 새로운 변수 df_namoit에 저장해
colSums(is.na(df_naomit))

# 이상치
boxplot(iris$Sepal.Length)

# scaler 
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
iris$Sepal.Length_scale <- scale(iris$Sepal.Length)
mean(iris$Sepal.Length_scale)
sd(iris$Sepal.Length_scale)


# cbind, rbind 
length <- cbind(iris$Sepal.Length, iris$Petal.Length)
length



iris_25 <- iris[1:25, ]
str(iris_25)
iris_50 <- iris[26:50, ]
str(iris_50)
iris_25_50 <- rbind(iris_25, iris_50)
str(iris_25_50)

