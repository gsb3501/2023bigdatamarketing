"hello world"

a <- 1  #a 변수에 1값 저장 
a
x1 <- "ubion"
x2 <- 25
x3 <- 1

str(x1)  ## str = structure
str(x2)
str(x3)
x3 <-as.factor(x3)
str(x3)

x4 <- 12.25
str(x4)
x4 <- as.integer(x4)
str(x4)

# chr = characters
# num = numeric
# int = integer 정수 


data <- iris
str(data)

# data.frame : 엑셀 형태의 데이터 (표)
# 150 obs :  150개의 관측치
# 5 variables : 5개의 변수 
# as factor : 어떤 변수를 집단 변수로 정의 
# ex) iris 의 종류 (집단) 3개를 setosa, versicolor, virginica 로 나눔

data$Sepal.Length        ## <- 어떤 데이터 안에 넣은 것을 볼 때 '$'로 확인한다. 

data$Sepal.Length <- as.integer(data$Sepal.Length)
data$Sepal.Length
str(data)
summary(data)
data$Species <-as.factor(data$Species) 
str(data)

#a <- lm(Sepal.Length~., data = data)
#a$


setosa <- data[data$Species == "setosa",]
versicolor <- data[data$Species == "versicolor",]
virginica <- data[data$Species == "virginica",]
str(setosa)
str(versicolor)
str(virginica)

mean(setosa$Sepal.Length)
mean(versicolor$Sepal.Length)
mean(virginica$Sepal.Length)

summary(iris)

data <- iris
data2 <- data

getwd()
setwd('/Users/hsy/Documents/디지털 마케팅 과제')
write.csv(iris,'iris.csv')
setwd("/Users/hsy/Desktop/bigdata marketing/R_exercise")
write.csv(iris,'iris.csv')
getwd()

# getwd() : 현재 작업 디렉토리 확인 
# setwd("폴더경로") : 작업 디렉토리 설정 

install.packages("psych", binary = T, dependencies = T)
library(psych)
describe(data)  
# describe : psych 패키지의 함수로, summary 보다 더 자세하게 데이터를 보여줌 

describe(setosa)
describe(versicolor)
describe(virginica)
plot(data$Sepal.Length)
plot(data$Petal.Length)


help(iris)

x4 <- as.factor(x4)
