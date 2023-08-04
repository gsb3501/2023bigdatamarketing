# t-test
data <- iris
str(data)

setosa <-data[data$Species == "setosa",]
versicolor <-data[data$Species == "versicolor",]
#virginica <-data[data$Species == "virginica", ]

str(setosa)
str(versicolor)

help(t.test)

# 모평균이 5라고 가정할 때,
# 귀무: setosa의 sepal.length가 모평균 5와 같다. 

t.test(setosa$Sepal.Length, 
       mu = 5, 
       alternative = "two.sided")

# 귀무 :  setosa 의 sepal length 와 versicolor 의 sepal length 의 평균이 같다. 
t.test(setosa$Sepal.Length, 
       versicolor$Sepal.Length, 
       alternative = "two.sided")

# setosa 대응표본 : 같은 집단 전후 평균 비교 
# t.test(setosaB$Sepal.Length, #paired t-test
#       setosaA$Sepal.Length, 
#       paired = T, 
#       alternative = "two.sided")


# 3 집단 이상 평균 비교 : ANOVA
# 귀무 : species 에 따라 sepal length 가 같다. 
anova <- aov(Sepal.Length ~ Species, data = data) # Species 가 독립변수, Sepal.Length 가 종속변수 
summary(anova)

# 사후 분석 : 어떤 집단이 어떻게 차이가 나는 지 확인이 필요
# 귀무 : 집단끼리 sepal.length 의 평균이 같다. 
install.packages("DescTools", binary = T, dependecies = T)
library(DescTools)
help(PostHocTest)
PostHocTest(anova, 
            method = "hsd",
            conf.level = 0.95)
