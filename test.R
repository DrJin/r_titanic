
#1. 세팅
# [O] 프로젝트 설정 ####
# [O] 경로설정

getwd()

#2. 데이터 입수
# [O] csv 확인 ####
# [] 데이터 불러와 저장하기 ####
raw <- read.csv('titanic.csv',
                sep = ',', #separator
                header = T,#칼럼형 존재 여부
                stringsAsFactors = F) #문자형 데이터 인식 형태

install.packages ('rlang')
install.packages('ggplot2')
install.packages('caret')
install.packages('e1071')
library(ggplot2)
library(caret)
library('e1071')


#[]데이터 확인
#head(raw,2)
#tail(raw,2)
#summary(raw)
#str(raw) ####

colnames(raw)[1] <- 'id'

raw_backup <- raw #backup
#summary(raw)

#변수별 데이터 확인 ####
summary(raw$Survived)
summary(raw$Pclass)
summary(raw$Sex)
summary(raw$Age) # NA : 263
summary(raw$Sibsp)
summary(raw$Parch)
summary(raw$Ticket)
summary(raw$Fare)
summary(raw$cabin)
summary(raw$Embarked)

# - 데이터 유형 변경 ####
raw1 <- raw #백업

str(raw1$Survived)
#as.factor(raw1$Survived) #R에서 범주형 데이터를 나타내는 유형


raw1$Survived <- as.factor(raw1$Survived)
raw1$Pclass <- as.factor(raw1$Pclass)
raw1$Sex <- as.factor(raw1$Sex)
raw1$Embaked <- as.factor(raw1$Embarked)
summary(raw1$Embarked)

# [O] 이상치, 결측치 확인 1 ####
#is.na(raw1)
# [O] 결측치 유형 - Null, NA, NaN, 공백('', '  '), -1, 99

raw1$Cabin == '' #결측치 ''
raw1[raw1$Cabin == '',]
nrow(raw1[raw1$Cabin == '',]) #1014

raw1$Embarked == '' #결측치 ''
raw1[raw1$Embarked == '',]
nrow(raw1[raw1$Embarked == '',]) #2

summary(raw1$Age)

mean(raw1$Age) #평균 -> NA
mean(raw1$Age, na.rm = T) # 결측치 값은 지우고 평균
age_mean <- mean(raw1$Age, na.rm = T)

raw1[which(is.na(raw1$Age)),]$Age <- age_mean

summary(raw1$Age)
mean(raw1$Age) #평균 -> 29.88114


raw2 <- raw1

raw2$SibSp
raw2$Parch

raw2$Family <- raw2$SibSp + raw2$Parch
summary(raw2)

# 모델링 ####
ggplot(raw2, aes(x=Family)) + geom_histogram()
ggplot(raw2, aes(x=Family, fill= Survived)) + geom_histogram(binwidth = 1)
ggplot(raw2, aes(x=Family, fill= Survived)) + geom_histogram(binwidth = 1, position = 'fill')


# ####

dataset <- raw # 백업2
dataset <- raw2[,c('Survived','Pclass','Sex','Age','Family','Fare','Embarked') ]
summary(dataset)
# [] 데이터셋 분할 ####
idx <- seq(1:1309)
train_idx <- sample(idx, #샘플링 대산 데이터
                    size = length(idx)*0.7, #70% 학습
                    replace = F) #비복원추출
test_idx <- idx[-train_idx]

dataset_train <- dataset[train_idx, ]
dataset_test <- dataset[test_idx, ]

# 정규화 ####
scale <- caret::preProcess(dataset_train, method = c('center','scale'))

dataset_train_scale <- predict(scale, dataset_train) # 정규화된 훈련용 데이터
dataset_test_scale <- predict(scale, dataset_test) # 정규화된 검증용 데이터

str(dataset_train_scale)


# 모델 학습
svm(Survived ~ Pclass + Sex + Age + Famil + Fare + Embarked,
    data = dataset_train_scale)
