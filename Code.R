getwd()
train <- read.csv("/home/ashok/Desktop/titanic/train .csv")
test <- read.csv("/home/ashok/Desktop/titanic/test .csv")
PassengerId=test$PassengerId

summary(train)
summary(test)
hist(train$Age)

train$Age[is.na(train$Age)] = 29.07
test$Age[is.na(test$Age)] = 29.07
test$Fare[is.na(test$Fare)] = 35.627

summary(train)
summary(test)

train$Sex = ifelse(train$Sex=="female", 1, 0)
test$Sex = ifelse(test$Sex=="female", 1, 0)

train$embarked_c = ifelse(train$Embarked=="C", 1, 0)
test$embarked_c = ifelse(test$Embarked=="C", 1, 0)

train$embarked_s = ifelse(train$Embarked=="S", 1, 0)
test$embarked_s = ifelse(test$Embarked=="S", 1, 0)

head(train)
train = train[-c(1, 4, 9,11,12)]

head(test)
test = test[-c(3, 8,10,11)]

boxplot(train$Age)
train$Age = ifelse(train$Age>=52, 52, train$Age)
train$Age = ifelse(train$Age<=4, 4, train$Age)
test$Age = ifelse(test$Age>=52, 52, test$Age)
test$Age = ifelse(test$Age<=4, 4, test$Age)

boxplot(test$Age)

boxplot(train$Fare)
train$Fare = ifelse(train$Fare>=136, 136, train$Fare)
test$Fare = ifelse(test$Fare>=136, 136, test$Fare)



library(car)
model <- lm(Survived~., data=train)
t = vif(model)
sort(t, decreasing=TRUE)
model1<- glm(as.factor(Survived)~., family="binomial", data=train)
summary(model1)

stepmodel = step(model1, direction="both")


test$score <- predict(model1, newdata = test, type="response")
head(test$score)

test$Survived <- ifelse(test$score>=0.56, 1, 0)
head(test)
  
df=test[c(1,11)]
head(df)
write.csv(df,file = "df.csv",row.names = FALSE)
