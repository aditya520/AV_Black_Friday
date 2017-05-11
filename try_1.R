setwd("D:/Practice/R/AnalyticsVidya/Black_Friday/")
train = read.csv("train.csv")
test = read.csv("test.csv")
str(train)
user_id = test$User_ID
product_id = test$Product_ID
train$User_ID = NULL
test$User_ID = NULL
train$Product_ID = NULL
test$Product_ID = NULL

###Imputing the values before converting to factors
summary(train)
nrow(train[is.na(train$Product_Category_2) & is.na(train$Product_Category_3),])

train$Product_Category_2[is.na(train$Product_Category_2)] <- 0
summary(train$Product_Category_2)

train$Product_Category_3[is.na(train$Product_Category_3)] <- 0
summary(train$Product_Category_3)

summary(test)
nrow(test[is.na(test$Product_Category_2) & !is.na(test$Product_Category_3),])

test$Product_Category_2[is.na(test$Product_Category_2)] <- 0
summary(test$Product_Category_2)

test$Product_Category_3[is.na(test$Product_Category_3)] <- 0
summary(test$Product_Category_3)

summary(test)
summary(train)
##Conversion to factors
train$Marital_Status = as.factor(train$Marital_Status)
train$Occupation = as.factor(train$Occupation)
train$Product_Category_1 = as.factor(train$Product_Category_1)
train$Product_Category_2 = as.factor(train$Product_Category_2)
train$Product_Category_3 = as.factor(train$Product_Category_3)
  #test
test$Marital_Status = as.factor(test$Marital_Status)
test$Occupation = as.factor(test$Occupation)
test$Product_Category_1 = as.factor(test$Product_Category_1)
test$Product_Category_2 = as.factor(test$Product_Category_2)
test$Product_Category_3 = as.factor(test$Product_Category_3)

summary(train)
summary(train$Occupation)

str(train)
str(test)

library(caTools)
split = sample.split(train$Purchase,SplitRatio = 0.6)
tr1 = subset(train,split == T)
te1 = subset(train,split == F)

rmse = function(x,y){
  return(sqrt((sum((x-y)^2)/length(x))))
}

###trying the models
lm.1 = lm(Purchase ~ .,data = tr1)
summary(lm.1)
step(lm.1,direction = "backward")

lm.step.1 = lm(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
                 Marital_Status + Product_Category_1 + Product_Category_2 + 
                 Product_Category_3, data = tr1)
summary(lm.step.1)
plot(lm.step.1)
lm.step.1.pred = predict(lm.step.1,newdata = te1)
head(lm.step.1.pred)
head(te1$Purchase)
rmse(lm.step.1.pred,te1$Purchase)

lm.step.2 = lm(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
                 Marital_Status + Product_Category_1 + Product_Category_2 + 
                 Product_Category_3, data = tr1)
summary(lm.step.2)
lm.step.2.pred = predict(lm.step.2,newdata = te1)
rmse(lm.step.2.pred,te1$Purchase)

##Anova

ano.1 = aov(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
               Marital_Status + Product_Category_1 + Product_Category_2 + 
               Product_Category_3, data = tr1)

summary(ano.1)
ano.1.pred = predict(ano.1,newdata = te1)
rmse(ano.1.pred,te1$Purchase)

lm.final = lm(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
                Marital_Status + Product_Category_1 + Product_Category_2 + 
                Product_Category_3, data = train)
summary(lm.final)
step(lm.final,direction = "backward")

lm.final.pred = predict(lm.final,newdata = test)

final_csv = data.frame(user_id,product_id,lm.final.pred)
head(final_csv)
colnames(final_csv) = c("User_ID","Product_ID","Purchase")
write.csv(final_csv,"sub1.csv",row.names = F)

# cor(train[c("Gender","Age","Occupation","City_Category",
#            "Stay_In_Current_City_Years","Marital_Status",
#             "Product_Category_1","Product_Category_2","Product_Category_3")])
# 
# library(rpart)
# 
# cart1 = rpart(formula = Purchase ~ Gender + Age + Occupation + City_Category + 
#                  Marital_Status + Product_Category_1 + Product_Category_2 + 
#                  Product_Category_3,method = "anova",data = tr1)
# library(rpart.plot)
# library(e1071)
# library(caret)
# prp(cart1)
# cart1.pred = predict(cart1,newdata = te1)
# head(cart1.pred)
# 
# rmse(lm.step.1.pred,te1$Purchase)
# rmse(cart1.pred,te1$Purchase)
# 
# numFolds = trainControl(method = "cv",number = 10)
# cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))
# train(Purchase ~ Gender + Age + Occupation + City_Category + 
#         Marital_Status + Product_Category_1 + Product_Category_2 + 
#         Product_Category_3,data = tr1,method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
# 
# cart1.cv = rpart(Purchase ~ Gender + Age + Occupation + City_Category + 
#                    Marital_Status + Product_Category_1 + Product_Category_2 + 
#                    Product_Category_3,data = tr1,cp = 0.01)
# prp(cart1.cv)
# cart1.cv.pred = predict(cart1.cv,newdata = te1)
# rmse(cart1.cv.pred,te1$Purchase)
