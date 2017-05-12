rm(list = ls())
#setwd("D:/Practice/R/AnalyticsVidya/Black_Friday/")
setwd("C:/data/Black_friday-av/")
train = read.csv("train.csv",stringsAsFactors = F)
test = read.csv("test.csv",stringsAsFactors = F)
str(train)
str(test)
summary(train)
summary(test)
library(dummies)
library(dplyr)

table(train$Product_Category_1)
table(train$Product_Category_2)
table(train$Product_Category_3)


train_purchase = train$Purchase
train$Purchase = NULL


full = rbind(train,test)
table(full$Age)
full$Age[full$Age == "0-17"] = 15
full$Age[full$Age == "18-25"] = 21
full$Age[full$Age == "26-35"] = 30
full$Age[full$Age == "36-45"] = 40
full$Age[full$Age == "46-50"] = 48
full$Age[full$Age == "51-55"] = 53
full$Age[full$Age == "55+"] = 60

full$Age = as.integer(full$Age)

full = dummy.data.frame(full,names = c("City_Category"),sep = "-")

table(full$Stay_In_Current_City_Years)

full$Stay_In_Current_City_Years[full$Stay_In_Current_City_Years == "4+"] = 4
full$Stay_In_Current_City_Years = as.integer(full$Stay_In_Current_City_Years)

full$Gender = ifelse(full$Gender == "M",1,0)
table(full$Gender)


train_new = full[1:length(train_purchase),]
k = length(train_purchase) + 1
test_new = full[k:nrow(full),]

train_new$Purchase = train_purchase

#User Count
user_count = as.data.frame(table(train_new$User_ID))
colnames(user_count) = c("User_ID","User_Count")
str(user_count)

train_new = merge(train_new,user_count,by = "User_ID")
str(train_new)
test_new = merge(test_new,user_count,by = "User_ID",all.x=T)
str(test_new)
summary(test_new)

#Product Count
product_count = as.data.frame(table(train_new$Product_ID))
colnames(product_count) = c("Product_ID","Product_Count")
str(product_count)

train_new = merge(train_new,product_count,by = "Product_ID")
str(train_new)
test_new = merge(test_new,product_count,by = "Product_ID",all.x=T)
str(test_new)
test_new$Product_Count[is.na(test_new$Product_Count)] = 0


mean_purchase = as.data.frame(aggregate(train_new$Purchase,list(train_new$Product_ID),mean))
colnames(mean_purchase) = c("Product_ID","Mean_Purchase")

train_new = merge(train_new,mean_purchase,by = "Product_ID")
str(train_new)
test_new = merge(test_new,mean_purchase,by = "Product_ID",all.x = T)
test_new$Mean_Purchase[is.na(test_new$Mean_Purchase)] = mean(train_new$Mean_Purchase)
#
train_new$high = ifelse(train_new$Purchase > train_new$Mean_Purchase,1,0)
user_prop = as.data.frame(aggregate(train_new$high,list(train_new$User_ID),mean))
colnames(user_prop) = c("User_ID","Proportion")
train_new = merge(train_new,user_prop,by = "User_ID")
test_new = merge(test_new,user_prop,by = "User_ID")
#

submit = test_new[c("User_ID","Product_ID")]
train_purchase = train_new$Purchase
train_new$Purchase = NULL
train_new$high = NULL
train_new$Product_ID = NULL
test_new$Product_ID = NULL

library(xgboost)


test_new = subset(test_new,select = c(colnames(train_new)))

model_xg = xgboost(as.matrix(train_new),as.matrix(train_purchase),
                   cv=5,objective="reg:linear",nrounds=500,max.depth=10,eta=0.1,
                  metric="rmse",nthread = -1,colsample_bytree=0.5)

model_xg.pred = predict(model_xg,newdata = as.matrix(test_new))
model_xg.pred
submit$Purchase = model_xg.pred
summary(submit$Purchase)
submit$Purchase[submit$Purchase < 185] = 185
submit$Purchase[submit$Purchase > 23961] = 23961
colnames(submit) = c("User_ID","Product_ID","Purchase")
write.csv(submit,"sub_1.csv",row.names = F)
